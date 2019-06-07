(** Copyright (c) 1993-2000 Excelsior, LLC., Russia. All Rights Reserved. *)
(** o2/m2 development system: main module (standalone utility) *)
<* IF ~ DEFINED(COMPONENT_TESTCOVERAGE) THEN *> <* NEW COMPONENT_TESTCOVERAGE- *> <* END *>
<*+ MAIN *>
MODULE xm; (* Ned 03-Mar-94. *)

(* Modifications:
   25-Mar-96 Ned  v2.14.
   16-May-96 Ned  v2.15.
   23-May-96 Ned  v2.16.
*)

IMPORT
(* abstract interface: *)
  xfs:=xiFiles,
  env:=xiEnv,
  pcK,

(* interface managers: *)
  xmConfig,
  xmFS,
  <* IF DEFINED(USE_CFM) AND USE_CFM THEN *> xmFM:=xmCFM,
  <* ELSE *>              xmFM:=xmISOFM,
  <* END *>
  xmArgs,
  xmErrors,
  xmPlatform,
  <* IF XDSIDE THEN *> xmShell, <* END *>

(* compiler managers: *)

  <* IF TARGET_386   THEN *> opCode,    <* END *>
  <* IF TARGET_RISC  THEN *> opCode,    <* END *>
  <* IF TARGET_SPARC THEN *> opCode,    <* END *>
  <* IF TARGET_68K   THEN *> opCode,    <* END *>
  <* IF TARGET_VAX   THEN *> opCode,    <* END *>
  <* IF TARGET_C     THEN *> ccCode,    <* END *>
  <* IF TARGET_NULL  THEN *> beNull,    <* END *>

   pcM2,

(*
  <* IF TARGET_86   THEN *> inMain,    <* END *>
  <* IF TARGET_STAT THEN *> slAnalys,  <* END *>
  <* IF TARGET_O2   THEN *> ocCode,    <* END *>
*)

  pcNum,
  pcConst,

(*----------*)
  XMM:=xmRTS,
  mk:=xcMake,
  xcMain,
  xcF,
  oberonRTS,
  platform,
  FormOut,
  TimeConv,
  DStrings,
  Printf,
  COMPILER;
IMPORT SYSTEM;
IMPORT xmRTS;

<* IF TARGET_IDB THEN *>
IMPORT model2;
<* END *>

<* IF DEFINED(COMPONENT_TESTCOVERAGE) AND COMPONENT_TESTCOVERAGE THEN *>
IMPORT tcMain,  tcConfig;
<* END *>

TYPE
  String = xfs.String;
  Node = POINTER TO NodeDesc;
  NodeDesc = RECORD
    name: String;
    next: Node;
    opt : BOOLEAN;
    val : String;
  END;
  ShowOpts = RECORD (env.Options)
    node: Node
  END;
  ShowEqus = RECORD (env.Equations)
    node: Node
  END;

CONST
  vers = "v2.51";
  header =
    "O2/M2 development system " + vers
    <* IF ts_compatible THEN *> + " TS " <* END *>
    + " (c) 1991-2010 Excelsior, LLC."
  ;
  mode_tag = '=';

  (* operation mode *)
  nothing = 0;
  pro     = 1;
  make    = 2;
  comp    = 3;
  sym2def = 4;
  gen     = 5;

  (* error codes in HALT() *)
  WARNING = 1;
  ERROR   = 2;
  FAULT   = 3;
  OTHER   = 4;

VAR
  job      : INTEGER;
  batch    : BOOLEAN;  (* prepare batch file *)
  all      : BOOLEAN;  (* suppress time checks *)
  help     : BOOLEAN;
  options  : BOOLEAN;  (* show options *)
  equations: BOOLEAN;  (* show equations *)

(*----------------------------------------------------------------*)

PROCEDURE Redirection;
  VAR fn: xfs.String; f: xfs.File; err: BOOLEAN;
BEGIN
  xfs.sys.SysLookup('red',fn);
  xfs.text.Open(fn^,FALSE);
  f:=xfs.text.file;
  IF f#NIL THEN
    WITH f: xfs.TextFile DO
      err:=xcMain.ReadRedirection(f);
      f.Close;
      IF err THEN HALT(OTHER) END;
    END;
  END;
END Redirection;

PROCEDURE Config;
  VAR fn: xfs.String; f: xfs.File; err: BOOLEAN;
      xdsdir: env.String;
BEGIN
  xfs.sys.SysLookup('cfg',fn);
  xfs.text.Open(fn^,FALSE);
  f:=xfs.text.file;
  IF f=NIL THEN
    env.info.print("%s\nConfiguration file is not opened: %s\n"
                  ,header,xfs.text.msg^);
    HALT(OTHER);
  END;
  WITH f: xfs.TextFile DO
    err:=xcMain.ReadConfig(f);
    f.Close;
    IF err THEN HALT(OTHER) END;
  END;
  env.config.Equation("XDSDIR", xdsdir);
  IF (xdsdir # NIL) & (xdsdir^ # "") THEN
    xfs.sys.ConvertToTarget(xdsdir^, xdsdir);
    env.config.SetEquation("XDSDIR", xdsdir^);
  END;
END Config;

(*----------------------------------------------------------------*)
PROCEDURE SetCompilerTitle;
VAR t: TimeConv.DateTime;
    str: ARRAY 128 OF CHAR;
BEGIN
  TimeConv.unpack(t, COMPILER.TIMESTAMP);
  Printf.sprintf(str, "%s (build %02d.%02d.%4d)",header,t.day,t.month,t.year);
  DStrings.Assign(str, env.info.title);
END SetCompilerTitle;

PROCEDURE SetMode(VAR s: ARRAY OF CHAR);

  PROCEDURE compare(a-,b-: ARRAY OF CHAR; n: LONGINT): BOOLEAN;
    VAR i: LONGINT;
  BEGIN
    i:=1;
    WHILE (i<LEN(a)) & (a[i]#0C) & (b[i]#0C) & (CAP(a[i])=b[i]) DO
      INC(i)
    END;
    RETURN (a[i]=0C) & (i>n)
  END compare;

  PROCEDURE set_job(j: INTEGER);
  BEGIN
    IF job=nothing THEN job:=j
    ELSE
      env.errors.Message(416);
      IF env.config.Option("XTEST") THEN HALT(OTHER) ELSE HALT(1) END;
    END;
  END set_job;

BEGIN
  IF    compare(s,"=PROJECT",1)   THEN set_job(pro);
<* IF TARGET_IDB THEN *>
  ELSIF compare(s,"=INTERVIEW",1) THEN set_job(pro); env.InterViewMode := TRUE;
<* END *>
  ELSIF compare(s,"=MAKE",1)      THEN set_job(make);
  ELSIF compare(s,"=COMPILE",1)   THEN set_job(comp);
  ELSIF compare(s,"=BROWSE",2)    THEN set_job(sym2def);
  ELSIF compare(s,"=GEN",1)       THEN set_job(gen);
  ELSIF compare(s,"=OPTIONS",1)   THEN options:=TRUE
  ELSIF compare(s,"=EQUATIONS",1) THEN equations:=TRUE
  ELSIF compare(s,"=BATCH",2)     THEN batch:=TRUE
  ELSIF compare(s,"=ALL",1)       THEN all:=TRUE
  ELSIF compare(s,"=HELP",1)      THEN help:=TRUE
  ELSE
    env.errors.Message(415,s); HALT(OTHER);
  END;
END SetMode;

(*----------------------------------------------------------------*)

PROCEDURE Insert(name-: ARRAY OF CHAR; VAR root,x: Node);
  VAR l,p: Node;
BEGIN
  l:=root; p:=NIL;
  WHILE (l#NIL) & (name > l.name^) DO p:=l; l:=l.next END;
  IF (l=NIL) OR (name#l.name^) THEN
    NEW(x); NEW(x.name,LENGTH(name)+1); COPY(name,x.name^);
    IF p=NIL THEN x.next:=root; root:=x
    ELSE x.next:=l; p.next:=x;
    END;
  ELSE x:=NIL;
  END;
END Insert;

PROCEDURE (VAR o: ShowOpts) Do(name-: ARRAY OF CHAR; val: BOOLEAN): BOOLEAN;
  VAR n: Node;
BEGIN
  Insert(name,o.node,n);
  IF n#NIL THEN n.opt:=val END;
  RETURN TRUE
END Do;

PROCEDURE (VAR e: ShowEqus) Do(name-: ARRAY OF CHAR; val: env.String): BOOLEAN;
  VAR n: Node;
BEGIN
  Insert(name,e.node,n);
  IF n#NIL THEN n.val:=val END;
  RETURN TRUE
END Do;

PROCEDURE ShowEnv;
  VAR o: ShowOpts; e: ShowEqus; n: Node; c: CHAR; i: INTEGER;
BEGIN
  IF options THEN
    o.node:=NIL; env.config.ListOptions(o,TRUE);
    env.info.print('Options:\n');
    n:=o.node; i:=0;
    WHILE n#NIL DO
      IF n.opt THEN c:='+' ELSE c:='-' END;
      env.info.print(' %c%-14.14s',c,n.name^);
      IF LENGTH(n.name^)>14 THEN c:="." ELSE c:=" " END;
      env.info.print('%.3c',c);
      IF i = 3 THEN i:=0; env.info.print('\n') ELSE INC(i) END;
      n:=n.next
    END;
    IF i#0 THEN env.info.print('\n') END;
  ELSIF equations THEN
    e.node:=NIL; env.config.ListEquations(e,'',TRUE);
    env.info.print('Equations:\n');
    n:=e.node;
    WHILE n#NIL DO
      IF n.val=NIL THEN env.info.print(' -%-14.14s =\n',n.name^);
      ELSE              env.info.print(' -%-14.14s = %s\n',n.name^,n.val^);
      END;
      n:=n.next
    END;
  END;
END ShowEnv;

(*----------------------------------------------------------------*)

PROCEDURE ShowHeader;
BEGIN
  IF env.dc_header IN env.decor THEN
    env.info.print("%s\n", env.info.title^);
  END;
END ShowHeader;

PROCEDURE ShowTailer;
BEGIN
  IF env.dc_tailer IN env.decor THEN env.info.Total END;
END ShowTailer;

PROCEDURE Help;
  VAR name: String; compiler: xfs.FNAME;
BEGIN
  env.args.ProgramName(name);
  IF ~ xfs.sys.sGetName(name^,compiler) THEN compiler:='xm' END;
  INCL(env.decor,env.dc_header);
  ShowHeader;

  env.info.print("Configuration:\n");
  env.info.print("   Front end:             %s\n", pcK.pars.vers);
  env.info.print("   Back end:              %s\n", pcK.code.vers);
<* IF COMPONENT_TESTCOVERAGE THEN *>
  env.info.print("   Test coverage system:  %s RTS v%d\n", tcConfig.version, tcConfig.versionRTS);
<* END *>

  env.info.print("\nUsage:\n  Make project:\n" +
            "    %s =project [=batch] [=all] { PROJECTFILE | OPTION | EQUATION }\n"
            ,compiler);
<* IF TARGET_IDB THEN *>
  env.info.print("  Make project database:\n" +
            "    %s =interview [=batch] [=all] { PROJECTFILE | OPTION | EQUATION }\n"
            ,compiler);
<* END *>
  env.info.print("  Check dependencies and recompile:\n" +
            "    %s =make [=batch] [=all] { FILENAME | OPTION | EQUATION }\n"
            ,compiler);
  env.info.print("  Extract definition from symbol file:\n" +
            "    %s =browse { MODULENAME | OPTION | EQUATION }\n"
            ,compiler);
  env.info.print("  Generate make file:\n" +
            "    %s =gen { PROJECTFILE | OPTION | EQUATION }\n"
            ,compiler);
  env.info.print("  Compile:\n" +
            "    %s [=compile] { FILENAME | OPTION | EQUATION }\n"
            ,compiler);
  env.info.print("\nOptional operation modes:\n" +
            "  =all        Recompile all modules\n"         +
            "  =batch      Prepare batch file\n"            +
            "  =equations  Show equations\n"                +
            "  =options    Show options\n"                  +
            "  =help       Print this text\n"
            );
END Help;

(*----------------------------------------------------------------*)

PROCEDURE Collect;
BEGIN
  IF (XMM.X2C_busymem < 3000000) & (XMM.X2C_usedmem < XMM.X2C_maxmem DIV 2) THEN
    RETURN
  END;
  oberonRTS.Collect;
END Collect;

PROCEDURE ListFiles(curpro: mk.Project);
  VAR x: mk.File; n: mk.Node; fnm: String;
BEGIN
  IF ~ env.shell.Active() THEN RETURN END;
  env.shell.StartFileList;
  n:=curpro.nodes;
  WHILE n#NIL DO
    x:=n.mod;
    IF x.mode IN {mk.md_def,mk.md_mod,mk.md_main,mk.md_oberon} THEN
      xfs.sys.ConvertToHost(x.fname^,fnm);
      env.shell.AppendFile(fnm^);
    END;
    n:=n.next;
  END;
  env.shell.EndFileList;
END ListFiles;

PROCEDURE Recompile(curpro: mk.Project);
  VAR
    x      : mk.File;
    a      : env.String;
    nodes  : mk.Node;
BEGIN
<* IF COMPONENT_TESTCOVERAGE THEN *>
  tcMain.Init(curpro.fname);
<* END *>
  IF batch THEN
    xcMain.MakeBatch(curpro,all);
  ELSIF env.config.Option("GLOBAL") THEN
    xcMain.CompileProject(curpro,all);
  ELSE
    nodes:=curpro.nodes;
    WHILE nodes#NIL DO
      x:=nodes.mod;
      IF x.IsCompilable(all) THEN
        Collect;
        xcMain.CompileModule(curpro,x);
        IF env.info.code_file#NIL THEN
          env.config.Equation("COMPILE",a);
          IF (a#NIL) & (a^#"") THEN
            env.config.Save;
            IF env.info.module # NIL THEN
              env.config.SetEquation("MODULE",env.info.module^)
            END;
            IF env.info.file # NIL THEN
              env.config.SetEquation("FILE",env.info.file^)
            END;
            xcF.Execute(curpro,a,FALSE);
            env.config.Restore;
          END;
        END;
        env.info.module:=NIL; (* for GC *)
      END;
      nodes:=nodes.next;
    END;
  END;
<* IF COMPONENT_TESTCOVERAGE THEN *>
  tcMain.CreateTCSModule();
<* END *>
END Recompile;

PROCEDURE Exit;
BEGIN
  IF env.config.Option("XTEST") THEN
    IF env.other   IN env.err_sum THEN HALT(OTHER)   END;
    IF env.fault   IN env.err_sum THEN HALT(FAULT)   END;
    IF env.error   IN env.err_sum THEN HALT(ERROR)   END;
    IF env.warning IN env.err_sum THEN HALT(WARNING) END;
  ELSE
    IF env.err_sum*{env.other,env.fault,env.error}#{} THEN HALT(1) END;
  END;
  HALT(0);
END Exit;

PROCEDURE RecompileAndLink(curpro: mk.Project; make: BOOLEAN);
  VAR a: env.String; f: mk.File;
BEGIN
  curpro.Regulate;
  ListFiles(curpro);
  IF (curpro.errs # 0) OR env.config.Option("__XDS_LIST__") THEN RETURN END;

<* IF TARGET_IDB THEN *>
  IF env.InterViewMode THEN
    model2.initialize(all);
  END;
<* END *>


  Recompile(curpro);
  IF (curpro.errs=0) & ~ batch THEN
    IF curpro.comp = 0 THEN env.errors.Message(435) END;
    IF make THEN
      f:=curpro.list;
      LOOP
        IF f = NIL THEN (* main not found *) RETURN END;
        IF   (f.mode = mk.md_main)
          OR (f.mode = mk.md_oberon) & (mk.obmain IN f.tags)
        THEN EXIT
        END;
        f:=f.next;
      END;
    END;
    IF env.config.Option("MAKEFILE") THEN xcF.MakeProject(curpro) END;
    oberonRTS.Collect;
<* IF MODE = "WORK" THEN *>
    env.info.print("\nX2C_usedmem=%d\n",xmRTS.X2C_usedmem);
<*END *>
    env.config.Equation("LINK",a);
    <* IF ~TARGET_IDB THEN *>
    IF (curpro.errs=0) & (a#NIL) & (a^#"") THEN
      ShowTailer;
      IF env.shell.Active() THEN env.shell.Caption ("Linking...") END;
      xcF.Execute(curpro,a,TRUE);
      xcMain.EndProject(curpro);
      Exit;
    END;
    <* ELSE*>
    IF env.InterViewMode THEN model2.post(curpro); END;

    IF (curpro.errs=0) & (a#NIL) & (a^#"") & (~ env.InterViewMode ) THEN
      ShowTailer;
      IF env.shell.Active() THEN env.shell.Caption ("Linking...") END;
      xcF.Execute(curpro,a,TRUE);
      xcMain.EndProject(curpro);
      Exit;
    END;
    <* END *>
  END;
END RecompileAndLink;

VAR
  curpro: mk.Project; (* global due bug in X2 garbage collector *)

PROCEDURE Do;
  VAR
    i     : INTEGER;
    a     : env.String;
    str   : String;
    err   : BOOLEAN;
    lang  : pcK.Lang;
BEGIN
  env.err_sum:={};
  job:=nothing;  batch:=FALSE; all:=FALSE; help:=FALSE;
  options:=FALSE; equations:=FALSE;
  i:=0;
  WHILE i < env.args.Number() DO
    env.args.GetArg(i,str);
    IF str[0]=mode_tag THEN
      SetMode(str^);
      env.args.DeleteArg(i);
    ELSE
      INC(i);
    END;
  END;
  IF help THEN Help; RETURN END;
  IF env.args.Number()=0 THEN
    IF options OR equations THEN
      env.config.Equation("PRJ",str);
      IF str#NIL THEN
        IF xcMain.UseProject(str^,curpro) THEN HALT(OTHER) END;
        ShowEnv;
        xcMain.EndProject(curpro);
      ELSE
        ShowEnv;
      END;
    ELSE
      Help;
    END;
    RETURN;
  END;
(*----------------------------------------------------------------*)
  IF job=pro THEN
    ShowHeader;
    FOR i:=0 TO env.args.Number()-1 DO
      IF env.shell.Active() THEN
        env.shell.Caption ("Checking dependencies");
      END;
      env.args.GetArg(i,str);
      xfs.sys.ConvertFromHost(str^,str);
      xfs.sys.ConvertToHost(str^,a);      --- !!! to print name in host format
      env.info.print('Make project "%s"\n',a^);
    <* IF COMPONENT_TESTCOVERAGE THEN *>
      tcMain.Init(str);
    <* END *>
      IF xcMain.ReadProject(str^,curpro) THEN HALT(OTHER) END;
      IF options OR equations THEN ShowEnv END;
      RecompileAndLink(curpro,FALSE);
      xcMain.EndProject(curpro);
      curpro:=NIL;
    END;
  ELSIF job=gen THEN
    ShowHeader;
    FOR i:=0 TO env.args.Number()-1 DO
      env.args.GetArg(i,str);
      xfs.sys.ConvertFromHost(str^,str);
      xcF.MakeFile(str^);
    END;
  ELSE
    env.config.Equation("PRJ",str);
    IF str=NIL THEN
      err:=xcMain.UseProject('',curpro)
    ELSE
      xfs.sys.ConvertFromHost(str^,str);
      err:=xcMain.UseProject(str^,curpro)
    END;
    IF err THEN HALT(OTHER) END;
    IF options OR equations THEN ShowEnv END;
    IF    env.config.Option("M2")  THEN lang:=pcK.flag_m2;
    ELSIF env.config.Option("O2")  THEN lang:=pcK.flag_o2;
    ELSIF env.config.Option("SL1") THEN lang:=pcK.flag_sl1;
    ELSE lang:=SYSTEM.VAL(pcK.Lang,-1);
    END;
  <* IF COMPONENT_TESTCOVERAGE THEN *>
    tcMain.Init(curpro.fname);
  <* END *>
    IF job=sym2def THEN
      ShowHeader;
      FOR i:=0 TO env.args.Number()-1 DO
        env.args.GetArg(i,str);
        xfs.sys.GetName(str^,str);
        env.info.print('Browser, "%s"\n',str^);
        xcMain.Browser(str)
      END;
      env.info.module:=NIL;
    ELSIF job=make THEN
      ShowHeader;
      FOR i:=0 TO env.args.Number()-1 DO
        env.args.GetArg(i,str);
        xfs.sys.ConvertFromHost(str^,str);
        curpro.Append(str^,lang)
      END;
      RecompileAndLink(curpro,TRUE);
    ELSE
      FOR i:=0 TO env.args.Number()-1 DO
        env.args.GetArg(i,str);
        xfs.sys.ConvertFromHost(str^,str);
        Collect;
        xcMain.Compile(curpro,str^,lang);
        IF env.info.code_file#NIL THEN
          env.config.Equation("COMPILE",a);
          IF (a#NIL) & (a^#"") THEN xcF.Execute(curpro,a,FALSE) END;
        END;
        env.info.module:=NIL;
      END;
    END;
    xcMain.EndProject(curpro);
  END;
  ShowTailer;
   Exit;
END Do;

(*----------------------------------------------------------------*)

PROCEDURE be(x: PROCEDURE; nm-: ARRAY OF CHAR);
  VAR b,c: pcK.CODE;
BEGIN
  c:=pcK.code; pcK.code:=NIL; x;
  b:=pcK.code; pcK.code:=c; b.insert(nm);
END be;

BEGIN
  xmConfig.SetManagers;
  xmFS.SetManagers;
  xmFM.SetManagers;
  xmErrors.SetManagers;  (* error manager, info manager *)
  xmArgs.SetManagers;    (* arguments *)

  <* IF XDSIDE THEN *> xmShell.Set; (* after xmArgs.Set ! *) <* END *>

  xmPlatform.Declare;
  xcMain.DeclareOptions;

  <* IF TARGET_386   THEN *> be(opCode.Set,"X86");      <* END *>
  <* IF TARGET_RISC  THEN *> be(opCode.Set,"RISC");     <* END *>
  <* IF TARGET_SPARC THEN *> be(opCode.Set,"SPARC");    <* END *>
  <* IF TARGET_68K   THEN *> be(opCode.Set,"68K");      <* END *>
  <* IF TARGET_VAX   THEN *> be(opCode.Set,"VAX");      <* END *>
  <* IF TARGET_C     THEN *> be(ccCode.Set,"C");        <* END *>
  <* IF TARGET_NULL  THEN *> be(beNull.Set,"NULL");     <* END *>

  pcNum.Set;
  pcK.CreatePrimitiveTypes();
  pcConst.Set;
  pcM2.Set;

(*
  <* IF TARGET_86   THEN *> be(inMain.Set,"I86");      <* END *>
  <* IF TARGET_386  THEN *> be(opCode.Set,"X86");      <* END *>
  <* IF TARGET_RISC THEN *> be(opCode.Set,"RISC");     <* END *>
  <* IF TARGET_SPARC THEN *> be(opCode.Set,"SPARC");   <* END *>
  <* IF TARGET_68K  THEN *> be(opCode.Set,"68K");      <* END *>
  <* IF TARGET_VAX  THEN *> be(opCode.Set,"VAX");      <* END *>
  <* IF TARGET_C    THEN *> be(ccCode.Set,"C");        <* END *>
  <* IF TARGET_STAT THEN *> be(slAnalys.Set,"STAT");   <* END *>
  <* IF TARGET_O2   THEN *> be(ocCode.Set,"O2");       <* END *>
*)

  IF pcK.code=NIL THEN
    env.info.print("There are no back-ends in compiler configuration.\n");
  END;
  IF pcK.pars=NIL THEN
    env.info.print("There are no front-ends in compiler configuration.\n");
  END;
  SetCompilerTitle;

  Redirection;
  Config;
  xcMain.ParseCommandLine;
  oberonRTS.gcAnchorTrace(env.config.Option("gcAnchorTrace"));
  Do;
END xm.
