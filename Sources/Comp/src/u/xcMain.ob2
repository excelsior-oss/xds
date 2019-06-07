(* Copyright (c) 1993,97 XDS Ltd, Russia. All Rights Reserved. *)
(* o2/m2 development system: user interface *)
<* IF ~ DEFINED(COMPONENT_TESTCOVERAGE) THEN *> <* NEW COMPONENT_TESTCOVERAGE- *> <* END *>
MODULE xcMain; (* Ned 04-Feb-93. *)
               (* Ned 03-Mar-94. v2.0 *)

(* Modifications:
   26-Jan-96 Ned:       CompileProject enable/disable back-end
   25-Mar-96 Ned:       PRO0033: macros - use equations and environment
                        variables, like $(XDSDIR).
                        $$ should be used for $.
   28-Mar-96 Ned:       call of "macro" is uncommented.
*)

IMPORT
  pc:=pcK,
  pcO,
  mk:=xcMake,
  xcStr,
  xfs:=xiFiles,
  env:=xiEnv,
  CharClass,
  DStrings,
  Strings,
  PFNConv,
  COMPILER,
<* IF TARGET_IDB THEN *>
  model2,
<* END *>
  SYSTEM;


(*----------------------------------------------------------------*)

CONST
  msg_undef_extension  = 406; (* %s *)
  msg_file_open_error  = 425; (* %s *)
  msg_file_create_error= 424; (* %s *)
  msg_file_close_error = 432; (* %s *)
  msg_error_in_file    = 414; (* %s %d %s *)
  msg_error_in_file_ps = 441; (* %s %d %d %s *)

  msg_line_too_long    = 429;
  msg_syntax_error     = 428;
  msg_type_error       = 442;
  msg_not_defined      = 443;
  msg_end_if_missed    = 446;
  msg_error_in_command_line = 430;

  msg_undefined_option = 320;

CONST
  PRJ = "PRJ";
  LOOKUP = "LOOKUP";
  BACKEND= "BACKEND";
  FATFS  = "FATFS";

TYPE
  ScanIf   = POINTER TO ScanIfRec;
  Scan*    = RECORD
    in-    : xfs.TextFile;
    linebf-: env.String;
    lineno-: LONGINT;
    ifs    : ScanIf;
    false  : env.String;
    true   : env.String;
    srcbf  : env.String;
  END;
  ScanRed  = RECORD (Scan) END;
  ScanIfRec= RECORD
    val    : BOOLEAN;
    else   : BOOLEAN;
    elsif  : BOOLEAN;
    up     : ScanIf;
  END;
  ScanPro  = RECORD (Scan)
    config : BOOLEAN;     (* in  *)
    options: BOOLEAN;     (* in  *)
    project: mk.Project;  (* in  *)
  END;
  Project* = mk.Project;
  IterEqu  = RECORD (env.Equations)
  END;

VAR
  cur_name: xfs.String;

(*----------------------------------------------------------------*)

PROCEDURE WrongSyntax(f: xfs.TextFile; line,ps: LONGINT; no: INTEGER);
  VAR buf: env.MESSAGE;
      fname: xfs.String;
BEGIN
  env.errors.GetMsg(no,buf);
  xfs.sys.ConvertToHost(f.name^, fname);
  IF ps<0 THEN
    env.errors.Message(msg_error_in_file,fname^,line,buf);
  ELSE
    env.errors.Message(msg_error_in_file_ps,fname^,line,ps,buf);
  END;
END WrongSyntax;

PROCEDURE (VAR s: Scan) Do*(): BOOLEAN;
(** ABSTRACT. Returns TRUE to stop iteration *)
BEGIN
  RETURN TRUE
END Do;

PROCEDURE (VAR p: Scan) Preprocessor(): BOOLEAN;

  CONST
    ident  = 1X;
    string = 2X;

  TYPE
    STR = ARRAY 256 OF CHAR;

  VAR
    ps  : INTEGER;
    syps: INTEGER;
    str : STR;
    sy  : CHAR;
    err : BOOLEAN;
    skip: BOOLEAN;

  PROCEDURE on(i: ScanIf): BOOLEAN;
  BEGIN
    RETURN (i=NIL) OR (i.val#i.else) & on(i.up);
  END on;

  PROCEDURE is_letter(): BOOLEAN;
  BEGIN
    IF CharClass.IsWhiteSpace(p.linebf[ps]) THEN RETURN FALSE END;
    CASE p.linebf[ps] OF
      |'!','(',')','=','#','&','<','>','+','-',0X: RETURN FALSE;
    ELSE
      RETURN TRUE;
    END;
  END is_letter;

  PROCEDURE next;
    VAR j: INTEGER;
  BEGIN
    j:=0;
    WHILE CharClass.IsWhiteSpace(p.linebf[ps]) DO INC(ps) END;
    syps:=ps;
    LOOP
      IF p.linebf[ps]='"' THEN
        INC(ps);
        WHILE (p.linebf[ps]#0X) & (p.linebf[ps]#'"') DO
          IF j<LEN(str)-1 THEN str[j]:=p.linebf[ps]; INC(j) END;
          INC(ps);
        END;
        str[j]:=0X; sy:=string;
        IF p.linebf[ps]='"' THEN INC(ps) END;
        RETURN;
      ELSIF is_letter() THEN
        IF j<LEN(str)-1 THEN str[j]:=p.linebf[ps]; INC(j) END;
        INC(ps);
      ELSE
        IF j=0 THEN
          sy:=p.linebf[ps]; INC(ps)
        ELSE
          str[j]:=0X; sy:=ident; Strings.Capitalize(str);
        END;
        RETURN;
      END;
    END;
  END next;

  PROCEDURE next_all;
    VAR j: INTEGER;
  BEGIN
    j:=0;
    WHILE CharClass.IsWhiteSpace(p.linebf[ps]) DO INC(ps) END;
    WHILE p.linebf[ps]#0X DO
      IF j<LEN(str)-1 THEN str[j]:=p.linebf[ps]; INC(j) END;
      INC(ps);
    END;
    WHILE (j>0) & CharClass.IsWhiteSpace(str[j-1]) DO DEC(j) END;
    str[j]:=0X;
  END next_all;

  PROCEDURE ps_error(no: INTEGER);
  BEGIN
    err:=TRUE;
    WrongSyntax(p.in,p.lineno,syps+1,no);
  END ps_error;

  PROCEDURE syntax_error;
  BEGIN
    ps_error(msg_syntax_error);
  END syntax_error;

  PROCEDURE type_error;
  BEGIN
    ps_error(msg_type_error);
  END type_error;

  PROCEDURE not_defined;
  BEGIN
    IF ~ on(p.ifs) THEN RETURN END;
    ps_error(msg_not_defined);
  END not_defined;

  PROCEDURE boolean(x-: STR): BOOLEAN;
  BEGIN
    IF x=p.false^ THEN RETURN FALSE;
    ELSIF x=p.true^ THEN RETURN TRUE;
    ELSE type_error; RETURN FALSE;
    END;
  END boolean;

  PROCEDURE ^ expression(VAR z: STR);

  PROCEDURE factor(VAR z: STR);
    VAR x: STR; v: BOOLEAN; o: env.String;
  BEGIN
    IF sy='(' THEN
      next;
      expression(z);
      IF sy#')' THEN syntax_error ELSE next END;
    ELSIF sy=string THEN
      z:=str;
      next;
    ELSIF sy#ident THEN
      syntax_error;
      z:="";
    ELSIF str="NOT" THEN
      next; factor(x);
      IF ~ boolean(x) THEN COPY(p.true^,z) ELSE COPY(p.false^,z) END;
    ELSIF str="DEFINED" THEN
      next;
      IF sy#ident THEN syntax_error; z:=""; RETURN END;
      SYSTEM.EVAL(env.config.Option(str));
      v:=env.config.res=env.ok;
      IF ~v THEN
        env.config.Equation(str,o);
        v:=env.config.res=env.ok;
      END;
      next;
      IF v THEN COPY(p.true^,z) ELSE COPY(p.false^,z) END;
    ELSIF skip THEN
      COPY(p.false^,z); next;
    ELSE
      v:=env.config.Option(str);
      IF env.config.res#env.ok THEN
        env.config.Equation(str,o);
        IF o=NIL THEN z:="" ELSE COPY(o^,z) END;
        IF env.config.res#env.ok THEN not_defined END;
      ELSIF v THEN
        COPY(p.true^,z);
      ELSE
        COPY(p.false^,z);
      END;
      next;
    END;
  END factor;

  PROCEDURE term(VAR z: STR);
    VAR y: STR; zv,sv: BOOLEAN;
  BEGIN
    factor(z);
    LOOP
      IF sy#ident THEN
        EXIT;
      ELSIF str="AND" THEN
        zv:=boolean(z); next;
        sv:=skip; skip:=~zv; factor(y); skip:=sv;
        IF zv & boolean(y) THEN COPY(p.true^,z) ELSE COPY(p.false^,z) END;
      ELSE
        EXIT;
      END;
    END;
  END term;

  PROCEDURE simple(VAR z: STR);
    VAR y: STR; zv,sv: BOOLEAN;
  BEGIN
    term(z);
    LOOP
      IF sy='+' THEN
        next; term(y);
        Strings.Append(y,z);
      ELSIF sy#ident THEN
        EXIT;
      ELSIF str="OR" THEN
        zv:=boolean(z); next;
        sv:=skip; skip:=zv; term(y); skip:=sv;
        IF zv OR boolean(y) THEN COPY(p.true^,z) ELSE COPY(p.false^,z) END;
      ELSE
        EXIT;
      END;
    END;
  END simple;

  PROCEDURE expression(VAR z: STR);
    VAR y   : STR;
        op  : CHAR;
        res : BOOLEAN;
  BEGIN
    simple(z);
    IF (sy="=") OR (sy="#") OR (sy="<") OR (sy=">") THEN
      op := sy;
      Strings.Capitalize(z);
      next; simple(y);
      Strings.Capitalize(y);
      IF    op="=" THEN res := z=y
      ELSIF op="#" THEN res := z#y
      ELSIF op="<" THEN res := z<y
      ELSE  (*op=">"*)  res := z>y
      END;
      IF res THEN COPY(p.true^,z) ELSE COPY(p.false^,z) END;
    END;        
  END expression;

  PROCEDURE macros(): BOOLEAN;
    VAR j: LONGINT;

    PROCEDURE put(ch: CHAR);
      VAR d: env.String;
    BEGIN
      IF j>=LEN(p.linebf^)-1 THEN
        NEW(d,LEN(p.linebf^)*2);
        p.linebf[j]:=0X;
        COPY(p.linebf^,d^);
        p.linebf:=d;
      END;
      p.linebf[j]:=ch; INC(j);
      p.linebf[j]:=0X;
    END put;

    PROCEDURE puts(s-: ARRAY OF CHAR);
      VAR k: LONGINT;
    BEGIN
      k:=0;
      WHILE (k<LEN(s)) AND (s[k]#0X) DO put(s[k]); INC(k) END;
    END puts;

    PROCEDURE macro(VAR i: LONGINT);
      VAR s: STR; n: LONGINT; x: env.String; ch: CHAR;
    BEGIN
      n:=0; ch:=p.srcbf[i];
      WHILE (ch#0X) & (ch#')') DO
        IF n < LEN(s)-1 THEN s[n]:=ch; INC(n) END;
        INC(i); ch:=p.srcbf[i];
      END;
      s[n]:=0X;
      IF ch # ')' THEN
        (* put everything back *)
        puts("$("); puts(s); RETURN
      END;
      INC(i);
      env.config.Equation(s,x);
      IF x=NIL THEN env.args.EnvString(s,x) END;
      puts(x^);
    END macro;

    VAR st,ch: CHAR; d: env.String; i,k: LONGINT;
  BEGIN
    i:=0; j:=0; st:=0X; p.linebf[0]:=0X;
    LOOP
      ch:=p.srcbf[i]; INC(i);
      IF ch=0X THEN RETURN FALSE END;
      IF ((ch='"') OR (ch="'")) & (st=0X) THEN
        st:=ch; put(ch);
      ELSIF (st#0X) & (ch=st) THEN
        st:=0X; put(ch);
      ELSIF (st=0X) & (ch='$') THEN
        ch:=p.srcbf[i]; INC(i);
        IF ch = '!' THEN
          xfs.sys.GetDir(p.in.name^,d);
          IF d[0]=0X THEN DStrings.Assign('.',d) END;
          k:=LENGTH(d^)-1;
          IF d[k]='/' THEN d[k]:=0X END;
          puts(d^);
        ELSIF ch = '(' THEN macro(i);
        ELSIF ch = '$' THEN put('$')
        ELSIF ch = 0X  THEN put('$'); RETURN FALSE
        ELSE put('$'); put(ch);
        END;
      ELSE
        put(ch);
      END;
    END;
  END macros;

  PROCEDURE o_value(nm-: STR): BOOLEAN;
  BEGIN
    IF (sy='+') OR (sy='-') THEN
      env.config.SetOption(nm,sy='+'); next;
      IF sy#0X THEN syntax_error; RETURN TRUE END;
      IF env.config.res#env.ok THEN not_defined; RETURN TRUE END;
    ELSIF sy='=' THEN
      next_all; env.config.SetEquation(nm,str);
      IF env.config.res#env.ok THEN not_defined; RETURN TRUE END;
    ELSE
      syntax_error; RETURN TRUE;
    END;
    RETURN FALSE;
  END o_value;

  VAR
    if  : ScanIf;
    v   : STR;
    fname: xfs.String;

BEGIN
  IF macros() THEN RETURN TRUE END;
  err:=FALSE; skip:=FALSE; ps:=0; syps:=0; next;
  IF sy='!' THEN
    next;
    IF sy#ident THEN
      IF on(p.ifs) THEN RETURN p.Do() END;
      RETURN FALSE;
    ELSIF str="IF" THEN
      next;
      NEW(if); if.else:=FALSE; if.elsif:=FALSE; if.up:=p.ifs;
      expression(v);
      if.val:=boolean(v);
      IF (sy#ident) OR (str#"THEN") THEN syntax_error; RETURN TRUE END;
      next;
      IF sy#0X THEN syntax_error; RETURN TRUE END;
      p.ifs:=if;
      RETURN err;
    ELSIF str="ELSIF" THEN
      IF (p.ifs=NIL) OR (p.ifs.else) THEN syntax_error; RETURN TRUE END;
      next;
      NEW(if); if.else:=FALSE; if.elsif:=TRUE; if.up:=p.ifs;
      expression(v);
      if.val:=boolean(v);
      IF (sy#ident) OR (str#"THEN") THEN syntax_error; RETURN TRUE END;
      next;
      IF sy#0X THEN syntax_error; RETURN TRUE END;
      p.ifs.else:=TRUE; p.ifs:=if;
      RETURN err;
    ELSIF str="ELSE" THEN
      IF (p.ifs=NIL) OR (p.ifs.else) THEN syntax_error; RETURN TRUE END;
      next;
      IF sy#0X THEN syntax_error; RETURN TRUE END;
      p.ifs.else:=TRUE;
      RETURN FALSE;
    ELSIF str="END" THEN
      next;
      IF (p.ifs=NIL) OR (sy#0X) THEN syntax_error; RETURN TRUE END;
      WHILE p.ifs.elsif DO p.ifs:=p.ifs.up END;
      p.ifs:=p.ifs.up;
      RETURN FALSE;
    ELSIF ~ on(p.ifs) THEN
      RETURN FALSE;
    ELSIF str="MESSAGE" THEN
      next;
      expression(v);
      IF sy#0X THEN syntax_error; RETURN TRUE END;
      xfs.sys.ConvertToHost(p.in.name^, fname);
      env.errors.Message(msg_error_in_file,fname^,p.lineno,v);
      RETURN err;
    ELSIF str="SET" THEN
      next;
      IF sy#ident THEN syntax_error; RETURN TRUE END;
      v:=str; next;
      RETURN o_value(v);
    ELSIF str="NEW" THEN
      next;
      IF sy#ident THEN syntax_error; RETURN TRUE END;
      v:=str; next;
      IF (sy='+') OR (sy='-') THEN
        env.config.NewOption(v,FALSE,SYSTEM.VAL(env.CompilerOption,-1));
      ELSIF sy='=' THEN
        env.config.NewEquation(v);
      END;
      RETURN o_value(v);
    END;
  ELSE
    IF ~ on(p.ifs) THEN RETURN FALSE END;
  END;
  RETURN p.Do();
END Preprocessor;

PROCEDURE (VAR s: Scan) ReadText*(f: xfs.TextFile): BOOLEAN;
  (* if error then returns TRUE *)
  VAR
    buf   : ARRAY 512 (*ok*) OF CHAR;
    lm,i  : LONGINT;
BEGIN
  s.in:=f;
  s.ifs:=NIL;
  DStrings.Assign("FALSE",s.false);
  DStrings.Assign("TRUE",s.true);
  lm:=1;
  NEW(s.linebf,128);
  NEW(s.srcbf,128);
  LOOP
    s.lineno:=lm;
    s.srcbf[0]:=0X;
    LOOP
      LOOP
        f.ReadString(buf);
        IF f.readRes # xfs.allRight THEN EXIT END;
        DStrings.Append(buf,s.srcbf);
      END;
      INC(lm);
      i:=LENGTH(s.srcbf^);
      WHILE (i>0) & CharClass.IsWhiteSpace(s.srcbf[i-1]) DO DEC(i) END;
      s.srcbf[i]:=0X;
      IF (i=0) OR (s.srcbf[i-1]#'\') OR (f.readRes#xfs.endOfLine) THEN
        EXIT;
      END;
      s.srcbf[i-1]:=0X;
    END;
    IF s.Preprocessor() THEN RETURN TRUE END;
    IF f.readRes = xfs.endOfInput THEN
      IF s.ifs#NIL THEN
        WrongSyntax(s.in,s.lineno,-1,msg_end_if_missed);
        RETURN TRUE
      END;
      RETURN FALSE
    END;
    ASSERT(f.readRes=xfs.endOfLine);
  END;
END ReadText;

(*----------------------------------------------------------------*)

PROCEDURE MakeBatch*(p: Project; all: BOOLEAN);
  VAR
    name,ext,fn: xfs.String;
    out     : xfs.TextFile;
    x       : mk.File;
    n       : mk.Node;
    pos,len,width: LONGINT;
    long    : BOOLEAN;
    cmode   : INTEGER;
    compiler: env.String;
BEGIN
  env.config.Equation("BATEXT",ext);
  IF ext=NIL THEN DStrings.Assign("bat",ext) END;
  env.config.Equation("BATNAME",name);
  IF name=NIL THEN
    IF p.fname#NIL THEN xfs.sys.GetName(p.fname^,name);
    ELSE DStrings.Assign("out",name)
    END;
  END;
  xfs.sys.Create('',name^,ext^,name);
  xfs.sys.UseFirst(name^,name);
  xfs.text.Open(name^,TRUE);
  IF xfs.text.file=NIL THEN
    env.errors.Message(msg_file_create_error,xfs.text.msg^);
    INCL(env.err_sum,env.other);
    RETURN
  END;
  out:=xfs.text.file(xfs.TextFile); xfs.text.file:=NIL;
  env.info.print('Batch file "%s"\n',name^);
  long:=env.config.Option("LONGNAME");
  env.config.Equation("BATWIDTH",ext);
  IF (ext=NIL) OR ~ xcStr.StrToInt(ext^,width) THEN width:=128 END;
  env.args.ProgramName(compiler);
  xfs.sys.ConvertToTarget(compiler^,compiler);
  pos:=0;
  cmode:=-1;
  n:=p.nodes;
  WHILE n#NIL DO
    x:=n.mod;
    IF x.IsCompilable(all) THEN
      IF long OR (mk.userdef IN x.tags) THEN
        fn:=x.fname;
      ELSE (* make short name *)
        xfs.sys.GetExt(x.fname^,ext);
        xfs.sys.Create('',x.name,ext^,fn);
      END;
      len:=LENGTH(fn^)+1;
      IF pos#0 THEN
        IF (pos+len>width) OR (cmode IN mk.DEFs) & (x^.mode IN mk.IMPs) THEN
          out.print("\n"); pos:=0
        END;
      END;
      IF pos=0 THEN
        out.print("%s",compiler^); pos:=LENGTH(compiler^);
        IF p.fname#NIL THEN
          out.print(" #%s=%s",PRJ,p.fname^);
          INC(pos,LENGTH(PRJ)+3+LENGTH(p.fname^));
        END;
      END;
      out.print(" %s",fn^); INC(pos,len);
      cmode:=x^.mode;
      x.SetResult(TRUE,FALSE);
    END;
    n:=n.next;
  END;
  IF pos#0 THEN out.print("\n") END;
  out.CloseNew(TRUE,FALSE,FALSE,fn);
  IF fn#NIL THEN
    env.errors.Message(msg_file_close_error,fn^);
  END;
END MakeBatch;

(*----------------------------------------------------------------*)

VAR
  mod_no: pc.Mno;
  main  : BOOLEAN;

PROCEDURE DoCompile;
BEGIN
  pc.pars.compile;
<* IF TARGET_IDB THEN *>
  IF env.InterViewMode THEN
    model2.end_module;
  END;
<* END *>
END DoCompile;

PROCEDURE DoCode;
BEGIN
  pc.code.gen_code(mod_no,main);
END DoCode;

PROCEDURE DoBrowser;
BEGIN
  pc.pars.browser(cur_name);
END DoBrowser;

PROCEDURE Compiler(lang: pc.Lang; f: xfs.TextFile; tm: xfs.Time);
  VAR err: BOOLEAN;
BEGIN
  pc.execute(f,TRUE,TRUE,lang,tm,DoCompile,err);
END Compiler;

PROCEDURE Browser*(nm: xfs.String);
  VAR err: BOOLEAN;
BEGIN
  cur_name:=nm;
  pc.execute(NIL,TRUE,FALSE,pc.flag_o2,0,DoBrowser,err);
  cur_name:=NIL;
END Browser;

VAR
  global: BOOLEAN;

PROCEDURE CompileModule*(p: Project; x: mk.File);
  VAR lang: pc.Lang;
BEGIN
  xfs.text.Open(x.fname^,FALSE);
  IF xfs.text.file#NIL THEN
    IF x.mode=mk.md_oberon THEN lang:=pc.flag_o2
    ELSIF x.mode=mk.md_sl1 THEN lang:=pc.flag_sl1
    ELSE lang:=pc.flag_m2
    END;
    Compiler(lang,xfs.text.file(xfs.TextFile),x.TextTime());
    x.SetResult(FALSE,env.errors.err_cnt#0);
    IF env.errors.err_cnt#0 THEN INC(p.errs) END;
    INC(p.comp);
  ELSE
    x.SetResult(FALSE,TRUE);
    INC(p.errs);
  END;
END CompileModule;

PROCEDURE ParseModule(p: Project; x: mk.File);
  VAR err: BOOLEAN; f: xfs.TextFile; lang: pc.Lang;
BEGIN
  xfs.text.Open(x.fname^,FALSE);
  IF xfs.text.file#NIL THEN
    f:=xfs.text.file(xfs.TextFile);
    IF x.mode=mk.md_oberon THEN lang:=pc.flag_o2 ELSE lang:=pc.flag_m2 END;
    pc.execute(f,FALSE,TRUE,lang,x.TextTime(),DoCompile,err);
    x.SetResult(FALSE,err);
    IF err THEN INC(p.errs) END;
    INC(p.comp);
  ELSE
    x.SetResult(FALSE,TRUE);
    INC(p.errs);
  END;
END ParseModule;

PROCEDURE CompileProject*(p: Project; all: BOOLEAN);
  TYPE
    List = POINTER TO ListItem;
    ListItem = RECORD
      mod   : mk.File;
      main  : BOOLEAN;
      mod_no: pc.Mno;
      next  : List;
    END;
  VAR
    n     : mk.Node;
    x     : mk.File;
    list,l: List;
    a     : env.String;
    f     : xfs.TextFile;
    lang  : pc.Lang;
    err   : BOOLEAN;
BEGIN
  global := env.config.Option("GLOBAL");
  pc.search_back_end;
  env.Clear;
  NEW(pc.mods,pc.demo_mod_limit);
  pc.mod_cnt:=pc.ZEROMno;
  pc.sys_mods := NIL;
  list:=NIL;
  env.info.en_b_end:=FALSE; (* Ned: disable back-end *)
  n:=p.nodes;
  WHILE n#NIL DO
    x:=n.mod;
    IF x.IsCompilable(all) THEN
      ParseModule(p,x);
      IF (x.mode IN mk.IMPs) & x.IsCompilable(TRUE) THEN
        NEW(l);
        l.mod:=x;
        l.mod_no:=env.info.mod_no;
        l.main:=env.info.main;
        l.next:=list;
        list:=l;
      END;
    END;
    n:=n.next;
  END;
  env.info.en_b_end:=TRUE; (* enable back-end *)
  pc.search_back_end;
  WHILE list#NIL DO
    l:=list; x:=l.mod; list:=list.next;
    xfs.text.Open(x.fname^,FALSE);
    IF xfs.text.file#NIL THEN
      f:=xfs.text.file(xfs.TextFile);
      IF x.mode=mk.md_oberon THEN lang:=pc.flag_o2 ELSE lang:=pc.flag_m2 END;
      mod_no:=l.mod_no;
      main:=l.main;
      IF global THEN
        pcO.Clean;
      END;
      pc.execute(f,FALSE,TRUE,lang,x.TextTime(),DoCode,err);
      x.SetResult(FALSE,err);
      IF err THEN INC(p.errs) END;
      IF env.info.code_file#NIL THEN
        env.config.Equation("COMPILE",a);
(* cycle in import ( xcF )
        IF (a#NIL) & (a^#"") THEN xcF.Execute(curpro,a) END;
*)
      END;
    ELSE
      x.SetResult(FALSE,TRUE);
      INC(p.errs);
    END;
  END;
  env.info.Reset;
  pc.mods:=NIL;
  pc.sys_mods:=NIL;
  pc.mod_cnt:=pc.ZEROMno;
END CompileProject;

(*----------------------------------------------------------------*)

PROCEDURE (VAR r: ScanRed) Do(): BOOLEAN;
  VAR ps: LONGINT;
BEGIN
  IF r.linebf^#"" THEN
    xfs.sys.ParseRed(r.linebf^,ps);
    IF ps >= 0 THEN WrongSyntax(r.in,r.lineno,ps,msg_syntax_error) END;
  END;
  RETURN FALSE
END Do;

PROCEDURE GetWord(VAR w: ARRAY OF CHAR;
                     s-: ARRAY OF CHAR; VAR i: LONGINT; cap: BOOLEAN);
  VAR n,len: LONGINT; q, c: CHAR;

BEGIN
  len:=LENGTH(s);
  WHILE (i<len) & CharClass.IsWhiteSpace(s[i]) DO INC(i) END;
  n:=0;
  IF (s[i] = '"') OR (s[i] = "'") THEN
    q:= s[i]; INC(i);
    WHILE (i<len) & (s[i] # q) DO
      c:=s[i];
      IF cap THEN c:=CAP(c) END;
      w[n]:=c;
      INC(i); INC(n);
    END;
    IF s[i] = q THEN INC(i) END;
  ELSE
    WHILE (i<len) & ~ CharClass.IsWhiteSpace(s[i]) DO
      c:=s[i];
      IF cap THEN c:=CAP(c) END;
      w[n]:=c;
      INC(i); INC(n);
    END;
  END;
  w[n]:=0C;
END GetWord;

PROCEDURE AppendModules(p: Project; s-: ARRAY OF CHAR): BOOLEAN;
  VAR i: LONGINT;
      w: ARRAY 256 (*!!!!*) OF CHAR;
BEGIN
  i:=0;
  WHILE (i<LEN(s)) & CharClass.IsWhiteSpace(s[i]) DO INC(i) END;
  IF i < LEN(s) THEN
    IF s[i]='!' THEN
      INC(i);
      GetWord(w,s,i,TRUE);
      IF w="MODULE" THEN
        LOOP
          GetWord(w,s,i,FALSE);
          IF w[0]=0X THEN EXIT END;
          p.Append(w,SYSTEM.VAL(pc.Lang,-1));
        END;
      ELSE
        RETURN FALSE
      END;
    ELSE
      RETURN (s[i]=0C) OR (s[i]='%')
    END;
  END;
  RETURN TRUE
END AppendModules;

PROCEDURE ConfigResToMsg(res: INTEGER): INTEGER;
BEGIN
  CASE res OF
    |env.wrongSyntax    : RETURN msg_syntax_error
    |env.unknownOption  : RETURN msg_undefined_option
    |env.definedOption  : RETURN 321
    |env.unknownEquation: RETURN 322
    |env.definedEquation: RETURN 323
  ELSE ASSERT(FALSE);
  END;
END ConfigResToMsg;

PROCEDURE (VAR p: ScanPro) ini;
BEGIN
  p.options:=TRUE;
  p.config:=TRUE;
  p.project:=NIL;
END ini;

PROCEDURE (VAR p: ScanPro) Do(): BOOLEAN;

  PROCEDURE OptError(res: INTEGER; name: env.String);
    VAR no: INTEGER; buf: env.MESSAGE;
        fname: xfs.String;
  BEGIN
    no:=ConfigResToMsg(res);
    env.errors.GetMsg(no,buf);
    IF no#msg_syntax_error THEN xcStr.prn_txt(buf,buf,name^) END;
    xfs.sys.ConvertToHost(p.in.name^, fname);
    env.errors.Message(msg_error_in_file,fname^,p.lineno,buf)
  END OptError;

  CONST Module = "!MODULE ";

  VAR ok: BOOLEAN; ps: LONGINT; i,j: INTEGER; name: env.String;

BEGIN
  IF p.options THEN
    i:=0; j:=0; ok:=TRUE;
    WHILE CharClass.IsWhiteSpace(p.linebf[j]) DO INC(j) END;
    IF (p.linebf[j]=0X) OR (p.linebf[j]='%') THEN RETURN FALSE END;
    WHILE Module[i]#0X DO
      ok:=ok & (CAP(p.linebf[j])=Module[i]);
      INC(i); INC(j);
    END;
    IF ok THEN
      IF p.config THEN
        WrongSyntax(p.in,p.lineno,-1,msg_syntax_error);
        RETURN TRUE;
      ELSIF p.project=NIL THEN
        RETURN FALSE;
      ELSE
        p.project.SetEquations;
        p.options:=FALSE;
      END;
    END;
  END;
  IF p.options THEN
    env.config.Parse(p.linebf^,name);
    IF env.config.res > env.ok THEN
      OptError(env.config.res,name);
    ELSIF (env.config.res=env.isEquation) & (name^=LOOKUP) THEN
      env.config.Equation(LOOKUP,name);
      xfs.sys.ParseRed(name^,ps);
      IF ps >= 0 THEN WrongSyntax(p.in,p.lineno,ps,msg_syntax_error) END;
    ELSIF (env.config.res=env.isEquation) & (name^=BACKEND) THEN
      pc.search_back_end;
    ELSIF (env.config.res#env.isEquation) & (name^=FATFS) THEN
      PFNConv.SetFATFS(env.config.Option(FATFS));
    END;
  ELSE
    ASSERT(p.project#NIL);
    IF ~ AppendModules(p.project,p.linebf^) THEN
      WrongSyntax(p.in,p.lineno,-1,msg_syntax_error)
    END;
  END;
  RETURN FALSE
END Do;

(*----------------------------------------------------------------*)

PROCEDURE ReadRedirection*(f: xfs.TextFile): BOOLEAN;
  VAR r: ScanRed;
BEGIN
  RETURN r.ReadText(f)
END ReadRedirection;

PROCEDURE ReadConfig*(f: xfs.TextFile): BOOLEAN;
  VAR s: ScanPro;
BEGIN
  xfs.sys.SaveRed;
  s.ini;
  RETURN s.ReadText(f);
END ReadConfig;

PROCEDURE GetDecor;
  VAR s: env.String; i: INTEGER;
BEGIN
  env.config.Equation("DECOR",s);
  IF s=NIL THEN
    env.decor:={env.dc_header,env.dc_tailer,env.dc_compiler,
                env.dc_report,env.dc_progress};
    RETURN
  END;
  i:=0; env.decor:={};
  WHILE s[i]#0X DO
    CASE CAP(s[i]) OF
      |'H': INCL(env.decor,env.dc_header);
      |'T': INCL(env.decor,env.dc_tailer);
      |'C': INCL(env.decor,env.dc_compiler);
      |'P': INCL(env.decor,env.dc_progress);
      |'R': INCL(env.decor,env.dc_report);
    ELSE
    END;
    INC(i);
  END;
END GetDecor;

PROCEDURE ParseCommandLine*;
  VAR i,no: INTEGER; s,name: env.String; ps: LONGINT;
    buf: env.MESSAGE;
BEGIN
  xfs.sys.SaveRed;
  env.args.Parse;
  i:=0;
  WHILE i < env.args.Number() DO
    env.args.GetArg(i,s);
    IF env.config.IsValidTag(s^) THEN
      env.config.Parse(s^,name);
      IF env.config.res > env.ok THEN
        no:=ConfigResToMsg(env.config.res);
        env.errors.GetMsg(no,buf);
        IF no#msg_syntax_error THEN xcStr.prn_txt(buf,buf,name^) END;
        env.errors.Message(msg_error_in_command_line,s^,buf)
      ELSIF (env.config.res=env.isEquation) & (name^=LOOKUP) THEN
        env.config.Equation(LOOKUP,name);
        xfs.sys.ParseRed(name^,ps);
        IF ps >=0 THEN
          env.errors.GetMsg(msg_syntax_error,buf);
          env.errors.Message(msg_error_in_command_line,s^,buf)
        END;
      ELSIF (env.config.res#env.isEquation) & (name^=FATFS) THEN
        PFNConv.SetFATFS(env.config.Option(FATFS));
      END;
      env.args.DeleteArg(i);
    ELSE INC(i)
    END;
  END;
  GetDecor;
END ParseCommandLine;

PROCEDURE OpenProject(str-: ARRAY OF CHAR; VAR f: xfs.TextFile);
  VAR dir,name,ext,fn: xfs.FNAME; x: env.String; copy: BOOLEAN;
BEGIN
  copy:=TRUE;
  IF xfs.sys.sGet(str,dir,name,ext) THEN
    IF ext[0]=0X THEN
      env.config.Equation("PRJEXT",x);
      IF x=NIL THEN COPY("prj",ext) ELSE COPY(x^,ext) END;
      copy:=~ xfs.sys.sCreate(dir,name,ext,fn);
    END;
  END;
  IF copy THEN COPY(str,fn) (* bad format, use str *) END;
  xfs.sys.sLookup(fn,fn);
  xfs.text.Open(fn,FALSE);
  IF xfs.text.file=NIL THEN
    env.errors.Message(msg_file_open_error,xfs.text.msg^);
    INCL(env.err_sum,env.other);
    f:=NIL;
  ELSE
    f:=xfs.text.file(xfs.TextFile); xfs.text.file:=NIL;
  END;
  GetDecor;
END OpenProject;

PROCEDURE EndProject*(VAR p: Project);
(** Close bracket to "UseProject" and "ReadProject" *)
BEGIN
  IF p#NIL THEN p.Destroy; p:=NIL END;
  env.config.Restore;
  xfs.sys.RestoreRed;
  GetDecor;
END EndProject;

PROCEDURE UseProject*(name-: ARRAY OF CHAR; VAR p: Project): BOOLEAN;
(** Reads project (option part only) if name is non-empty
  string.
*)
  VAR f: xfs.TextFile; s: ScanPro; r: BOOLEAN;
BEGIN
  r:=FALSE;
  env.config.Save;
  xfs.sys.SaveRed;
  p:=mk.New();
  IF name[0]#0X THEN
    OpenProject(name,f);
    IF f=NIL THEN
      r:=TRUE;
    ELSE
      s.ini; s.config:=FALSE;
      s.project:=p;
      p.fname:=f.name;
      p.SetEquations;
      r:=s.ReadText(f);
      f.Close;
    END;
  END;
  IF r THEN EndProject(p);
  ELSE p.SetEquations;
  END;
  GetDecor;
  RETURN r;
END UseProject;

PROCEDURE ReadProject*(name-: ARRAY OF CHAR; VAR p: Project): BOOLEAN;
(** Reads all project. Use EndProject as close bracket. *)
  VAR f: xfs.TextFile; s: ScanPro; r: BOOLEAN;
BEGIN
  OpenProject(name,f);
  IF f#NIL THEN
    env.config.Save;
    xfs.sys.SaveRed;
    s.ini; s.config:=FALSE;
    p:=mk.New();
    s.project:=p;
    p.fname:=f.name;
    p.SetEquations;
    r:=s.ReadText(f);
    f.Close;
    IF r THEN
      EndProject(p);
    ELSE
      p.SetEquations;
    END;
    RETURN r;
  ELSE
    p:=NIL;
    RETURN TRUE;
  END;
END ReadProject;

(*----------------------------------------------------------------*)

PROCEDURE Compile*(p: Project; str-: ARRAY OF CHAR; lang: pc.Lang);
  VAR ext,fn: xfs.FNAME; was: BOOLEAN; tm: xfs.Time;
BEGIN
  IF VAL(SHORTINT,lang)<0 THEN
    IF xfs.sys.sGetExt(str,ext) THEN lang:=p.GetLang(ext) END;
    IF VAL(SHORTINT,lang) < 0 THEN
      env.errors.Message(msg_undef_extension,str);
      INCL(env.err_sum,env.other);
      RETURN
    END;
  END;
  COPY(str,fn);
  xfs.sys.sLookup(fn,fn);
  xfs.sys.ModifyTime(fn,tm,was);
  xfs.text.Open(fn,FALSE);
  IF xfs.text.file=NIL THEN
    env.errors.Message(msg_file_open_error,xfs.text.msg^);
    INCL(env.err_sum,env.other);
  ELSE
    Compiler(lang,xfs.text.file(xfs.TextFile),tm);
  END;
END Compile;

(*----------------------------------------------------------------*)
PROCEDURE ConvertFileNameToIR(VAR s: env.String);
VAR t: env.String;
BEGIN
  IF s # NIL THEN
    xfs.sys.ConvertFromHost(s^, t);
    s:= t;
  END;
END ConvertFileNameToIR;

(*----------------------------------------------------------------*)

PROCEDURE DeclareOptions*;
  CONST
    opts = "LONGNAME-"
         + "VERBOSE-"
         + "OVERWRITE-"
         + "COMMENT-"
         + "__GEN_C__-"
         + "__XDS__+"
         + "__GEN_X86__-"
         + "__XDS_SHELL__-"
         + "MAKEFILE-"
         + "M2-"
         + "O2-"
         + "NOCODE-"
         + "NOHEADER-"
         + "CSTDLIB-"
         + "GCAUTO-"
         + "XDEBUG-"
         + "GLOBAL-"
         + "FATFS-"
         + "XTEST-"
         + "MAKEDIRS-"
         + "alwaysinline-"
         + "neverinline-"
       <* IF COMPONENT_TESTCOVERAGE THEN *>  
         + env.OPT_TESTCOVERAGE+"-"
       <* END *>
         ;

    equs = "DEF=def;MOD=mod;OBERON=ob2;PASCAL=pas;"
         + "CODE=c;SYM=sym;HEADER=h;"
         + "BSDEF=odf;BSTYLE=DEF;"
         + "BATEXT=bat;BATWIDTH=128;DECOR;"
         + "ATTENTION;MKFEXT=mkf;"
         + "PRJEXT=prj;OBJEXT=obj;ASMEXT=asm;EMFEXT=emf;"
         + "TEMPLATE;LOOKUP;ERRFMT;ERRLIM=16;PROJECT;"
         + "STACKLIMIT;HEAPLIMIT;GCTHRESHOLD;"
         + "COMPILERHEAP;COMPILERTHRES;"
   <* IF TARGET_IDB THEN *>
         + "COMPILE;LINK;ERRORLEVEL;TABSTOP=1;BACKEND;"
         + "FILE;MODULE;"
         + "FidStart=0;"
   <* ELSE *>
         + "COMPILE;LINK;ERRORLEVEL;TABSTOP;BACKEND;"
         + "FILE;MODULE;"
   <* END *>    
         + "ENV_HOST="+COMPILER.TARGET+";ENV_TARGET="+COMPILER.TARGET+";"
   <* IF COMPONENT_TESTCOVERAGE THEN *>  
         + env.EQU_TESTCOVERAGEMASK+"=*;"
   <* END *>
         ;

BEGIN
  env.OptionList(opts);
  env.EquationList(equs);

  env.config.NewActiveEquation("BATNAME", ConvertFileNameToIR);
  env.config.NewActiveEquation("MKFNAME", ConvertFileNameToIR);
  env.config.NewActiveEquation("PRJ",     ConvertFileNameToIR);
END DeclareOptions;

BEGIN
  global := FALSE;
END xcMain.
