(** Copyright (c) 1993,97 XDS Ltd, Russia. All Rights Reserved. *)
(* o2/m2 development system: make file generator *)
MODULE xcF; (* Ned 11-Feb-93. *)
            (* Ned 24-Mar-94. *)
            (* Sem 18-Aug-95. *)

IMPORT
  SYSTEM,
  env:=xiEnv,
  xfs:=xiFiles,
  mk:=xcMake,
  xcMain,
  DStrings,
  Strings,
  cc:=CharClass,
  FileSys,
  xcStr,
  ProgExec,
  FormOut;

CONST
  IDEOPTION = "__XDS_SHELL__";
  ATTNEQU   = "ATTENTION";
  LINKEQU   = "LINK";
  MKFNMEQU  = "MKFNAME";
  MKFEXTEQU = "MKFEXT";
  TEMPLATEEQU = "TEMPLATE";

CONST
  msg_file_open_error  = 425; (* %s *)
  msg_file_create_error= 424; (* %s *)
  msg_file_close_error = 432; (* %s *)
  msg_template_file_error   = 433; (* %s %d.%d %s *)
  msg_template_string_error = 440; (* %d.%d %s\n %s *)
  msg_wrong_equ_value  = 434; (* %s %s *)

  err_string_expected  = 418;
  err_symbol_expected  = 8;
  err_ident_expected   = 7;
  err_inv_string       = 4;
  err_too_many_levels  = 421;
  err_unknown_extension= 419;
  err_unknown_argument = 420;
  err_inconsistent_set = 422;
  err_level0           = 438;

CONST (* additional bits in valid mode set *)
  md_adef  = 16; (* alien definition *)
  md_aimp  = 17; (* alien implementation *)
  md_cstd  = 18; (* C library *)
  md_other = 19; (* unknown for the compiler, use extension  *)
                 (* should not be equal to xcMake.md_unknown *)

CONST (* symbols *)
  ident  = 1X;
  string = 2X;

TYPE
  String = ARRAY 128 OF CHAR;
  Scan = RECORD (xcMain.Scan)
    ou         : xfs.TextFile;
    inp_is_file: BOOLEAN;    (* input is a file, not equation *)
    project    : mk.Project;
    err        : BOOLEAN;
    pos        : INTEGER;
    string     : String;
    dstring    : env.String;
    name       : env.OPNAME; (* ident value *)
    exec       : BOOLEAN;
    attention  : CHAR;
  END;

TYPE EList = POINTER TO ExtNode;
     ExtNode = RECORD
       ext : env.OPNAME;
       next: EList;
     END;

VAR
  null: env.String;

(*----------------------------------------------------------------*)

PROCEDURE (VAR s: Scan) Error(code: INTEGER; name-: ARRAY OF CHAR);
  VAR bump: ARRAY 128 OF CHAR; msg: env.MESSAGE;
BEGIN
  IF ~ s.err THEN
    s.err:=TRUE;
    env.errors.GetMsg(code,msg);
    xcStr.prn_bin(bump,msg,name);
    IF s.inp_is_file THEN
      env.errors.EnvError(msg_template_file_error,
        s.in.name^,s.lineno,s.pos+1,bump);
    ELSE
      env.errors.EnvError(msg_template_string_error,
        s.lineno,s.pos+1,bump,s.linebf^);
    END;
    INC(s.project.errs);
  END;
END Error;

(*----------------------------------------------------------------*)

PROCEDURE (VAR s: Scan) Get(VAR sy: CHAR);
  VAR i,p: INTEGER;
BEGIN
  sy:=s.linebf[s.pos]; INC(s.pos);
  WHILE cc.IsWhiteSpace(sy) DO sy:=s.linebf[s.pos]; INC(s.pos) END;
  IF sy=0X THEN
    DEC(s.pos);
  ELSIF cc.IsLetter(sy) OR (sy='_') THEN (* ident *)
    i:=0;
    REPEAT
      IF i < LEN(s.name)-1 THEN s.name[i]:=sy; INC(i) END;
      sy:=s.linebf[s.pos]; INC(s.pos);
    UNTIL ~ cc.IsLetter(sy) & ~ cc.IsNumeric(sy) & (sy#'_');
    s.name[i]:=0X;
    DEC(s.pos);
    sy:=ident;
  ELSIF (sy='"') OR (sy="'") THEN (* string *)
    p:=s.pos; i:=p; s.dstring:=NIL;
    WHILE (s.linebf[i]#0X) & (s.linebf[i]#sy) DO INC(i) END;
    IF s.linebf[i]=0X THEN
      s.Error(err_inv_string,'');
      s.string:="";
      s.pos:=i;
    ELSIF i-p<LEN(s.string) THEN
      Strings.Extract(s.linebf^,p,i-p,s.string);
      s.pos:=i+1;
    ELSE
      NEW(s.dstring,i-p+1);
      Strings.Extract(s.linebf^,p,i-p,s.dstring^);
      s.pos:=i+1;
    END;
    sy:=string;
  END;
END Get;

(*----------------------------------------------------------------*)

PROCEDURE (VAR s: Scan) ParseItem(VAR ch: CHAR;
                                  name-,name0-: ARRAY OF CHAR;
                                  mod: mk.File;
                                  en: BOOLEAN);

  PROCEDURE ParseSeq(pos: INTEGER; nm-: ARRAY OF CHAR;
                     mod: mk.File; en: BOOLEAN);
  BEGIN
    s.pos:=pos;
    s.Get(ch);
    WHILE (ch#'}') & (ch#0X) & ~ s.err DO
      s.ParseItem(ch,nm,name,mod,en);
    END;
  END ParseSeq;

  PROCEDURE FileName(name-,ext-: ARRAY OF CHAR; out: BOOLEAN; VAR res: env.String);
    VAR
      done  : BOOLEAN;
      time  : xfs.Time;
      dir   : xfs.FNAME;
      nm    : xfs.FNAME;
      fn    : xfs.FNAME;
  BEGIN
    res:=null;
    IF ~xfs.sys.sGetDir(name,dir) THEN RETURN END;
    IF ~xfs.sys.sGetName(name,nm) THEN RETURN END;
    IF ~xfs.sys.sCreate(dir,nm,ext,fn) THEN RETURN END;
    IF out THEN
      xfs.sys.sUseFirst(fn,fn);
    ELSE
      xfs.sys.sLookup(fn,fn);
      xfs.sys.ModifyTime(fn,time,done);
      IF ~ done THEN xfs.sys.sUseFirst(fn,fn) END;
    END;
    xfs.sys.ConvertToTarget(fn,res);
  END FileName;

  PROCEDURE Iterator;

    TYPE Unit = POINTER TO RECORD (mk.UnitDesc) mod: mk.File END;

    VAR
      nms : mk.Unit;
      pos : INTEGER;
      set : SET;
      ext : EList;

    PROCEDURE app(nm-: ARRAY OF CHAR; m: mk.File);
      VAR u: Unit; v: mk.Unit;
    BEGIN
      ASSERT(nm#"");
      v:=mk.Search(nms,nm);
      IF v#NIL THEN RETURN END;
      NEW(u);
      COPY(nm,u.name);
      u.Append(nms);
      u.mod:=m;
    END app;

    PROCEDURE CmpExt(m: mk.File; ext: EList): BOOLEAN;
      VAR x: xfs.FNAME; e: EList;
    BEGIN
      e:=ext;
      WHILE e#NIL DO
        IF (m.fname#NIL) & xfs.sys.sGetExt(m.fname^,x) &
           (xfs.sys.CompareExtTarget(e.ext, x)) THEN
          RETURN TRUE
        END;
        e:=e.next;
      END;
      RETURN FALSE
    END CmpExt;

    PROCEDURE test_and_append(m: mk.File; s: SET; ext: EList);
      VAR md: INTEGER;
    BEGIN
      md:=m.mode;
      IF (md=mk.md_def)    & (mk.noheader IN m.tags) THEN md:=md_adef END;
      IF (md=md_adef)      & (mk.cstdlib IN m.tags)  THEN md:=md_cstd END;
      IF (md=mk.md_mod)    & (mk.nocode IN m.tags)   THEN md:=md_aimp END;
      IF (md=mk.md_oberon) & (mk.obmain IN m.tags)   THEN md:=mk.md_main END;
      IF (md=mk.md_sl1)    & (mk.obmain IN m.tags)   THEN md:=mk.md_main END;
      IF md IN s THEN
        app(m.name,m)
      ELSIF (md_other IN s) & CmpExt(m,ext) THEN
        app(m.fname^,m);
      END;
    END test_and_append;

    PROCEDURE one(u: mk.Unit);
    BEGIN
      IF u=NIL THEN RETURN END;
      one(u.l);
      ASSERT(u.name#'');
      ParseSeq(pos,u.name,u(Unit).mod,TRUE);
      one(u.r);
    END one;

    PROCEDURE all(m: mk.File; ext: EList);
      VAR u: mk.Unit;
    BEGIN
      u:=m.u;
      WHILE u#NIL DO
        test_and_append(u(mk.File),set,ext);
        u:=u.s;
      END;
    END all;

    PROCEDURE newext(s: ARRAY OF CHAR);
      VAR e: EList;
    BEGIN
      NEW(e);
      COPY(s,e.ext);
      e.next:=ext; ext:=e;
    END newext;

    VAR
      m   : mk.File;
      u   : mk.Unit;
      n   : mk.Node;
      fn  : env.String;
      cp_name: env.OPNAME; (* capitalized name *)
  BEGIN
    s.Get(ch);
    set:={}; nms:=NIL; ext:=NIL;
    LOOP
      IF ch=ident THEN
        COPY(s.name, cp_name);
        Strings.Capitalize(cp_name);
        IF    cp_name="DEF"     THEN INCL(set,mk.md_def);
        ELSIF cp_name="IMP"     THEN INCL(set,mk.md_mod);
        ELSIF cp_name="OBERON"  THEN INCL(set,mk.md_oberon);
        ELSIF cp_name="MAIN"    THEN INCL(set,mk.md_main);
        ELSIF cp_name="C"       THEN INCL(set,mk.md_code);
        ELSIF cp_name="ASM"     THEN INCL(set,mk.md_asm);
        ELSIF cp_name="OBJ"     THEN INCL(set,mk.md_obj);
        ELSIF cp_name="ADEF"    THEN INCL(set,md_adef);
        ELSIF cp_name="AIMP"    THEN INCL(set,md_aimp);
        ELSIF cp_name="HEADER"  THEN INCL(set,mk.md_header);
        ELSIF cp_name="SYM"     THEN INCL(set,mk.md_sym);
        ELSE
          newext(s.name);
          INCL(set,md_other);
(*!!!          s.Error(err_unknown_argument,s.name); RETURN; *)
        END;
        s.Get(ch);
      ELSIF ch=string THEN
        IF s.dstring=NIL THEN app(s.string,NIL);
        ELSE app(s.dstring^,NIL);
        END;
        s.Get(ch);
      ELSE
        s.Error(err_symbol_expected,"ident or string"); RETURN;
      END;
      IF ch=':' THEN EXIT END;
    END;
    pos:=s.pos;
    DStrings.Assign("dummy",fn);
    ParseSeq(pos,fn^,NIL,FALSE);
    IF s.err THEN RETURN END;
    IF en THEN
      IF set#{} THEN
        IF name='' THEN
          m:=s.project.list;
          WHILE m#NIL DO
            test_and_append(m,set,ext);
            m:=m.next;
          END;
        ELSE
          u:=mk.Search(s.project.tree,name);
          WHILE u#NIL DO
            m:=u(mk.File);
            IF m.from#NIL THEN
              all(m.from,ext); n:=m.from.inp;
              WHILE n#NIL DO all(n.mod,ext); n:=n.next END;
            END;
            u:=u.s;
          END;
        END;
      END;
      one(nms);
    END;
    IF ch#'}' THEN s.Error(err_symbol_expected,"'}'"); RETURN END;
    s.Get(ch);
  END Iterator;

  PROCEDURE GetEquation(VAR val: env.String);
  BEGIN
    IF ch#ident THEN s.Error(err_symbol_expected,"ident"); RETURN END;
    env.config.Equation(s.name,val);
    IF (env.config.res#env.ok) OR (val=NIL) THEN
      s.Error(err_unknown_argument,s.name);
    END;
    s.Get(ch);
  END GetEquation;

  PROCEDURE GetString(VAR val: String; VAR dval: env.String);
  BEGIN
    IF ch=string THEN
      dval:=s.dstring;
      IF dval=NIL THEN val:=s.string END;
      s.Get(ch);
    ELSE
      GetEquation(dval);
    END;
  END GetString;

  PROCEDURE MakeFileName(nm-: ARRAY OF CHAR; out: BOOLEAN; VAR dval: env.String);
     VAR dir,name,ext: xfs.FNAME;
  BEGIN
    dval:=NIL;
    IF xfs.sys.sGet(nm,dir,name,ext) & (dir[0] = 0X) & (ext[0] # 0X) THEN
      FileName(name,ext,out,dval)
    END;
  END MakeFileName;

  PROCEDURE Suffix(nm-: ARRAY OF CHAR; VAR val: String; VAR dval: env.String);
    VAR ext: String; dext: env.String; out: BOOLEAN;
  BEGIN
    val:=""; dval:=NIL;
    IF ch='#' THEN
      IF nm="" THEN s.Error(err_level0,""); RETURN END;
      s.Get(ch); out:=ch='>';
      IF out THEN s.Get(ch) END;
      IF (ch=',') OR (ch=';') OR (ch='}') OR (ch=0X) THEN
        IF (mod#NIL) & (mod.fname#NIL) THEN
          xfs.sys.ConvertToTarget(mod.fname^,dval);
        ELSE
          MakeFileName(nm,out,dval);
          IF dval = NIL THEN DStrings.Assign(nm,dval) END;
        END;
      ELSE
        GetString(ext,dext);
        IF s.err THEN RETURN END;
        IF dext=NIL THEN FileName(nm,ext,out,dval);
        ELSE             FileName(nm,dext^,out,dval);
        END;
      END;
    ELSIF LENGTH(nm)<LEN(val) THEN
      COPY(nm,val);
    ELSE
      DStrings.Assign(nm,dval);
    END;
  END Suffix;

  PROCEDURE Expression(VAR val: String; VAR dval: env.String);
    VAR nm: String; dnm: env.String; up: BOOLEAN;
  BEGIN
    IF ch='^' THEN up:=TRUE; s.Get(ch) ELSE up:=FALSE END;
    IF ch#'#' THEN
      GetString(nm,dnm);
      IF dnm#NIL THEN Suffix(dnm^,val,dval);
      ELSE Suffix(nm,val,dval);
      END;
    ELSIF up THEN
      Suffix(name0,val,dval);
    ELSE
      Suffix(name,val,dval);
    END;
  END Expression;

  PROCEDURE Format(fmt-: ARRAY OF CHAR);
    VAR s1,s2: String; val1,val2: env.String;
  BEGIN
    s1:=""; val1:=NIL; val2:=null;
    IF ch=',' THEN
      s.Get(ch);
      Expression(s1,val1);
      IF ch=',' THEN
        s.Get(ch);
        Expression(s2,val2);
        IF val2=NIL THEN DStrings.Assign(s2,val2) END;
      END;
    END;
    IF ~s.err & en THEN
      IF val1=NIL THEN
        s.ou.print(fmt,s1,val2^);
      ELSE
        s.ou.print(fmt,val1^,val2^);
      END;
    END;
  END Format;

  VAR fmt: String; dfmt: env.String;
BEGIN
  ASSERT(ch#0X);
  IF ch='{' THEN
    Iterator;
  ELSE
    Expression(fmt,dfmt);
    IF dfmt=NIL THEN Format(fmt) ELSE Format(dfmt^) END;
    IF s.err THEN RETURN END;
    IF ch=';' THEN s.Get(ch); RETURN END;
    IF ch=0X  THEN RETURN END;
    IF ch='}' THEN RETURN END;
    s.Error(err_symbol_expected,"';'");
  END;
END ParseItem;

PROCEDURE (VAR s: Scan) Do(): BOOLEAN;

  PROCEDURE ParseLine;
    VAR ch: CHAR;
  BEGIN
    s.Get(ch);
    WHILE ~ s.err & (ch#0X) DO
      s.ParseItem(ch,'','',NIL,TRUE);
    END;
  END ParseLine;

  PROCEDURE cmd(): BOOLEAN;
  BEGIN
    WHILE cc.IsWhiteSpace(s.linebf[s.pos]) DO INC(s.pos) END;
    IF s.linebf[s.pos]=s.attention THEN INC(s.pos); RETURN TRUE END;
    RETURN FALSE;
  END cmd;

BEGIN
  s.pos:=0;
  IF s.exec THEN
    ParseLine;
  ELSIF cmd() THEN
    ParseLine;
  ELSE
    s.ou.print('%s\n',s.linebf^);
  END;
  RETURN s.err;
END Do;

PROCEDURE Parse(p: mk.Project; in,ou: xfs.TextFile;
                inp_is_file,exec: BOOLEAN): BOOLEAN;
  VAR
    s: Scan;
    a,link: env.String;
BEGIN
  env.config.Save;
  s.err:=FALSE;
  s.exec:=exec;
  s.inp_is_file:=inp_is_file;
  env.config.Equation(ATTNEQU,a);
  IF a=NIL THEN s.attention:="!" ELSE s.attention:=a[0] END;
  s.project:=p;
  s.ou:=ou;
  s.err:=s.ReadText(in) OR s.err;
  env.config.Equation(LINKEQU,link);
  env.config.Restore;
  IF link=NIL THEN env.config.SetEquation(LINKEQU,"");
  ELSE             env.config.SetEquation(LINKEQU,link^);
  END;
  IF s.err THEN INC(p.errs); INCL(env.err_sum, env.other) END;
  RETURN s.err;
END Parse;

PROCEDURE MakeFileName(p: mk.Project): env.String;
  VAR dir,name,ext,fn: env.String;
BEGIN
  env.config.Equation(MKFNMEQU,fn);
  IF fn = NIL THEN
    dir:=null; ext:=null;
    IF p.fname=NIL THEN
      DStrings.Assign("tmp",name);
      env.config.SetEquation(MKFNMEQU,name^);
    ELSE
      xfs.sys.GetName(p.fname^,name); ASSERT(name # xfs.null);
    END;
  ELSE
    xfs.sys.Get(fn^,dir,name,ext);  ASSERT(ext # xfs.null);
  END;
  IF ext[0]=0X THEN
    env.config.Equation(MKFEXTEQU,ext);
    IF ext = NIL THEN DStrings.Assign("mkf",ext) END;
  END;
  xfs.sys.Create(dir^,name^,ext^,fn);
  xfs.sys.UseFirst(fn^,fn);
  RETURN fn;
END MakeFileName;

PROCEDURE OpenOut(p: mk.Project; VAR f: xfs.TextFile; VAR tm: xfs.Time);
  VAR fn: env.String; was: BOOLEAN;
BEGIN
  fn:=MakeFileName(p);
  xfs.sys.ModifyTime(fn^,tm,was);
  xfs.text.Open(fn^,TRUE);
  IF xfs.text.file=NIL THEN
    env.errors.EnvError(msg_file_create_error,xfs.text.msg^); INC(p.errs);
    f:=NIL;
  ELSE
    f:=xfs.text.file(xfs.TextFile);
  END;
END OpenOut;

PROCEDURE RemoveQuote(fn:env.String):env.String;
VAR l,i,j:LONGINT;
    c:CHAR;
BEGIN
   l:=LENGTH(fn^);
   i:=0;
   WHILE (i<l)AND(fn[i]=" ") DO INC(i); END;
   IF (i=l) OR ((fn[i]#"'") AND (fn[i]#'"')) THEN 
      RETURN fn;
   END;
   c:=fn[i];
   j:=0; INC(i);
   WHILE (i<l) AND (fn[i]#c) AND (fn[i]#0X) DO
      fn[j]:=fn[i]; INC(i); INC(j);
   END;
   fn[j]:=0C;
   RETURN fn;
END RemoveQuote;

PROCEDURE TemplateName(): env.String;
  VAR fn: env.String;
BEGIN
  env.config.Equation(TEMPLATEEQU,fn);
  IF fn = NIL THEN DStrings.Assign("template",fn)
  ELSE fn:=RemoveQuote(fn);  (* Sergic fix for BUG956 *)
  END;
  xfs.sys.Lookup(fn^,fn);
  RETURN fn;
END TemplateName;

PROCEDURE OpenInp(p: mk.Project; VAR f: xfs.TextFile; VAR fn: env.String);
BEGIN
  fn:=TemplateName();
  xfs.text.Open(fn^,FALSE);
  IF xfs.text.file=NIL THEN
    env.errors.EnvError(msg_file_open_error,xfs.text.msg^); INC(p.errs);
    f:=NIL; fn:=NIL;
  ELSE
    f:=xfs.text.file(xfs.TextFile);
  END;
END OpenInp;

PROCEDURE Make(p: mk.Project);
  VAR
    a  : env.String;
    in,ou: xfs.TextFile;
    tm : xfs.Time;
    fnm: env.String;
    err: BOOLEAN;
BEGIN
  OpenInp(p,in,fnm);
  IF in#NIL THEN
    OpenOut(p,ou,tm);
    IF ou#NIL THEN
      err:=Parse(p,in,ou,TRUE,FALSE);
      ou.CloseNew(~ err & (p.errs=0),tm>p.PrjTime(),FALSE,a);
      IF a#NIL THEN
        env.errors.EnvError(msg_file_close_error,a^); INC(p.errs);
      ELSIF ~ err & (p.errs=0) & ou.new THEN
        env.info.print('New "%s" is generated using template "%s"\n',
                        ou.name^,in.name^);
      END;
    END;
    in.Close;
  END;
END Make;

PROCEDURE MakeProject*(p: mk.Project);
BEGIN
  Make(p);
END MakeProject;

PROCEDURE MakeFile*(project-: ARRAY OF CHAR);
  VAR p: mk.Project;
BEGIN
  env.info.print('Generate makefile for project "%s"\n',project);
  IF xcMain.ReadProject(project,p) THEN RETURN END;
  p.Regulate;
  IF p.errs=0 THEN Make(p) END;
  xcMain.EndProject(p);
END MakeFile;

PROCEDURE TemplateTime*(): xfs.Time;
  VAR fn: env.String; tm: xfs.Time; was: BOOLEAN;
BEGIN
  fn:=TemplateName();
  xfs.sys.ModifyTime(fn^,tm,was);
  IF NOT was THEN tm:=MIN(xfs.Time) END;
  RETURN tm;
END TemplateTime;

PROCEDURE MakeFileTime*(p: mk.Project): xfs.Time;
  VAR fn: env.String; tm: xfs.Time; was: BOOLEAN;
BEGIN
  fn:=MakeFileName(p);
  xfs.sys.ModifyTime(fn^,tm,was);
  IF NOT was THEN tm:=MIN(xfs.Time) END;
  RETURN tm;
END MakeFileTime;

(*--------------------------------------------*)

TYPE
  TextFile = POINTER TO TextFileDesc;
  TextFileDesc = RECORD (xfs.TextFileDesc)
    ip : LONGINT;
    inp: env.String;
    out: env.String;
  END;

PROCEDURE (f: TextFile) ReadString(VAR s: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  i:=0;
  WHILE (i < LEN(s)-1) & (f.ip < LEN(f.inp^)) DO
    s[i]:=f.inp[f.ip]; INC(i); INC(f.ip);
  END;
  s[i]:=0X;
  f.readLen:=i;
  IF i = 0 THEN f.readRes:=xfs.endOfInput ELSE f.readRes:=xfs.allRight END;
END ReadString;

PROCEDURE [1] TextWrite(w: SYSTEM.ADDRESS; s-: ARRAY OF CHAR; len: LONGINT);
  VAR f: TextFile; new: env.String; i,pos: LONGINT;
BEGIN
  f:=SYSTEM.VAL(TextFile,w);
  IF f.out=NIL THEN
    NEW(f.out,256+len); f.out[0]:=0X;
  ELSIF LENGTH(f.out^) + len >= LEN(f.out^) THEN
    NEW(new,LENGTH(f^.out^)+len+256);
    COPY(f.out^,new^); f.out:=new
  END;
  i:=LENGTH(f.out^);
  pos:=0;
  WHILE len > 0 DO
    f.out[i]:=s[pos]; DEC(len); INC(pos); INC(i);
  END;
  f.out[i]:=0X;
END TextWrite;

PROCEDURE (f: TextFile) print(format-: ARRAY OF CHAR; SEQ args: SYSTEM.BYTE);
BEGIN
  FormOut.format(f,TextWrite,format,FormOut.default,SYSTEM.ADR(args),SIZE(args));
END print;

PROCEDURE (f: TextFile) Close;
END Close;

(*--------------------------------------------------*)

PROCEDURE ExecCmd(cmd: ARRAY OF CHAR; overlay: BOOLEAN; VAR exit: SYSTEM.CARD): BOOLEAN;
  VAR
    i,j,k : LONGINT;  -- SYSTEM.CARD32;
    pgm   : xfs.String;
    fbf   : xfs.FNAME;
BEGIN
  i := 0; j := 0;
  WHILE (0X < cmd[j]) & (cmd[j]<=' ') DO INC(j) END;
  WHILE cmd[j] > ' ' DO
    IF cmd[j] = '"' THEN
      k:= j;
      REPEAT INC(k) UNTIL (cmd[k] = 0X) OR (cmd[k] = '"');
      IF cmd[k] = '"' THEN
        WHILE j < k DO
          fbf [i] := cmd[j]; INC(i); INC(j)
        END;
      END;
    END;
    fbf [i] := cmd[j]; INC(i); INC(j)
  END;
  fbf [i] := 0X;
  Strings.Delete(cmd, 0, j);
  xfs.sys.SearchPath(fbf, pgm);
  xfs.sys.ConvertToHost(pgm^, pgm);
  RETURN ProgExec.Execute(pgm^,cmd,exit)
END ExecCmd;

PROCEDURE Execute*(p: mk.Project; template: env.String; overlay: BOOLEAN);
  VAR
    f      : TextFile;
    err    : BOOLEAN;
    res    : SYSTEM.CARD;
BEGIN
  NEW(f);
  f.ip:=0;
  f.inp:=template;
  f.out:=NIL;
  f.name:=null;
  err:=Parse(p,f,f,FALSE,TRUE);
  IF err OR (p.errs#0) OR (f.out=NIL) THEN RETURN END;
  overlay:=overlay & ~env.config.Option(IDEOPTION);
  IF ExecCmd(f.out^,overlay,res) THEN
    IF NOT (res IN {0..1}) THEN
      env.errors.EnvError(439,res,f.out^); INC(p.errs);
    END
  ELSE
    env.errors.EnvError(447,f.out^); INC(p.errs);
  END;
END Execute;

BEGIN
  NEW(null,1); null[0]:=0X;
END xcF.
