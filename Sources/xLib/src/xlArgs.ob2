(** Copyright (c) 1995 xTech Ltd, Russia. All Rights Reserved. *)
(** XDS Librarian. Arguments parser. *)
MODULE xlArgs; (* Hady. Oct 25, 1995 *)

IMPORT
  xlStrings
  ,xlMsg
  ,xlFiles
  ,ProgramArgs
  ,IOResult
  ,CharClass
  ,Strings
  ;


CONST
  MAXOPTION = 16;

  SOFF  = "-";
  SON   = "+";
  SBODY = "=";

  SPREF = "-";         -- default option syntax is -option
  SPREF_LEGACY = "/";  -- old-style, win32 syntax /option

TYPE
  String = xlStrings.String;
  OptName = ARRAY MAXOPTION OF CHAR;

  Opt* = POINTER TO OptDesc;
  OptDesc = RECORD
    name-: OptName;
    body-: String;
    bool-: BOOLEAN;
    next-: Opt;
  END;

  Arg* = POINTER TO ArgDesc;
  ArgDesc = RECORD
    body-: String;
    next-: Arg;
  END;

VAR
  args-: Arg;
  larg : Arg;
  opts : Opt;
  undef: Opt;
  lopt : Opt;


PROCEDURE equ(s0-,s1-: ARRAY OF CHAR): BOOLEAN;
  VAR i: LONGINT; c0,c1: CHAR;
BEGIN
  i:=0;
  LOOP
    IF i>=LEN(s0) THEN c0:=0X ELSE c0:=CAP(s0[i]) END;
    IF i>=LEN(s1) THEN c1:=0X ELSE c1:=CAP(s1[i]) END;
    IF c0#c1 THEN RETURN FALSE END;
    IF c0=0X THEN EXIT END;
    INC(i);
  END;
  RETURN TRUE
END equ;

PROCEDURE cap(VAR s: ARRAY OF CHAR);
  VAR i: LONGINT;
BEGIN
  i:=0;
  WHILE (i<LEN(s)) & (s[i]#0X) DO s[i]:=CAP(s[i]); INC(i) END;
END cap;

PROCEDURE DefineBool*(name-: ARRAY OF CHAR; val: BOOLEAN);
  VAR t,p: Opt;
BEGIN
  t:=opts; p:=NIL;
  WHILE (t#NIL) & ~equ(name,t.name) DO p:=t; t:=t.next END;
  IF t#NIL THEN
    ASSERT(t.body=NIL);
    t.bool:=val;
  ELSE
    NEW(t);
    COPY(name,t.name); cap(t.name);
    t.body:=NIL;
    t.bool:=val;
    t.next:=NIL;
    IF p=NIL THEN
      opts:=t
    ELSE
      p.next:=t
    END;
  END;
END DefineBool;

PROCEDURE DefineStr*(name-: ARRAY OF CHAR; val-: ARRAY OF CHAR);
  VAR t,p: Opt;
BEGIN
  t:=opts; p:=NIL;
  WHILE (t#NIL) & ~equ(name,t.name) DO p:=t; t:=t.next END;
  IF t#NIL THEN
    ASSERT(t.body#NIL);
    t.body:=xlStrings.Make(val);
  ELSE
    NEW(t);
    COPY(name,t.name); cap(t.name);
    t.body:=xlStrings.Make(val);
    t.bool:=FALSE;
    t.next:=NIL;
    IF p=NIL THEN
      opts:=t
    ELSE
      p.next:=t
    END;
  END;
END DefineStr;

PROCEDURE find(name-: ARRAY OF CHAR): Opt;
  VAR t: Opt;
BEGIN
  t:=opts;
  WHILE (t#NIL) & ~equ(name,t.name) DO t:=t.next END;
  RETURN t;
END find;

PROCEDURE Option*(name-: ARRAY OF CHAR): BOOLEAN;
  VAR t: Opt;
BEGIN
  t:=find(name);
  ASSERT(t#NIL);
  ASSERT(t.body=NIL);
  RETURN t.bool;
END Option;

PROCEDURE Equation*(name-: ARRAY OF CHAR): String;
  VAR t: Opt;
BEGIN
  t:=find(name);
  ASSERT(t#NIL);
  ASSERT(t.body#NIL);
  RETURN t.body;
END Equation;


PROCEDURE ParseOption (s-: ARRAY OF CHAR; start :LONGINT; VAR err: BOOLEAN);
  VAR i: LONGINT;
      o: Opt;
BEGIN
  i:=start;
  WHILE (i<LEN(s)) & (s[i]#0X) & (s[i]#SBODY) & (s[i]#SON) & (s[i]#SOFF) DO
    INC(i);
  END;
  IF (i=start) OR (i-1>MAXOPTION) THEN
    xlMsg.error('invalid option syntax "%s"',s);
    err:=TRUE;
    RETURN
  END;
  NEW(o);
  Strings.Extract(s,start,i-1,o.name);
  IF (i<LEN(s)) & (s[i]=SBODY) THEN
    INC(i);
    NEW(o.body,LEN(s)-i+1);
    Strings.Extract(s,i,LEN(o.body^),o.body^);
  ELSE
    o.body:=NIL;
    IF (i<LEN(s)) & (s[i]#0X) THEN o.bool:=(s[i]=SON) ELSE o.bool:=TRUE END;
  END;
  o.next:=NIL;
  IF lopt=NIL THEN undef:=o ELSE lopt.next:=o END;
  lopt:=o;
END ParseOption;


PROCEDURE IsOptionPrefix (ch: CHAR) :BOOLEAN;
BEGIN
  RETURN (ch = SPREF)
         <* IF env_target="x86nt" THEN *> OR (ch = SPREF_LEGACY) <* END *>
         ;
END IsOptionPrefix;


PROCEDURE OneArg(s-: ARRAY OF CHAR);
VAR a: Arg;
BEGIN
  NEW(a);
  a.body:=xlStrings.Make(s);
  a.next:=NIL;
  IF larg=NIL THEN args:=a ELSE larg.next:=a END;
  larg:=a;
END OneArg;

PROCEDURE TakeOptions*(VAR err: BOOLEAN);
  VAR o,n,p,t: Opt;
BEGIN
  p:=NIL; o:=undef;
  WHILE o#NIL DO
    n:=o.next;
    t:=find(o.name);
    IF t#NIL THEN
      IF p=NIL THEN undef:=n ELSE p.next:=n END; (* untie *)
      IF (o.body=NIL) # (t.body=NIL) THEN
        err:=TRUE;
        xlMsg.error('option "%s" invlid value',o.name);
      ELSE
        t.body:=o.body;
        t.bool:=o.bool;
      END;
    ELSE
      p:=o;
    END;
    o:=n;
  END;
END TakeOptions;

PROCEDURE Parse*(cid: xlFiles.ChanId;
                 name-: ARRAY OF CHAR;
                 VAR options :BOOLEAN;
                 VAR err: BOOLEAN);

  VAR buf,w: String; res: IOResult.ReadResults; f,t,i: LONGINT;
BEGIN
  buf:=NIL;
  LOOP
    xlFiles.ReadString(cid,buf,res);
    IF res=IOResult.endOfInput THEN EXIT END;
    IF res#IOResult.allRight THEN
      IF name[0]="" THEN
        xlMsg.error('I/O error reading program arguments');
      ELSE
        xlMsg.error('I/O error reading "%s"',name);
      END;
      err:=TRUE;
      RETURN
    END;
    f:=0;
    LOOP
      WHILE (f<LEN(buf^)) & CharClass.IsWhiteSpace(buf[f]) DO INC(f) END;
      IF (f>=LEN(buf^)) OR (buf[f]=0X) THEN EXIT END;
      t:=f;
      REPEAT INC(t)
      UNTIL (t>=LEN(buf^)) OR (buf[t]=0X) OR CharClass.IsWhiteSpace(buf[t]);
      IF t>f THEN
        NEW(w,t-f+1);
        i:=0;
        WHILE (f<t) DO w[i]:=buf[f]; INC(f); INC(i) END;
        w[i]:=0X;
        IF options & (IsOptionPrefix (w^[0])) THEN
          ParseOption (w^, 1, (* VAR *) err);
        ELSE
          options := FALSE;
          OneArg(w^);
        END;
      END;
    END;
  END;
END Parse;

PROCEDURE Arguments*(VAR err: BOOLEAN);
VAR opts :BOOLEAN;
BEGIN
  err:=FALSE;
  opts:= TRUE;
  WHILE ProgramArgs.IsArgPresent() DO
    Parse(ProgramArgs.ArgChan(),"",opts,err);
    ProgramArgs.NextArg();
  END;
END Arguments;

PROCEDURE DeleteArg*(a: Arg);
  VAR t,p: Arg;
BEGIN
  p:=NIL; t:=args;
  WHILE (t#NIL) & (t#a) DO p:=t; t:=t.next END;
  IF t#NIL THEN
    IF p=NIL THEN args:=t.next ELSE p.next:=t.next END;
  END;
END DeleteArg;

PROCEDURE WarnUnusedOptions*;
  VAR o: Opt;
BEGIN
  o:=undef;
  WHILE o#NIL DO
    xlMsg.warning('option "%s" ignored',o.name);
    o:=o.next;
  END;
END WarnUnusedOptions;

END xlArgs.
