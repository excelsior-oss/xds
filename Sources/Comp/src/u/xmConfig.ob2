(** Copyright (c) 1994,97 XDS Ltd, Russia. All Rights Reserved. *)
(** o2/m2 development system v2.0 *)
(** Standard implementation of Config manager *)
<*+ O2EXTENSIONS *>
MODULE xmConfig;
(* Ned 24-Feb-94. *)
(* Sem 09-Sep-95. *)

(* Modifications
  16.06.96 Ned  Parse: change syntax for synonyms and declaring equations.
*)

IMPORT
  env:=xiEnv,
  CharClass,
  DStrings,
  Strings,
  TimeConv;
IMPORT SYSTEM;
CONST
  equTag  = '#';
  onTag   = '+';
  offTag  = '-';
  dclTag  = ':';
  comment = '%';

TYPE
  String = env.String;

  EquItem = POINTER TO EquItemDesc;
  EquItemDesc = RECORD
    val  : String;
    next : EquItem;
    level: INTEGER;
  END;

  Equ = POINTER TO EquDesc;
  EquDesc = RECORD
    name  : String;
    next  : Equ;
    item  : EquItem;
    action: env.EQ_ACTION;
    level : INTEGER;
  END;

  OptItem = POINTER TO OptItemDesc;
  OptItemDesc = RECORD
    val  : BOOLEAN;
    level: INTEGER;
    next : OptItem;
  END;

  Opt = POINTER TO OptDesc;
  OptDesc = RECORD
    name : String;
    next : Opt;
    item : OptItem;
    ref  : Opt;
    action: env.OP_ACTION;
    level: INTEGER;
    bit  : env.CompilerOption;
  END;

  OptPos = POINTER TO OptPosDesc;
  OptPosDesc = RECORD
    opt  : Opt;
    pos  : env.TPOS;
    val  : BOOLEAN;
    next : OptPos;
  END;

  StackItem = POINTER TO StackItemDesc;
  StackItemDesc = RECORD
    tags : env.CompilerOptionSet;
    next : StackItem;
  END;

  Config = POINTER TO ConfigDesc;
  ConfigDesc = RECORD (env.ConfigDesc)
    level: INTEGER; (* state level *)
    opts : Opt;
    equs : Equ;
    stack: StackItem;
    pos  : OptPos;
  END;

VAR
  warning_hook*: PROCEDURE (s-: ARRAY OF CHAR; value: BOOLEAN);


(*----------------------------------------------------------------*)

PROCEDURE FindEqu(c: Config; nm-: ARRAY OF CHAR; VAR n: Equ);
  VAR l: Equ; name: env.OPNAME;
BEGIN
  COPY(nm,name);
  Strings.Capitalize(name);
  l:=c.equs;
  WHILE l#NIL DO
    IF l.name^=name THEN
      n:=l;
      IF l.item=NIL THEN c.lev:=-1;
      ELSE c.lev:=l.item.level;
      END;
      RETURN;
    END;
    l:=l.next;
  END;
  n:=NIL;
END FindEqu;

PROCEDURE NewEqu(c: Config; VAR n: Equ; name-: ARRAY OF CHAR; lev: INTEGER);
BEGIN
  NEW(n);
  DStrings.Assign(name,n.name);
  Strings.Capitalize(n.name^);
  n.level:=lev;
  n.action:=NIL;
  n.item:=NIL;
  n.next:=c.equs; c.equs:=n;
END NewEqu;

PROCEDURE (c: Config) NewEquation(name-: ARRAY OF CHAR);
  VAR n: Equ;
BEGIN
  FindEqu(c,name,n);
  IF n#NIL THEN
    c.res:=env.definedEquation;
  ELSE
    NewEqu(c,n,name,c.level);
    c.res:=env.ok;
  END;
END NewEquation;

PROCEDURE (c: Config) NewActiveEquation(name- : ARRAY OF CHAR;
                                        action: env.EQ_ACTION);
  VAR n: Equ; sNIL: env.String;
BEGIN
  sNIL:= NIL;
  FindEqu(c,name,n);
  IF n#NIL THEN
    c.res:=env.definedEquation;
  ELSE
    NewEqu(c,n,name,c.level);
    n.action:=action;
    IF action # NIL THEN action(sNIL) END;
    c.res:=env.ok;
  END;
END NewActiveEquation;

PROCEDURE (c: Config) SetEquation(name-,val-: ARRAY OF CHAR);
  VAR n: Equ; i: EquItem;
BEGIN
  FindEqu(c,name,n);
  IF n=NIL THEN
    c.res:=env.unknownEquation;
    NewEqu(c,n,name,c.level);
  ELSE
    c.res:=env.ok;
  END;
  NEW(i);
  DStrings.Assign(val,i.val);
  i.level:=c.level;
  i.next:=n.item; n.item:=i;
  IF n.action # NIL THEN n.action(i.val) END;
END SetEquation;

PROCEDURE (c: Config) Equation(name-: ARRAY OF CHAR; VAR val: String);
  VAR n: Equ;
BEGIN
  FindEqu(c,name,n);
  IF (n=NIL) OR (n.item=NIL) THEN val:=NIL
  ELSE val:=n.item(EquItem).val;
  END;
  IF n=NIL THEN c.res:=env.unknownEquation ELSE c.res:=env.ok END;
END Equation;

PROCEDURE (c: Config) ListEquations(VAR iter: env.Equations;
                                       name-: ARRAY OF CHAR;
                                         all: BOOLEAN);

  PROCEDURE iterate(n: Equ): BOOLEAN;
    VAR name: String; l: EquItem;
  BEGIN
    l:=n.item;
    IF l=NIL THEN
      RETURN iter.Do(n.name^,NIL)
    ELSE
      name:=n.name;
      WHILE (l#NIL) & (all OR (l.level=c.level)) DO
        IF ~ iter.Do(name^,l.val) THEN RETURN FALSE END;
        l:=l.next
      END;
      RETURN TRUE
    END;
  END iterate;

  VAR l: Equ;
BEGIN
  IF name[0]=0X THEN
    l:=c.equs;
    WHILE (l#NIL) & (all OR (l.level=c.level)) & iterate(l) DO l:=l.next END;
    c.res:=env.ok;
  ELSE
    FindEqu(c,name,l);
    IF (l#NIL) & (all OR (l.level=c.level)) & iterate(l) THEN END;
    IF l=NIL THEN c.res:=env.unknownEquation ELSE c.res:=env.ok END;
  END;
END ListEquations;

PROCEDURE (c: Config) GetTimeStamp(): env.TimeStamp;
BEGIN
  RETURN TimeConv.time()
END GetTimeStamp;

(*----------------------------------------------------------------*)

PROCEDURE IsWOFF(name-: ARRAY OF CHAR): BOOLEAN;
  CONST woff = "WOFF";
  VAR i: INTEGER;
BEGIN
  IF LENGTH(name) < LENGTH(woff) THEN RETURN FALSE END;
  i:=0;
  WHILE i < LENGTH(woff) DO
    IF name[i]#woff[i] THEN RETURN FALSE END;
    INC(i)
  END;
  WHILE i < LENGTH(name) DO
    IF ~ CharClass.IsNumeric(name[i]) THEN RETURN FALSE END;
    INC(i)
  END;
  IF warning_hook # NIL THEN warning_hook (name, FALSE); END;
  RETURN TRUE
END IsWOFF;

PROCEDURE IsWON(name-: ARRAY OF CHAR): BOOLEAN;
  CONST woff = "WON";
  VAR i: INTEGER;
BEGIN
  IF LENGTH(name) < LENGTH(woff) THEN RETURN FALSE END;
  i:=0;
  WHILE i < LENGTH(woff) DO
    IF name[i]#woff[i] THEN RETURN FALSE END;
    INC(i)
  END;
  WHILE i < LENGTH(name) DO
    IF ~ CharClass.IsNumeric(name[i]) THEN RETURN FALSE END;
    INC(i)
  END;
  IF warning_hook # NIL THEN warning_hook (name, TRUE); END;
  RETURN TRUE
END IsWON;

-- AVY: message as error
PROCEDURE IsWERR (name-: ARRAY OF CHAR): BOOLEAN;
CONST
  werr = "WERR";
VAR
  i: INTEGER;
BEGIN
  IF LENGTH(name) < LENGTH(werr) THEN
    RETURN FALSE;
  END;
  i := 0;
  WHILE i < LENGTH(werr) DO
    IF name[i] # werr[i] THEN
      RETURN FALSE;
    END;
    INC(i)
  END;
  WHILE i < LENGTH(name) DO
    IF NOT CharClass.IsNumeric(name[i]) THEN
      RETURN FALSE;
    END;
    INC(i)
  END;
  IF warning_hook # NIL THEN
    warning_hook (name, FALSE);
  END;
  RETURN TRUE
END IsWERR;


PROCEDURE NewOpt(c: Config; VAR n: Opt;
                 name-: ARRAY OF CHAR;
                 lev: INTEGER;
                 bit: env.CompilerOption);
BEGIN
  NEW(n);
  DStrings.Assign(name,n.name);
  Strings.Capitalize(n.name^);
  n.level:=lev;
  n.item:=NIL;
  n.action:=NIL;
  n.ref:=n;
  IF VAL(SHORTINT,bit) < 0 THEN n.bit:=SYSTEM.VAL(env.CompilerOption,-1);
  ELSE
    ASSERT(VAL(SHORTINT,bit)<MAX(SET));
    n.bit:=bit;
  END;
  n.next:=c.opts; c.opts:=n;
END NewOpt;

PROCEDURE FindOpt(c: Config; nm-: ARRAY OF CHAR; VAR n: Opt);
  VAR l: Opt; name: env.OPNAME;
BEGIN
  COPY(nm,name);
  Strings.Capitalize(name);
  l:=c.opts;
  WHILE l#NIL DO
    IF l.name^=name THEN
      IF l.item=NIL THEN c.lev:=-1;
      ELSE c.lev:=l.item.level;
      END;
      n:=l;
      RETURN
    END;
    l:=l.next;
  END;
  n:=NIL;
END FindOpt;

PROCEDURE FindCreate(c: Config; name-: ARRAY OF CHAR; VAR o: Opt);
BEGIN
  FindOpt(c,name,o);
  IF o=NIL THEN
    NewOpt(c,o,name,c.level,SYSTEM.VAL(env.CompilerOption,-1));
    IF ~ IsWOFF(name) & ~ IsWON(name) & ~IsWERR(name) THEN
      c.res:=env.unknownOption
    ELSE
      c.res:=env.ok;
      o.level:=0;
    END;
  ELSE
    c.res:=env.ok;
  END;
  o:=o.ref;
END FindCreate;

PROCEDURE (c: Config) NewOption(name-: ARRAY OF CHAR;
                                 val: BOOLEAN;
                                 bit: env.CompilerOption);
  VAR n: Opt;
BEGIN
  FindOpt(c,name,n);
  IF n#NIL THEN
    c.SetOption(name,val);
    c.res:=env.definedOption;
  ELSE
    NewOpt(c,n,name,c.level,bit);
    NEW(n.item);
    n.item.val:=val;
    n.item.next:=NIL;
    IF val & (VAL(SHORTINT,bit)>=0) THEN INCL(c.tags,bit) END;
    IF IsWOFF(name) OR IsWON(name) OR IsWERR(name)THEN
      n.item.level:=0;
      n.level:=0;
      c.res:=env.definedOption;
    ELSE
      n.item.level:=c.level;
      c.res:=env.ok;
    END;
  END;
END NewOption;

PROCEDURE (c: Config) NewActiveOption(name-: ARRAY OF CHAR;
                                        val: BOOLEAN;
                                     action: env.OP_ACTION);
  VAR n: Opt;
BEGIN
  FindOpt(c,name,n);
  IF n#NIL THEN
    c.SetOption(name,val);
    c.res:=env.definedOption;
  ELSE
    NewOpt(c,n,name,c.level,SYSTEM.VAL(env.CompilerOption,-1));
    n.action:=action;
    IF action # NIL THEN action(val) END;
    NEW(n.item);
    n.item.val:=val;
    n.item.next:=NIL;
    IF IsWOFF(name) OR IsWON(name) OR IsWERR(name) THEN
      n.item.level:=0;
      n.level:=0;
      c.res:=env.definedOption;
    ELSE
      n.item.level:=c.level;
      c.res:=env.ok;
    END;
  END;
END NewActiveOption;

PROCEDURE (c: Config) set_option(n: Opt; val: BOOLEAN);
  VAR i: OptItem;
BEGIN
  IF (((n.item#NIL) & n.item.val) = val) &
      ((n.item#NIL) & (n.item.level = c.level)) THEN
    RETURN
  END;
  IF (n.item=NIL) OR (n.item.level # c.level) THEN
    NEW(i);
    i.val:=val;
    i.next:=n.item;
    i.level:=c.level;
    n.item:=i;
  ELSE
    n.item.val:=val;
  END;
  IF VAL(SHORTINT,n.bit)>=0 THEN
    IF val THEN INCL(c.tags,n.bit) ELSE EXCL(c.tags,n.bit) END;
  END;
  IF n.action # NIL THEN n.action(val) END;
END set_option;

PROCEDURE (c: Config) SetOption(name-: ARRAY OF CHAR; val: BOOLEAN);
  VAR o: Opt;
BEGIN
  FindCreate(c,name,o);
  c.set_option(o,val);
END SetOption;

PROCEDURE (c: Config) chngOption(ps: env.TPOS; o: Opt; fr,to: BOOLEAN);
  VAR l,p: OptPos;
BEGIN
  IF (fr=to) OR ps.IsNull() THEN RETURN END;
  NEW(p);
  p.pos:=ps;
  p.opt:=o.ref;
  p.val:=fr;
  IF (c.pos=NIL) OR c.pos.pos.lss(ps) THEN
    p.next:=c.pos; c.pos:=p;
  ELSE
    l:=c.pos;
    WHILE (l.next#NIL) & l.next.pos.gtr(ps) DO l:=l.next END;
    p.next:=l.next; l.next:=p;
  END;
END chngOption;

PROCEDURE (c: Config) SetOptionAt(ps: env.TPOS;
                                  name-: ARRAY OF CHAR;
                                  val: BOOLEAN);
  VAR o: Opt;
BEGIN
  FindCreate(c,name,o);
  c.chngOption(ps,o,(o.item#NIL) & o.item.val,val);
  c.set_option(o,val);
END SetOptionAt;

PROCEDURE (c: Config) Option(name-: ARRAY OF CHAR): BOOLEAN;
  VAR n: Opt;
BEGIN
  FindOpt(c,name,n);
  IF n=NIL THEN
    IF ~ IsWOFF(name) & ~ IsWON(name) & ~IsWERR(name) THEN c.res:=env.unknownOption;
    ELSE c.res:=env.ok;
    END;
    RETURN FALSE;
  ELSE
    c.res:=env.ok;
    n:=n.ref;
    RETURN (n.item#NIL) & n.item.val;
  END;
END Option;

PROCEDURE (c: Config) OptionAt(ps: env.TPOS; name-: ARRAY OF CHAR): BOOLEAN;
  VAR n: Opt; v: BOOLEAN; p: OptPos;
BEGIN
  FindOpt(c,name,n);
  IF n=NIL THEN
    IF ~ IsWOFF(name) & ~ IsWON(name) & ~IsWERR(name) THEN c.res:=env.unknownOption;
    ELSE c.res:=env.ok;
    END;
    RETURN FALSE;
  ELSE
    c.res:=env.ok;
    n:=n.ref;
    v:=(n.item#NIL) & n.item.val;
  END;
  p:=c.pos;
  WHILE (p#NIL) & p.pos.gtr(ps) DO
    IF p.opt=n THEN v:=p.val END;
    p:=p.next;
  END;
  RETURN v;
END OptionAt;

PROCEDURE (c: Config) ListOptions(VAR iter: env.Options; all: BOOLEAN);
  VAR l,o: Opt; val: BOOLEAN;
BEGIN
  l:=c.opts;
  WHILE (l#NIL) & (all OR (l.level=c.level)) DO
    o:=l.ref;
    val:=(o.item#NIL) & o.item.val;
    IF (l^.name[0] # ' ') & ~ iter.Do(l.name^,val) THEN RETURN END;
    l:=l.next;
  END;
  c.res:=env.ok;
END ListOptions;

PROCEDURE (c: Config) Synonym(old-,new-: ARRAY OF CHAR);
  VAR o,n: Opt;
BEGIN
  FindOpt(c,old,o);
  IF o=NIL THEN
    IF IsWOFF(old) OR IsWON(old) OR IsWERR(old) THEN
      c.NewOption(old,FALSE,SYSTEM.VAL(env.CompilerOption,-1));
      FindOpt(c,old,o);
      c.res:=env.ok;
    ELSE
      c.res:=env.unknownOption
    END;
  END;
  IF o#NIL THEN
    FindOpt(c,new,n);
    IF n#NIL THEN
      c.res:=env.definedOption
    ELSE
      NewOpt(c,n,new,c.level,SYSTEM.VAL(env.CompilerOption,-1));
      n.ref:=o;
      c.res:=env.ok;
    END;
  END;
END Synonym;

(*----------------------------------------------------------------*)

PROCEDURE (c: Config) Save;
  VAR p: StackItem;
BEGIN
  INC(c.level);
  NEW(p); p.tags:=c.tags;
  p.next:=c.stack; c.stack:=p;
END Save;

PROCEDURE (c: Config) removeOpts(ps: env.TPOS);
  VAR l,p: Opt; i: OptItem; b: BOOLEAN;
BEGIN
  l:=c.opts; p:=NIL;
  WHILE l#NIL DO
    IF l.level>=c.level THEN
      IF l.action # NIL THEN l.action(FALSE) END;
      l:=l.next;
      IF p=NIL THEN c.opts:=l ELSE p.next:=l END;
    ELSE
      b:=(l.item#NIL) & l.item.val;
      i:=l.item;
      WHILE (i#NIL) & (i.level>=c.level) DO i:=i.next END;
      IF (l.action # NIL) & (l.item # i) THEN
        IF i = NIL THEN l.action(FALSE) ELSE l.action(i.val) END;
      END;
      l.item:=i;
      c.chngOption(ps,l,b,(l.item#NIL) & l.item.val);
      p:=l; l:=l.next;
    END;
  END;
END removeOpts;

PROCEDURE (c: Config) removeEqus;
  VAR l,p: Equ; i: EquItem; sNIL: env.String;
BEGIN
  l:=c.equs; p:=NIL;
  WHILE l#NIL DO
    IF l.level>=c.level THEN
      IF l.action # NIL THEN l.action(sNIL) END;
      l:=l.next;
      IF p=NIL THEN c.equs:=l ELSE p.next:=l END;
    ELSE
      i:=l.item;
      WHILE (i#NIL) & (i.level>=c.level) DO i:=i.next END;
      IF (l.action # NIL) & (l.item # i) THEN
        IF i = NIL THEN l.action(sNIL) ELSE l.action(i.val) END;
      END;
      l.item:=i;
      p:=l; l:=l.next;
    END;
  END;
END removeEqus;

PROCEDURE (c: Config) Restore;
BEGIN
  ASSERT(c.level>0);
  c.removeOpts(env.null_pos);
  c.removeEqus;
  c.tags:=c.stack.tags;
  c.stack:=c.stack.next;
  DEC(c.level);
END Restore;

PROCEDURE (c: Config) RestoreAt(ps: env.TPOS);
BEGIN
  ASSERT(c.level>0);
  c.removeOpts(ps);
  c.removeEqus;
  c.tags:=c.stack.tags;
  c.stack:=c.stack.next;
  DEC(c.level);
END RestoreAt;

PROCEDURE (c: Config) Level(): INTEGER;
BEGIN
  RETURN c.level
END Level;

PROCEDURE (c: Config) ClearPos;
BEGIN
  c.pos:=NIL;
END ClearPos;

(*----------------------------------------------------------------*)

PROCEDURE Skip(s-: ARRAY OF CHAR; VAR p: LONGINT);
BEGIN
  WHILE (p<LEN(s)) & CharClass.IsWhiteSpace(s[p]) DO INC(p) END;
END Skip;

PROCEDURE (c: Config) IsValidTag(str-: ARRAY OF CHAR): BOOLEAN;
  VAR p: LONGINT; ch: CHAR;
BEGIN
  p:=0; Skip(str,p);
  IF p >= LEN(str) THEN RETURN TRUE END;
  ch:=str[p];
  RETURN (ch=onTag) OR (ch=offTag) OR (ch=dclTag) OR (ch=equTag)
END IsValidTag;

PROCEDURE (c: Config) Parse(s-: ARRAY OF CHAR; VAR name: String);

  PROCEDURE GetWord(s-: ARRAY OF CHAR;
                VAR p: LONGINT;
                VAR w: ARRAY OF CHAR);
    VAR i,len: LONGINT; ch: CHAR;
  BEGIN
    Skip(s,p);
    i:=0; len:=LENGTH(s);
    LOOP
      IF p>=len THEN EXIT END;
      ch:=s[p];
      IF CharClass.IsLetter(ch) OR CharClass.IsNumeric(ch) OR (ch='_') THEN
        IF i < LEN(w) THEN w[i]:=ch; INC(i) END;
      ELSE EXIT
      END;
      INC(p);
    END;
    IF i < LEN(w) THEN w[i]:=0X END;
  END GetWord;

  PROCEDURE equation(s-: ARRAY OF CHAR; VAR p: LONGINT);
    VAR i,len: LONGINT; w,val0: env.OPNAME; val1: String;
  BEGIN
    GetWord(s,p,w);
    IF w[0]=0X THEN
      c.res:=env.wrongSyntax
    ELSE
      Strings.Capitalize(w); DStrings.Assign(w,name);
      Skip(s,p);
      len:=LENGTH(s);
      IF (p < len) & (s[p] = ':') THEN
        INC(p);
        IF (p < len) & (s[p] = '=') THEN
          INC(p); Skip(s,p);
          c.NewEquation(w);
          IF c.res#env.ok THEN RETURN END;
        END;
      ELSIF (p < len) & (s[p] = '=') THEN
        INC(p); Skip(s,p);
      ELSIF (p < len) & (s[p] = '!') THEN
        INC(p); Skip(s,p);
        c.NewEquation(w);
        IF (c.res#env.ok) & (c.res#env.definedEquation) THEN RETURN END;
      END;

      i:=len-1;
      WHILE (i>=p) & CharClass.IsWhiteSpace(s[i]) DO DEC(i) END;
      IF i-p < LEN(val0) THEN
        Strings.Extract(s,p,i+1-p,val0);
        c.SetEquation(w,val0);
      ELSE
        NEW(val1,i-p+1);
        Strings.Extract(s,p,i+1-p,val1^);
        c.SetEquation(w,val1^);
      END;
    END;
  END equation;

  PROCEDURE option(s-: ARRAY OF CHAR; VAR p: LONGINT; tag: CHAR);
    VAR w,o: env.OPNAME; syn,val: BOOLEAN; ch: CHAR; fr: LONGINT;
  BEGIN
    fr:=p;
    GetWord(s,p,w);
    IF w[0]=0X THEN c.res:=env.wrongSyntax;
    ELSE
      Strings.Capitalize(w); DStrings.Assign(w,name);
      Skip(s,p);
      IF tag = '-' THEN
	ch:=s[p];
	IF    ch='-' THEN tag:=offTag; INC(p);
	ELSIF ch='+' THEN tag:=onTag; INC(p);
	ELSIF ch=':' THEN tag:=dclTag; INC(p);
          IF s[p] = '=' THEN (* dcl equation *)
            p:=fr; equation(s,p);
            IF c.res=env.ok THEN c.res:=env.isEquation END;
            RETURN;
          END;
	ELSIF (ch='=') OR (ch='!') THEN
	  p:=fr; equation(s,p);
	  IF c.res=env.ok THEN c.res:=env.isEquation END;
	  RETURN;
	END;
      END;
      syn:=FALSE; o:="";
      IF tag = dclTag THEN
	val:=FALSE;
	IF p < LEN(s) THEN
	  ch:=s[p];
	  IF (ch = '+') OR (ch = '-') THEN
	    val:=(ch='+'); INC(p); Skip(s,p);
          ELSIF ch = ':' THEN   (* Ned: was '=' *)
	    syn:=TRUE;
	    INC(p); GetWord(s,p,o);
	    IF o[0]=0X THEN c.res:=env.wrongSyntax; RETURN END;
	  END;
	END;
      ELSE
	val:=(tag=onTag);
      END;
      IF (p < LEN(s)) & ((s[p]#0X) & (s[p]#comment)) THEN
	c.res:=env.wrongSyntax;
      ELSIF syn THEN c.Synonym(o,w)
      ELSIF tag=dclTag THEN c.NewOption(w,val,SYSTEM.VAL(env.CompilerOption,-1))
      ELSE c.SetOption(w,val);
      END;
    END;
  END option;

  VAR ch: CHAR; p: LONGINT;
BEGIN
  c.res:=env.ok;
  p:=0;
  Skip(s,p);
  IF p < LEN(s) THEN
    ch:=s[p]; INC(p);
    IF (ch=onTag) OR (ch=offTag) OR (ch=dclTag) THEN
      option(s,p,ch);
      IF c.res=env.ok THEN c.res:=env.isOption END;
    ELSIF ch=equTag THEN
      equation(s,p);
      IF c.res=env.ok THEN c.res:=env.isEquation END;
    ELSIF (ch#0X) & (ch#comment) THEN
      c.res:=env.wrongSyntax
    END;
  END;
END Parse;

(*----------------------------------------------------------------*)

PROCEDURE SetManagers*;
  VAR c: Config;
BEGIN
  NEW(c);
  c.tags:=env.CompilerOptionSet{};
  c.level:=0;
  c.res  :=env.ok;
  c.stack:=NIL;
  c.opts:=NIL;
  c.equs:=NIL;
  env.config:=c;
END SetManagers;

BEGIN
  warning_hook := NIL;
END xmConfig.
