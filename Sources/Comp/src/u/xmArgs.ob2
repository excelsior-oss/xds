(** Copyright (c) 1994,97 XDS Ltd, Russia. All Rights Reserved. *)
(** o2/m2 development system v2.0. *)
(** Concrete interface: program arguments *)
MODULE xmArgs; (* Ned 05-Mar-94. *)

IMPORT
   DStrings
  ,ProgEnv
  ,env:=xiEnv
  ,xfs:=xiFiles
  ;

TYPE
  Arg = POINTER TO ArgDesc;
  ArgDesc = RECORD
    str : DStrings.String;
    next: Arg;
  END;
  Args = POINTER TO ArgsDesc;
  ArgsDesc = RECORD (env.ArgsDesc);
    number: INTEGER;
    list  : Arg;
    parsed: BOOLEAN;
  END;

VAR
  ProgName: xfs.String;

PROCEDURE (a: Args) Number(): INTEGER;
BEGIN
  RETURN a.number
END Number;

PROCEDURE (a: Args) GetArg(i: INTEGER; VAR s: DStrings.String);
  VAR l: Arg;
BEGIN
  ASSERT((i>=0) & (i<a.number));
  l:=a.list;
  WHILE i>0 DO l:=l.next; ASSERT(l#NIL); DEC(i) END;
  DStrings.Assign(l.str^,s);
END GetArg;

PROCEDURE (a: Args) DeleteArg(i: INTEGER);
  VAR l: Arg;
BEGIN
  ASSERT((i>=0) & (i<a.number));
  IF i=0 THEN ASSERT(a.list#NIL); a.list:=a.list.next
  ELSE
    l:=a.list;
    WHILE i>1 DO l:=l.next; ASSERT(l#NIL); DEC(i) END;
    ASSERT(l.next#NIL);
    l.next:=l.next.next;
  END;
  DEC(a.number);
END DeleteArg;

PROCEDURE (a: Args) ProgramName(VAR fnm: DStrings.String);
  VAR a0: xfs.String;
BEGIN
  IF ProgName#NIL THEN DStrings.Assign(ProgName^,fnm); RETURN END;
  NEW(a0,ProgEnv.ProgramNameLength()+3); (* "+3" is because "xm" string may be copied to it *)
  ProgEnv.ProgramName(a0^);
  IF a0[0]=0X THEN COPY("xm",a0^) END;
  xfs.sys.ConvertFromHost(a0^,a0);
  xfs.sys.SearchPath(a0^,fnm);
  DStrings.Assign(fnm^,ProgName);
END ProgramName;

PROCEDURE (a: Args) EnvString(name-: ARRAY OF CHAR; VAR s: env.String);
BEGIN
  NEW(s,ProgEnv.StringLength(name)+1);
  ProgEnv.String(name,s^);
END EnvString;

PROCEDURE (a: Args) Parse;
  VAR i: INTEGER; x,tail: Arg;
BEGIN
  IF a.parsed THEN RETURN END;
  a.parsed:=TRUE;
  a.number:=VAL(INTEGER,ProgEnv.ArgNumber());
  a.list:=NIL;
  tail:=NIL;
  FOR i:=0 TO a.number-1 DO
    NEW(x);
    NEW(x.str,ProgEnv.ArgLength(i)+1);
    ProgEnv.GetArg(i,x.str^);
    IF tail=NIL THEN a.list:=x ELSE tail.next:=x END;
    tail:=x;
  END;
END Parse;

PROCEDURE SetManagers*;
  VAR args: Args;
BEGIN
  NEW(args);
  args.parsed:=FALSE;
  args.Parse;
  env.args:=args;
  ProgName:=NIL;
END SetManagers;

END xmArgs.
