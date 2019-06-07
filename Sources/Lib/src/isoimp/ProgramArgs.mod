(* Copyright (c) xTech 1993,94. All Rights Reserved. *)
(* ATTENTION: C BASED and GEN_386 implementations are textually concatenated *)
(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE ProgramArgs;

IMPORT SYSTEM, IOChan, IOLink, io:=IOConsts, ChanConsts, CharClass;
IMPORT X2C;

TYPE
  Object = IOLink.DeviceTablePtr;
  StrPtr = POINTER TO ARRAY [0..4095] OF CHAR;

VAR
  did: IOLink.DeviceId;
  cid: ChanId;
  arg: CARDINAL;
  pos: CARDINAL;
  len: CARDINAL;
  ptr: StrPtr;
  args: POINTER TO ARRAY [0..4095] OF StrPtr;
  argc: CARDINAL;
  init: BOOLEAN;

(*----------------------------------------------------------------*)

<*$< CHECKINDEX- WOFF301+ *>
PROCEDURE Look(x: Object; VAR ch: CHAR; VAR res: io.ReadResults);
BEGIN
  res:=io.allRight;
  IF pos > len THEN
    IF arg >= argc-1 THEN res:=io.endOfInput;
    ELSE
      INC(arg); ptr:=args^[arg]; pos:=0; len:=LENGTH(ptr^);
      ch:=ptr^[pos];
    END;
  ELSIF pos = len THEN ch:=" ";
  ELSE  ch:=ptr^[pos];
  END;
END Look;
<*$>*>

PROCEDURE Read(x: Object; VAR ch: CHAR; VAR res: io.ReadResults);
BEGIN
  Look(x,ch,res);
  IF res=io.allRight THEN INC(pos) END;
END Read;

PROCEDURE Skip(x: Object);
  VAR ch: CHAR; res: io.ReadResults;
BEGIN
  Read(x,ch,res);
  IF res=io.allRight THEN x^.result:=io.allRight
  ELSE
    IOLink.RAISEdevException(cid,did,IOChan.skipAtEnd,"ProgramArgs.Skip");
  END;
END Skip;

PROCEDURE SkipLook(x: Object; VAR ch: CHAR; VAR res: io.ReadResults);
BEGIN
  Skip(x);
  Look(x,ch,res);
END SkipLook;

PROCEDURE TextRead(x: Object; a: SYSTEM.ADDRESS; n: CARDINAL; VAR locs: CARDINAL);
  VAR p: StrPtr; i: CARDINAL; res: io.ReadResults;
BEGIN
  p:=a; i:=0;
  LOOP
    IF i>=n THEN EXIT END;
    Read(x,p^[i],res);
    IF res#io.allRight THEN EXIT END;
    INC(i);
  END;
  locs:=i;
  IF (n>0) & (i=0) THEN x^.result:=io.endOfInput
  ELSE x^.result:=io.allRight;
  END;
END TextRead;

(*----------------------------------------------------------------*)

<*$< WOFF301+ *>
PROCEDURE look(x: Object; VAR c: CHAR; VAR r: io.ReadResults);
BEGIN
  IOLink.RAISEdevException(x^.cid,x^.did,IOChan.notAvailable,"ProgramArgs.Look");
END look;

PROCEDURE skip(x: Object);
BEGIN
  IOLink.RAISEdevException(x^.cid,x^.did,IOChan.notAvailable,"ProgramArgs.Skip");
END skip;

PROCEDURE read(x: Object; a: SYSTEM.ADDRESS; max: CARDINAL; VAR n: CARDINAL);
BEGIN
  IOLink.RAISEdevException(x^.cid,x^.did,IOChan.notAvailable,"ProgramArgs.Read");
END read;
<*$> *>

(*----------------------------------------------------------------*)

PROCEDURE Ini;
  VAR x: Object;
BEGIN
  args:=SYSTEM.CAST(SYSTEM.ADDRESS,X2C.X2C_argv);
  argc:=VAL(CARDINAL,X2C.X2C_argc);
  x:=IOLink.DeviceTablePtrValue(cid,did,IOChan.notAvailable,"");
  arg:=1;
  IF arg < argc THEN
    x^.flags:=ChanConsts.read+ChanConsts.text;
    x^.doLook:=Look;
    x^.doSkip:=Skip;
    x^.doSkipLook:=SkipLook;
    x^.doTextRead:=TextRead;
    ptr:=args^[arg];
    pos:=0;
    len:=LENGTH(ptr^);
  END;
END Ini;

PROCEDURE ArgChan(): ChanId;
BEGIN
  IF init THEN Ini; init:=FALSE END;
  RETURN cid
END ArgChan;

PROCEDURE IsArgPresent(): BOOLEAN;
  VAR x: Object; ch: CHAR; res: io.ReadResults;
BEGIN
  IF init THEN Ini; init:=FALSE END;
  x:=IOLink.DeviceTablePtrValue(cid,did,IOChan.notAvailable,"");
  Look(x,ch,res);
  WHILE (res=io.allRight) & CharClass.IsWhiteSpace(ch) DO
    Skip(x); Look(x,ch,res);
  END;
  RETURN res=io.allRight
END IsArgPresent;

PROCEDURE NextArg;
  VAR x: Object;
BEGIN
  IF init THEN Ini; init:=FALSE END;
  IF arg >= argc-1 THEN
    x:=IOLink.DeviceTablePtrValue(cid,did,IOChan.notAvailable,"");
    EXCL(x^.flags,ChanConsts.readFlag);
    x^.result:=io.endOfInput;
    x^.doLook:=look;
    x^.doSkip:=skip;
    x^.doSkipLook:=look;
    x^.doTextRead:=read;
  ELSE
<*$< CHECKINDEX- *>
    INC(arg); ptr:=args^[arg]; pos:=0; len:=LENGTH(ptr^);
<*$>*>
  END;
END NextArg;

BEGIN
  init:=TRUE;
  arg:=0; pos:=0; len:=0;
  IOLink.AllocateDeviceId(did);
  IOLink.MakeChan(did,cid);
END ProgramArgs.
