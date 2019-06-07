(* Copyright (c) 1994 xTech Ltd, Russia. All Rights Reserved. *)
<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE ProgEnv; (* VitVit'n'Ned 27-Jun-97. *)

IMPORT  X2C, xlibOS, SYSTEM, xmRTS;

TYPE
  ARGV = POINTER TO ARRAY [0..4*1024-1] OF
         POINTER TO ARRAY [0..4*1024-1] OF CHAR;


PROCEDURE getNumber(): CARDINAL;
BEGIN 
  (* Usually, argc>0 because argv[0] contains a full path to EXE
     being executed ( _main sets them in the state ). If a program contains
     some DLL ( as its part ) then there's a span of run-time when
     <argc,argv> is set in < 0, NIL > state ( namely DLL initialization )
     - return 0 as a number of arguments - VitVit *)

  IF (X2C.X2C_argc = 0) THEN
    RETURN 0;  (* the control has not been in _main yet - call from DLL *) 
  ELSE
    RETURN X2C.X2C_argc - 1; (* without full path parameter *)
  END;
END getNumber;

PROCEDURE GetArgs(): ARGV;
BEGIN
  RETURN SYSTEM.CAST(ARGV,X2C.X2C_argv);
END GetArgs;


PROCEDURE ArgNumber(): CARDINAL;
BEGIN
  RETURN getNumber();
END ArgNumber;

PROCEDURE GetArg(n: CARDINAL; VAR arg: ARRAY OF CHAR);
VAR
  number: CARDINAL;
  args: ARGV;
BEGIN
  number := getNumber();
  IF n >= number THEN arg[0]:=0C
  ELSE 
    args := GetArgs();
    COPY(args^[n+1]^,arg);
  END;
END GetArg;

PROCEDURE ArgLength(n: CARDINAL): CARDINAL;
VAR
  number: CARDINAL;
  args: ARGV;
BEGIN
  number := getNumber();
  IF n >= number THEN RETURN 0
  ELSE 
    args := GetArgs();
    RETURN LENGTH(args^[n+1]^);
  END;
END ArgLength;

(*----------------------------------------------------------------*)

PROCEDURE ProgramName(VAR name: ARRAY OF CHAR);
VAR
  args: ARGV;
BEGIN
  IF (X2C.X2C_argc = 0) THEN
    name[0] := 0C;
  ELSE
    args := GetArgs();
    COPY(args^[0]^,name);
  END; 
END ProgramName;

PROCEDURE ProgramNameLength(): CARDINAL;
VAR
  args: ARGV;
BEGIN
  IF (X2C.X2C_argc = 0) THEN
    RETURN 0;
  ELSE
    args := GetArgs();
    RETURN LENGTH(args^[0]^)
  END;
END ProgramNameLength;

PROCEDURE String(name-: ARRAY OF CHAR; VAR str: ARRAY OF CHAR);
BEGIN
  xlibOS.X2C_EnvString(
    SYSTEM.CAST(xmRTS.X2C_pCHAR,SYSTEM.ADR(name)),
    SYSTEM.CAST(xmRTS.X2C_pCHAR,SYSTEM.ADR(str)),
    HIGH(str)+1);
END String;

PROCEDURE StringLength(name-: ARRAY OF CHAR): CARDINAL;
BEGIN
  RETURN xlibOS.X2C_EnvStringLength(SYSTEM.CAST(xmRTS.X2C_pCHAR,SYSTEM.ADR(name)));
END StringLength;

END ProgEnv.
