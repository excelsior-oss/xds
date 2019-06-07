(* Copyright (C) 1996-99 XDS Ltd. *)
(* Lib : OS/2 *)

<*+M2ADDTYPES   *>
<*+M2EXTENSIONS *>

IMPLEMENTATION MODULE xtsLib;

IMPORT SYSTEM, Str, O := OS2;



PROCEDURE Delay( t :LONGCARD);
BEGIN
  O.DosSleep( t );
END Delay;


PROCEDURE Speaker( FreqHz, TimeMs :CARDINAL);
BEGIN
  O.DosBeep( FreqHz, TimeMs);
END Speaker;


PROCEDURE Exec ( Path- :ARRAY OF CHAR; Command- :ARRAY OF CHAR; Env :ExecEnvPtr) :CARDINAL;
VAR
  LoadError :ARRAY [0..O.CCHMAXPATH-1] OF CHAR;
  ChildRC   :O.RESULTCODES;
  arg       :ARRAY [0..511] OF CHAR;
  rc        :LONGCARD;
  i, j      :CARDINAL;
BEGIN
  (* args format for OS2.DosExecPgm: exec filename (Path), 0, ' ', command line, 0, 0 *)

  Str.Copy (arg, Path);
  IF LENGTH(arg)+2+LENGTH(Command)+2 > HIGH (arg)+1 THEN RETURN MAX(CARDINAL); END;

  i:= LENGTH(arg)+1;
  arg [i] := ' ';
  FOR j:=0 TO LENGTH( Command)-1 DO
    INC(i);
    arg[i] := Command[j];
  END;
  arg[i+1] := 0C;
  arg[i+2] := 0C;

  rc := O.DosExecPgm(LoadError,       --   Object name buffer
                     SIZE(LoadError), --   Length of object name buffer
                     O.EXEC_SYNC,     --   Asynchronous/Trace flags
                     arg,             --   Argument string
                     Env,             --   Environment string
                     ChildRC,         --   Termination codes
                     Path);           --   Program file name

  IF (rc # O.NO_ERROR)
    THEN RETURN MAX(CARDINAL);
    ELSE RETURN ChildRC.codeResult;
  END;
END Exec ;


PROCEDURE ExecCmd ( command- :ARRAY OF CHAR ) :CARDINAL;
VAR
  ComLine: ARRAY [0..255] OF CHAR;
BEGIN
  Str.Concat( ComLine, ' /C "', command);
  Str.Append(ComLine, ' & EXIT"');
  RETURN Exec ("CMD.EXE", ComLine, NIL);
END ExecCmd ;


PROCEDURE Environment(N :CARDINAL; VAR result :ARRAY OF CHAR);
VAR
  ptib :O.PTIB;
  ppib :O.PPIB;
  scan :POINTER TO CHAR;
  i    :CARDINAL;
BEGIN
  O.DosGetInfoBlocks (ptib, ppib);
  scan := ppib^.pib_pchenv;
  WHILE N > 0 DO
    WHILE scan^ <> 0C DO
      scan := SYSTEM.ADDADR(scan,1);
    END;
    scan := SYSTEM.ADDADR(scan,1);
    IF scan^ = 0C THEN
      COPY("",result);
      RETURN;      
    END;
    DEC(N);
  END;
  i := 0;
  WHILE i < HIGH(result) DO 
    result[i] := scan^;
    IF scan^ = 0C THEN RETURN END;
    scan := SYSTEM.ADDADR(scan,1);
    INC(i);
  END;
  result[i] := 0C;
END Environment ;

BEGIN
END xtsLib.
