<* MAIN + *>
MODULE xdasm;
IMPORT 
  FormOut,
  platform,
  adt, 
  cmdline, 
  objs:= Objects, 
  dasm,
  lstr:= LongStrs,
  file:= H2DFile,
  io:= Printf,
  SYSTEM,
  RTS:= oberonRTS;


<* IF OBJ_FORMAT_XOMF THEN *>  IMPORT readobj := ReadXOMF;  <* END *>
<* IF OBJ_FORMAT_OMF THEN *>   IMPORT readobj := ReadOMF;   <* END *>
<* IF OBJ_FORMAT_ELF THEN *>   IMPORT readobj := ReadELF;   <* END *>
<* IF OBJ_FORMAT_AOUT THEN *>  IMPORT readobj := ReadAOUT;  <* END *>


CONST logo = "XDS Disassembler v1.0 (c) 1997-2001 Excelsior\n";

CONST help =
"Usage:   xdasm [options] files [options]                  \n" +
"Options:                                                  \n" +
"        -l[=list_file]          generate listing file     \n" +
"\n";

VAR lib, lib_mod: BOOLEAN;

(* --------------------------------------------------------------------- *)
PROCEDURE Error (fmt-: ARRAY OF CHAR; SEQ x: SYSTEM.BYTE);
BEGIN
  io.printf(fmt, x);
  HALT;
END Error;

(* --------------------------------------------------------------------- *)
PROCEDURE ProcessWholeLib();
VAR e: adt.Element;
    m: objs.Module;
BEGIN
  objs.Modules.FindFirst(e);
  WHILE e # NIL DO
    m:= e(objs.Module);
    objs.Modules.DeleteCurrent();
    dasm.DisAssembly(m, m.name^);
    m:= NIL;
    RTS.Collect;
    objs.Modules.FindFirst(e);
  END;
  lib:= FALSE;
END ProcessWholeLib;

(* --------------------------------------------------------------------- *)
PROCEDURE ProcessName(name-: ARRAY OF CHAR);
VAR s1, s2, s3: lstr.String;
    m: objs.Module;
    e: adt.Element;
    ne: adt.NamedElement;
BEGIN
  file.SplitName(name, s1, s2, s3);
  m:= NIL;
  IF (s1^ = '') & (s3^ = '') THEN
    IF lib THEN
      adt.NewNamedElement(ne, s2^);
      objs.Modules.Find(ne, e);
      IF e # NIL THEN
        m:= e(objs.Module);
        lib_mod:= TRUE;
      ELSE
        Error("Module %s is not found in library %s\n", name, objs.CurrentModule.lib_name);
      END;
    ELSE
      file.CreateName('', s2^, 'obj', s1);
      objs.Init();
      readobj.ReadObj (s1^);
      lib:= lstr.Length1(objs.CurrentModule.lib_name) > 0;
      lib_mod:= FALSE;
      IF ~lib THEN m:= objs.CurrentModule END;
    END;
  ELSE
    IF lib & ~lib_mod THEN ProcessWholeLib() END;
    IF s3^ = '' THEN
      file.CreateName(s1^, s2^, 'obj', s3);
    ELSE
      lstr.Assign(name, s3);
    END;
    objs.Init();
    readobj.ReadObj (s3^);
    lib:= lstr.Length1(objs.CurrentModule.lib_name) > 0;
    lib_mod:= FALSE;
    IF ~lib THEN m:= objs.CurrentModule END;
  END;
  IF m # NIL THEN dasm.DisAssembly(m, s2^) END;
END ProcessName;


VAR
  e: adt.Element;
BEGIN
  FormOut.LineSeparator(platform.lineSep);
  FormOut.TextSeparator(platform.textSep);

  io.printf(logo);

  lib:= FALSE;
  cmdline.filenames.FindFirst(e);
  IF e = NIL THEN io.printf(help) END;
  WHILE e # NIL DO
    ProcessName(e(adt.NamedElement).name^);
    cmdline.filenames.FindNext(e);
  END;
  IF lib & ~lib_mod THEN ProcessWholeLib() END;
END xdasm.
