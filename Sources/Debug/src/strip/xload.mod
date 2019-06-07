<* storage+ *>

MODULE xLoad;

FROM Printf IMPORT printf;

IMPORT sys := SYSTEM;
IMPORT fn  := FileName;
IMPORT str := Strings;
IMPORT rf  := RndFile;
IMPORT io  := IOChan;
IMPORT xfp := xFilePos;
IMPORT arg := ProgEnv;

IMPORT opt := Options;
IMPORT fil := File;
IMPORT xs  := xStr;

IMPORT kt  := KrnTypes;
IMPORT dt  := DI_Types;
IMPORT bld := DI_Build;
IMPORT tls := DI_Tools;
IMPORT di  := DI_Read;

IMPORT sex := ScanExe;


PROCEDURE GetFileName (VAR full_name: ARRAY OF CHAR): BOOLEAN;
BEGIN
  IF arg.ArgNumber() = 0 THEN
    RETURN FALSE;
  END;
  arg.GetArg(0, full_name);
  RETURN TRUE;
END GetFileName;


PROCEDURE LoadProgram (VAR program_name: ARRAY OF CHAR): BOOLEAN;
VAR
  comp : dt.COMPONENT;
  comno: dt.ComNo;
  name : xs.String;
BEGIN
  fn.GetName (program_name, name);
  str.Append('$', name);
  comp := dt.EmptyComponent;
  bld.AddComponent (comp);
  comno := dt.Components.Count-1;
  RETURN sex.OpenExe(program_name, comno, dt.Components.Components^[comno]) AND
         di.CheckDebugInfoVersion (dt.Components.Components^[comno].EI);
END LoadProgram;


PROCEDURE ProcessDebugInfo (print: BOOLEAN): CARDINAL;
BEGIN
  ASSERT(dt.Components.Count=1);
 <* IF DEFINED(mode) AND (mode="work") THEN *>
  IF print THEN
    opt.DebugOn (opt.Load);
  END;
 <* END *>
  RETURN di.ProcessDebugInfo (0, dt.Components.Components^[0]);
EXCEPT
  RETURN MAX(CARDINAL);
END ProcessDebugInfo;



VAR
  err_code : CARDINAL;
  full_name: xs.String;

BEGIN
  IF GetFileName (full_name) THEN
    IF LoadProgram (full_name) THEN
      printf ("Program %s is successfully loaded.\n", full_name);
      err_code := ProcessDebugInfo (TRUE);
      IF err_code = 0 THEN
        printf ("Debug information is successfully loaded.\n");
      ELSE
        printf ("Error: debug information was not loaded (error code = %d).\n", err_code);
        HALT (3);
      END;
    ELSE
      printf ("Error: program %s was not loaded.\n", full_name);
      HALT (2);
    END;
  ELSE
    printf ("Error: file name is expected.\n");
    HALT (1);
  END;
END xLoad.
