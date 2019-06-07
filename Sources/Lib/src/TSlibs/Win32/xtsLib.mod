(* Copyright (C) 1996-99 XDS Ltd. *)
(*  Lib : Win32 *)


<*+M2ADDTYPES   *>
<*+M2EXTENSIONS *>

IMPLEMENTATION MODULE xtsLib;

IMPORT W := Windows, WinBase, WinDef, SYSTEM;



PROCEDURE Delay( t :LONGCARD);
BEGIN
  W.Sleep( t );
END Delay;

PROCEDURE Speaker( FreqHz, TimeMs :CARDINAL);
BEGIN
  W.Beep( FreqHz, TimeMs);
END Speaker;

PROCEDURE Environment(N :CARDINAL; VAR result :ARRAY OF CHAR);
  TYPE CSTR = POINTER TO ARRAY [0 .. MAX(LONGINT)-1] OF CHAR;
  VAR str: CSTR; pstr: WinDef.PSTR; i, k: CARDINAL;
BEGIN
  pstr := WinBase.GetEnvironmentStringsA();
  str := SYSTEM.CAST(CSTR, pstr);
  i:=0; k:=0;
  WHILE (k#N) & (str^[i]#0C) DO
    WHILE str^[i]#0C DO
      INC(i);
    END;
    INC(k);
    INC(i);
  END;
  IF k=N THEN
    k:=0;
    WHILE (k<HIGH(result)) & (str^[i]#0C) DO
      result[k]:=str^[i];
      INC(k);
      INC(i);
    END;
    result[k]:=0C;
  ELSE
    COPY("",result);
  END;
  WinBase.FreeEnvironmentStringsA(pstr);
END Environment;

BEGIN
END xtsLib.
