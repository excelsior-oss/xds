<* storage+ *>

IMPLEMENTATION MODULE Header;

IMPORT sys := SYSTEM;
IMPORT rio := RawIO;
IMPORT rf  := RndFile;
IMPORT xfp := xFilePos;
IMPORT arg := ProgEnv;
IMPORT fn  := FileName;
IMPORT str := Strings;


PROCEDURE GetFileName (VAR full_name: ARRAY OF CHAR): BOOLEAN;
BEGIN
  IF arg.ArgNumber() = 0 THEN
    RETURN FALSE;
  END;
  arg.GetArg(0, full_name);
  RETURN TRUE;
END GetFileName;


PROCEDURE CreateFileName (ext-: ARRAY OF CHAR; VAR full_name: ARRAY OF CHAR);
VAR
  dir, name: STR;
BEGIN
  fn.GetDir (full_name, dir);
  fn.GetName (full_name, name);
  fn.Create (dir, name, ext, full_name);
END CreateFileName;


PROCEDURE SaveHeader (VAR full_name: ARRAY OF CHAR): BOOLEAN;
VAR
  file: rf.ChanId;
  res : rf.OpenResults;

  PROCEDURE fseek (pos: CARDINAL);
  VAR
    fp: rf.FilePos;
  BEGIN
    xfp.CardToPos (fp, pos);
    rf.SetPos (file, fp);
  END fseek;

VAR
  offs: CARDINAL;
  i   : CARDINAL;

BEGIN
  CreateFileName (EXT_EXE, full_name);
  rf.OpenOld(file, full_name, rf.raw+rf.read, res);
  IF res # rf.opened THEN
    RETURN FALSE;
  END;
  fseek(03CH);
  rio.Read (file, offs);
  fseek (offs);
  rio.Read(file, Header);
  IF (Header.signature # 'LX') THEN
    RETURN FALSE;
  END;
  NEW (Objects, Header.objinmod);
  fseek(offs+Header.objtoffs);
  FOR i := 0 TO Header.objinmod-1 DO
    rio.Read(file, Objects^[i]);
  END;
  rf.Close(file);
  RETURN TRUE;
END SaveHeader;


PROCEDURE WriteHeader (VAR full_name: ARRAY OF CHAR): BOOLEAN;
VAR
  file: rf.ChanId;
  res : rf.OpenResults;
  i   : CARDINAL;
BEGIN
  CreateFileName (EXT_HDR, full_name);
  rf.OpenClean (file, full_name, rf.raw+rf.write+rf.old, res);
  IF res # rf.opened THEN
    RETURN FALSE;
  END;
  rio.Write(file, Header);
  FOR i := 0 TO Header.objinmod-1 DO
    rio.Write(file, Objects^[i]);
  END;
  rf.Close(file);
  RETURN TRUE;
END WriteHeader;


PROCEDURE ReadHeader (VAR full_name: ARRAY OF CHAR): BOOLEAN;
VAR
  file: rf.ChanId;
  res : rf.OpenResults;
  i   : CARDINAL;
BEGIN
  CreateFileName (EXT_HDR, full_name);
  rf.OpenOld (file, full_name, rf.raw+rf.read, res);
  IF res # rf.opened THEN
    RETURN FALSE;
  END;
  rio.Read(file, Header);
  NEW (Objects, Header.objinmod);
  FOR i := 0 TO Header.objinmod-1 DO
    rio.Read(file, Objects^[i]);
  END;
  rf.Close(file);
  RETURN TRUE;
END ReadHeader;


BEGIN
END Header.

