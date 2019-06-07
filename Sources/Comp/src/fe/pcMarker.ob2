MODULE pcMarker;

IMPORT
  xfs := xiFiles,
  env := xiEnv,
  pc := pcK,
  DStrings,
  crc,
  SYSTEM;

CONST
  VERSION_OptionName    = "VERSION";
  CRCVERSION_OptionName = "CRCVERSION";
  SOURCEID_OptionName   = "SOURCEID";
  CRCVERSION_DEF_ConstName* = CRCVERSION_OptionName + "_DEF";
  CRCVERSION_MOD_ConstName* = CRCVERSION_OptionName + "_MOD";
  COMPILER_VERSION_OptionName = "COMPILER_VERSION";

TYPE
  STRING = xfs.String;

-- append "label" with a module name and mangled "version"
PROCEDURE mangle_label(VAR label: STRING; version: ARRAY OF CHAR);
VAR
  i: LONGINT;
BEGIN
  DStrings.Append("_", label);
  DStrings.Append(pc.mods[pc.cur_mod].name^, label);
  IF LENGTH(version) # 0 THEN
    DStrings.Append("_", label);
    FOR i := 0 TO LENGTH(version) - 1 DO
      IF (version[i] # "_") &
        ((ORD(version[i]) < ORD('a')) OR (ORD(version[i]) > ORD('z'))) &
        ((ORD(version[i]) < ORD('A')) OR (ORD(version[i]) > ORD('Z'))) &
        ((ORD(version[i]) < ORD('0')) OR (ORD(version[i]) > ORD('9')))
      THEN
        version[i] := "_"
      END
    END;
    DStrings.Append(version, label)
  END
END mangle_label;

--------------------------------------------------------------------------------
PROCEDURE GetVersionLabel * (for_def_module:=FALSE: BOOLEAN): STRING;
VAR
  version, label: STRING;
BEGIN
  env.config.Equation(VERSION_OptionName, version);
  IF (version # NIL) & (version^ # "") THEN
    DStrings.Assign(VERSION_OptionName, label);
    mangle_label(label, version^);
    RETURN label;
  ELSIF env.config.Option("K26") THEN
    env.errors.Warning(pc.mods[pc.cur_mod].pos, 2001)
  END;
  RETURN NIL
END GetVersionLabel;

--------------------------------------------------------------------------------
PROCEDURE GetCompilerVersionLabel * (): STRING;
VAR version, label: STRING;
BEGIN
  DStrings.Assign(pc.pars.vers, version);
  DStrings.Append(" ", version);
  DStrings.Append(pc.code.vers, version);

  DStrings.Assign(COMPILER_VERSION_OptionName, label);

  mangle_label(label, version^);
  RETURN label;
END GetCompilerVersionLabel;


--------------------------------------------------------------------------------
PROCEDURE cksum16_tab(p_byte: SYSTEM.ADDRESS; length: SYSTEM.CARD32): SYSTEM.CARD16;
CONST INITVAL = 01D0FH;
BEGIN
  RETURN crc.cksum16_tab(INITVAL, p_byte, length)
END cksum16_tab;

--------------------------------------------------------------------------------
PROCEDURE cksum32_tab(p_byte: SYSTEM.ADDRESS; length: SYSTEM.CARD32): SYSTEM.CARD32;
CONST INITVAL = 01D0FH;
BEGIN
  RETURN crc.cksum32_tab(INITVAL, p_byte, length)
END cksum32_tab;


--------------------------------------------------------------------------------
PROCEDURE GetFileCrc16 * (fileName-: ARRAY OF CHAR; VAR crc: SYSTEM.CARD16): BOOLEAN;
VAR
  fnm: STRING;
  file: xfs.RawFile;
  size: SYSTEM.CARD32;
  data: POINTER TO ARRAY OF SYSTEM.BYTE;
BEGIN
--  xfs.sys.Lookup(env.info.file^, fnm);
  xfs.sys.Lookup(fileName, fnm);
  xfs.raw.Open(fnm^, FALSE);
  IF xfs.raw.file # NIL THEN
    file := xfs.raw.file(xfs.RawFile);
    size := file.Length();
    NEW(data, size);
    file.ReadBlock(data^, 0, size);
    IF file.readRes = xfs.allRight THEN
      crc := cksum16_tab(SYSTEM.ADR(data^), size);
      RETURN TRUE
    END
  END;
  RETURN FALSE
END GetFileCrc16;

--------------------------------------------------------------------------------
PROCEDURE GetFileCrc32 * (fileName-: ARRAY OF CHAR; VAR crc: SYSTEM.CARD32): BOOLEAN;
VAR
  fnm: STRING;
  file: xfs.RawFile;
  size: SYSTEM.CARD32;
  data: POINTER TO ARRAY OF SYSTEM.BYTE;
BEGIN
  xfs.sys.Lookup(fileName, fnm);
  xfs.raw.Open(fnm^, FALSE);
  IF xfs.raw.file # NIL THEN
    file := xfs.raw.file(xfs.RawFile);
    size := file.Length();
    NEW(data, size);
    file.ReadBlock(data^, 0, size);
    IF file.readRes = xfs.allRight THEN
      crc := cksum32_tab(SYSTEM.ADR(data^), size);
      RETURN TRUE
    END
  END;
  RETURN FALSE
END GetFileCrc32;


--------------------------------------------------------------------------------
PROCEDURE GetCurrentModuleCrc16 * (VAR crc_value: SYSTEM.CARD16): BOOLEAN;
BEGIN
  RETURN GetFileCrc16 (env.info.file^, crc_value);
END GetCurrentModuleCrc16;

--------------------------------------------------------------------------------
PROCEDURE GetCurrentModuleCrc32 * (VAR crc_value: SYSTEM.CARD32): BOOLEAN;
BEGIN
  RETURN GetFileCrc32 (env.info.file^, crc_value);
END GetCurrentModuleCrc32;


--------------------------------------------------------------------------------
PROCEDURE GetCrcVersionDefLabel*(): STRING;
VAR
  label: STRING;
BEGIN
  DStrings.Assign(CRCVERSION_DEF_ConstName, label);
  mangle_label(label, "");
  RETURN label;
END GetCrcVersionDefLabel;

PROCEDURE GetCrcVersionModLabel*(): STRING;
VAR
  label: STRING;
BEGIN
  IF env.config.Option(CRCVERSION_OptionName) THEN
    DStrings.Assign(CRCVERSION_MOD_ConstName, label);
    mangle_label(label, "");
    RETURN label;
  ELSIF env.config.Option("K26") THEN
    env.errors.Warning(pc.mods[pc.cur_mod].pos, 2003);
  END;
  RETURN NIL;
END GetCrcVersionModLabel;

PROCEDURE isEnabled_CrcVersion *(): BOOLEAN;
VAR enabled: BOOLEAN;
BEGIN
  enabled := env.config.Option(CRCVERSION_OptionName);
  IF NOT enabled AND env.config.Option("K26") THEN
    env.errors.Warning(pc.mods[pc.cur_mod].pos, 2003);
  END;
  RETURN enabled;
END isEnabled_CrcVersion;


PROCEDURE GetSourceIdLabelAndValue* (VAR label, value: STRING): BOOLEAN;
VAR
  sourceIdSpecified: BOOLEAN;
BEGIN
  env.config.Equation (SOURCEID_OptionName, value);
  sourceIdSpecified := (value # NIL) AND (value^ # "");
  IF sourceIdSpecified THEN
    DStrings.Assign (SOURCEID_OptionName, label);
    DStrings.Append ("_", label);
    DStrings.Append (pc.mods[pc.cur_mod].name^, label)
  END;
  RETURN sourceIdSpecified
END GetSourceIdLabelAndValue;

PROCEDURE AddMarkerComments* ();

  PROCEDURE append_version;
  VAR
    c: pc.Comment;
    ver: STRING;
  BEGIN
    ver := GetVersionLabel(TRUE (* definition module *));
    IF (ver # NIL) & (ver^ # "") THEN
      ASSERT( env.info.file # NIL );
      NEW(c);
      c.tags := pc.CTAG_SET{};
      DStrings.Assign("("+"** ", c.str);
      DStrings.Append(ver^, c.str);
      DStrings.Append(" *)", c.str);
      c.pos.pack(env.info.file^, 0, 0);
      c.end.pack(env.info.file^, 0, LENGTH(c.str^));
      c.Append();
    END
  END append_version;

  PROCEDURE append_sourceId;
  VAR
    c: pc.Comment;
    label, value: STRING;
  BEGIN
    IF GetSourceIdLabelAndValue(label, value) THEN
      ASSERT( env.info.file # NIL );
      NEW(c);
      c.tags := pc.CTAG_SET{};
      DStrings.Assign("("+"** ", c.str);
      DStrings.Append(label^, c.str);
      DStrings.Append('="', c.str);
      DStrings.Append(value^, c.str);
      DStrings.Append('" *)', c.str);
      c.pos.pack(env.info.file^, 0, 0);
      c.end.pack(env.info.file^, 0, LENGTH(c.str^));
      c.Append();
    END
  END append_sourceId;

  -- returns TRUE, if a marker was removed
  PROCEDURE remove (): BOOLEAN;
  VAR c: pc.Comment;
  BEGIN
    IF pc.comments # NIL THEN
      c := pc.comments;
      REPEAT
        IF c.isBelongToCurrentModule() &
          (c.isMarker(VERSION_OptionName) OR c.isMarker(VERSION_OptionName))
        THEN
          c.Remove();
          RETURN TRUE;
        ELSE
          c := c.next;
        END;
      UNTIL (c = pc.comments);
    END;
    RETURN FALSE;
  END remove;

BEGIN
  WHILE remove() DO
  END;
  append_version;
  append_sourceId
END AddMarkerComments;

END pcMarker.