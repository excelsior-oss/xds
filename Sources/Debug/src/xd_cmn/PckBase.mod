IMPLEMENTATION MODULE PckBase;

IMPORT str := Strings;

IMPORT opt := Options;
IMPORT fil := File;
IMPORT        xStr;

IMPORT pt  := PckTypes;


PROCEDURE GetParam (st-: ARRAY OF CHAR; VAR p:CARDINAL; VAR dest: ARRAY OF CHAR): BOOLEAN;
VAR
  p0 : CARDINAL;
BEGIN
  IF SkipBlanks(st, p) THEN
    RETURN FALSE;
  ELSE
    p0 := 0;
    WHILE (st[p] > ' ') & (st[p] # pt.SepParam) & (st[p] # pt.Comment) & (st[p] # 0C) DO
      dest[p0] := st[p];
      INC(p);
      INC(p0);
    END;
    dest[p0] := 0C;
    RETURN p0 # 0;
  END;
END GetParam;


PROCEDURE SkipBlanks (st-:ARRAY OF CHAR;VAR p:CARDINAL): BOOLEAN;
BEGIN
  WHILE (st[p] # 0C) AND (st[p] <= ' ') DO INC(p); END;
  RETURN (st[p] = pt.Comment) OR (st[p] = 0C);
END SkipBlanks;


(* “αβ ­ Ά«¨Ά ¥β ΰ¥¦¨¬ *)
PROCEDURE ModeOn (mode: pt.MODE_FLAGS );
BEGIN
  INCL(pt.Mode,mode);
END ModeOn;

(* ‚λª«ξη ¥β ΰ¥¦¨¬ *)
PROCEDURE ModeOff (mode: pt.MODE_FLAGS );
BEGIN
  EXCL(pt.Mode,mode);
END ModeOff;

(* ΰ®Ά¥ΰο¥β, Άª«ξη¥­ «¨ ΰ¥¦¨¬ *)
PROCEDURE CheckMode (mode: pt.MODE_FLAGS ) : BOOLEAN;
BEGIN
  RETURN (mode IN pt.Mode);
END CheckMode;


PROCEDURE SetModeName (mode: pt.MODE_FLAGS; first-, second-: ARRAY OF CHAR);
BEGIN
  WITH pt.ModeNames[mode] DO
    COPY(first, First);
    COPY(second, Second);
  END;
END SetModeName;


PROCEDURE SetPrefixMode ();
BEGIN
  pt.ModePrefix := TRUE;
END SetPrefixMode;


PROCEDURE SetPostfixMode ();
BEGIN
  pt.ModePrefix := FALSE;
END SetPostfixMode;


PROCEDURE SetModeOn (on-: ARRAY OF CHAR);
BEGIN
  COPY(on, pt.ModeOn);
END SetModeOn;


PROCEDURE SetModeOff (off-: ARRAY OF CHAR);
BEGIN
  COPY(off, pt.ModeOff);
END SetModeOff;


PROCEDURE ShiftMode (mode: pt.MODE_FLAGS; active: BOOLEAN);
BEGIN
  IF active THEN INCL(pt.Mode, mode); ELSE EXCL(pt.Mode, mode); END;
END ShiftMode;


PROCEDURE GetMode (name-: ARRAY OF CHAR; VAR mode: pt.MODE_FLAGS; VAR active: BOOLEAN): BOOLEAN;

  PROCEDURE FindMode (current-: ARRAY OF CHAR): BOOLEAN;
  VAR
    m: pt.MODE_FLAGS;
  BEGIN
    FOR m := MIN(pt.MODE_FLAGS) TO MAX(pt.MODE_FLAGS) DO
      WITH pt.ModeNames[m] DO
        IF (First = current) OR (Second = current) THEN
          mode := m;
          RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END FindMode;

VAR
  found: BOOLEAN;
  s  : pt.SHORT_STRING;
  pos: CARDINAL;
BEGIN
  IF name = '' THEN RETURN FALSE; END;
  found := FALSE;
  IF pt.ModePrefix THEN
    IF pt.ModeOn # '' THEN
      str.FindNext(pt.ModeOn, name, 0, found, pos);
      IF found THEN
        xStr.Extract(name, pos+LENGTH(pt.ModeOn), LENGTH(name), s);
        active := TRUE;
      END;
    ELSIF FindMode(name) THEN
      active := TRUE;
      RETURN TRUE;
    END;
    IF NOT found THEN
      IF pt.ModeOff # '' THEN
        str.FindNext(pt.ModeOff, name, 0, found, pos);
        IF found THEN
          xStr.Extract(name, pos+LENGTH(pt.ModeOff), LENGTH(name), s);
          active := FALSE;
        END;
      ELSIF FindMode(name) THEN
        active := FALSE;
        RETURN TRUE;
      END;
    END;
  ELSE
    IF pt.ModeOn # '' THEN
      str.FindPrev(pt.ModeOn, name, LENGTH(name)-1, found, pos);
      IF found THEN
        xStr.Extract(name, 0, pos, s);
        active := TRUE;
      END;
    ELSIF FindMode(name) THEN
      active := TRUE;
      RETURN TRUE;
    END;
    IF NOT found THEN
      IF pt.ModeOff # '' THEN
        str.FindPrev(pt.ModeOff, name, LENGTH(name)-1, found, pos);
        IF found THEN
          xStr.Extract(name, 0, pos, s);
          active := FALSE;
        END;
      ELSIF FindMode(name) THEN
        active := FALSE;
        RETURN TRUE;
      END;
    END;
  END;
  IF found AND FindMode(s) THEN RETURN TRUE; END;
  RETURN FALSE;
END GetMode;


PROCEDURE GetBreakMode (name-: ARRAY OF CHAR; VAR mode: pt.MODE_FLAGS): BOOLEAN;
VAR
  m: pt.MODE_FLAGS;
BEGIN
  IF name = '' THEN RETURN FALSE; END;
  FOR m := MIN(pt.MODE_FLAGS) TO MAX(pt.MODE_FLAGS) DO
    IF m IN pt.ModeBreak THEN
      WITH pt.ModeNames[m] DO
        IF (First = name) OR (Second = name) THEN
          mode := m;
          RETURN TRUE;
        END;
      END;
    END;
  END;
  RETURN FALSE;
END GetBreakMode;



PROCEDURE SetIdentName (ident: pt.IDENT; name-: ARRAY OF CHAR);
BEGIN
  COPY(name, pt.IdentNames[ident]);
END SetIdentName;


PROCEDURE GetIdent (name-: ARRAY OF CHAR; VAR ident: pt.IDENT): BOOLEAN;
VAR
  i: pt.IDENT;
BEGIN
  FOR i := MIN(pt.IDENT) TO MAX(pt.IDENT) DO
    IF pt.IdentNames[i] = name THEN
      ident := i;
      RETURN TRUE;
    END;
  END;
  RETURN FALSE;
END GetIdent;



PROCEDURE FileName (full-:ARRAY OF CHAR; VAR name:ARRAY OF CHAR);
BEGIN
  IF opt.name_only THEN
    fil.ExtractFileName(full, name);
  ELSE
    COPY(full, name);
  END;
END FileName;


BEGIN
  ModeOn (pt.OutMem);
  ModeOn (pt.WrProt);
  ModeOn (pt.ProgInt);
  ModeOn (pt.Address);
  ModeOn (pt.Write);
  ModeOn (pt.WriteReg);
  ModeOn (pt.Read);
  ModeOn (pt.ReadReg);
  ModeOn (pt.Call);
  ModeOn (pt.Ret);
  ModeOn (pt.If);
  ModeOn (pt.Line);

<* IF DEST_K26 THEN *>

  SetModeName (pt.Pak,           'PAK',            '€…’');
  SetModeName (pt.Trace,         'TRACE',          '’€‘‘€');
  SetModeName (pt.Put,           'PUT',            '…—€’');
  SetModeName (pt.IO,            'IO',             '…');
  SetModeName (pt.Time_Count,    'TIME',           '‚…');
  SetModeName (pt.Put_Addr,      'ADR',            '€„…‘');
  SetModeName (pt.Dump,          'DUMP',           '„€');
  SetModeName (pt.User,          '‹‡',          'USER');
  SetModeName (pt.OutMem,        '‚',             'OUT');
  SetModeName (pt.WrProt,        '‡“',            'WRPR');
  SetModeName (pt.ProgInt,       '',             'PR');
  SetModeName (pt.Address,       '€„',            'ADDR');
  SetModeName (pt.Write,         '‡',              'WRITE');
  SetModeName (pt.WriteReg,      '‡',              'WRITE');
  SetModeName (pt.Read,          '—',              'READ');
  SetModeName (pt.ReadReg,       '—',              'READ');
  SetModeName (pt.Call,          '‚›‡‚',          'CALL');
  SetModeName (pt.Ret,           '‚‡‚€’',        'RET');
  SetModeName (pt.If,            '…‘‹',           'IF');
  SetModeName (pt.Line,          '‘',              'LINE');
  SetModeName (pt.IO,            '‚‚',             'IO'  );
  SetModeName (pt.System,        '‘‘’…€',        'SYSTEM');
  SetModeName (pt.Storage,       '€’',         'STORAGE');
  SetModeName (pt.StoragePrint,  '€’_…—€’',  'STORAGE_PRINT');
  SetModeName (pt.CompCreated,   '”€‡€',           'COMP' );
  SetModeName (pt.CompDestroyed, '”€‡€“„',         'COMPDEL');
  SetModeName (pt.Test,          '’…‘’',           'TEST');
  SetModeName (pt.ModelProc,     '–_…—€’',    'PROC');
  SetModeName (pt.DirectStart,   '‘’€’',          'START');

  SetIdentName (pt.TraceCode,    'CODE'   );
  SetIdentName (pt.TraceSource,  'SOURCE' );
  SetIdentName (pt.TraceMix,     'MIX'    );
  SetIdentName (pt.Error,        'ERRORS' );
  SetIdentName (pt.Disasm,       'DISASM' );

  pt.TraceMode := pt.Code;

  ModeOn (pt.Pak);
  ModeOn (pt.Time_Count);
  ModeOn (pt.Put);
  ModeOn (pt.Put_Addr);
  ModeOn (pt.ModelProc);

  SetPrefixMode;
  SetModeOn  ('');
  SetModeOff ('NO');

<* ELSIF DEST_XDS THEN *>

  SetModeName (pt.Pak,           'BAT',      'bat' );
  SetModeName (pt.Trace,         'TRACE',    'trace' );
  SetModeName (pt.Put,           'PRINT',    'print' );
  SetModeName (pt.Put_Addr,      'PRN_ADDR', 'prn_addr' );
  SetModeName (pt.Dump,          'DUMP',     'dump' );
  SetModeName (pt.User,          'USER',     'user' );
  SetModeName (pt.ProgInt,       'XCPT',     'xcpt' );
  SetModeName (pt.Address,       'ADDR',     'addr' );
  SetModeName (pt.Write,         'WRITE',    'write' );
  SetModeName (pt.Read,          'READ',     'read' );
  SetModeName (pt.Call,          'PROC',     'proc' );
  SetModeName (pt.If,            'COND',     'cond' );
  SetModeName (pt.Line,          'LINE',     'line' );
  SetModeName (pt.CompCreated,   'COMP',     'comp' );
  SetModeName (pt.CompDestroyed, 'COMPDEL',  'compdel');

  SetIdentName (pt.TraceCode,    'CODE'   );
  SetIdentName (pt.TraceSource,  'SOURCE' );
  SetIdentName (pt.TraceMix,     'MIX'    );
  SetIdentName (pt.Error,        'ERRORS' );
  SetIdentName (pt.Disasm,       'DISASM' );

  pt.TraceMode := pt.Source;

  ModeOn (pt.Pak);
  ModeOn (pt.Put);
  ModeOn (pt.CompCreated);

  SetPostfixMode;
  SetModeOn  ('+');
  SetModeOff ('-');

<* END *>

END PckBase.


