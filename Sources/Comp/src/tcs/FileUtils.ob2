MODULE FileUtils;

IMPORT
  sys := SYSTEM,
<* IF TARGET_FAMILY="WIN32" THEN *>
  Windows,
<* END *>
  CharClass,
  Strings,
  DStr := DStrings,
  FileName,
  RegComp,
  FileSys,
  IOChan,
  TextIO,
  IOResult,
  ProgEnv,
  Printf,
  platform;

CONST
  -- PROCEDURE FileSys.Exists(fname: ARRAY OF CHAR): BOOLEAN;
  -- Returns TRUE, if file "fname" exists.
  exists *= FileSys.Exists;


  -- PROCEDURE FileSys.CreateDirectory(name: ARRAY OF CHAR): BOOLEAN;
  makeDir *= FileSys.CreateDirectory;

TYPE
  StringT *= DStr.String;

CONST
  MAX_PATH_LENGTH = 1024;

--------------------------------------------------------------------------------
--                         File Name Utilities
--------------------------------------------------------------------------------
PROCEDURE replaceExtension * (VAR name: StringT; ext-: ARRAY OF CHAR);
VAR
  f: FileName.Format;
BEGIN
  FileName.GetFormat(name^, f);
  IF f.ok THEN
    IF f.extPos > 0 THEN
      name[f.extPos - 1] := 0C;
    END;
    DStr.Append(ext, name)
  ELSE
    ASSERT(FALSE)
  END
END replaceExtension;


--------------------------------------------------------------------------------
PROCEDURE getParent * (path-: ARRAY OF CHAR): StringT;
VAR
  f: FileName.Format;
  parent: StringT;
BEGIN
  FileName.GetFormat(path, f);
  IF f.ok THEN
    NEW(parent, LENGTH(path));
    Strings.Extract(path, f.dirPos, f.dirLen, parent^);
  ELSE
    parent := NIL;
  END;
  RETURN parent;
END getParent;


--------------------------------------------------------------------------------
PROCEDURE makeDirs * (path-: ARRAY OF CHAR): BOOLEAN;
VAR parent: StringT;
BEGIN
  IF (path # "") & NOT exists(path) THEN
     parent := getParent(path);
     RETURN ((parent = NIL) OR (parent^ = "") OR makeDirs(parent^))
          & makeDir(path);
  ELSE
    RETURN TRUE;
  END;
END makeDirs;

--------------------------------------------------------------------------------
PROCEDURE deleteNameAndExtension (VAR path: StringT);
VAR
  f: FileName.Format;
BEGIN
  FileName.GetFormat(path^, f);
  IF f.ok THEN
    path[f.dirPos + f.dirLen + 1] := 0C
  ELSE
    ASSERT(FALSE)
  END
END deleteNameAndExtension;

--------------------------------------------------------------------------------
PROCEDURE deleteExtension * (name: StringT);
VAR
  f: FileName.Format;
BEGIN
  FileName.GetFormat(name^, f);
  IF f.ok THEN
    IF f.extPos > 0 THEN
      name[f.extPos - 1] := 0C;
    END
  ELSE
    ASSERT(FALSE)
  END
END deleteExtension;

--------------------------------------------------------------------------------
PROCEDURE applyRoot * (fn-: ARRAY OF CHAR; root: StringT): StringT;
VAR
  result: StringT;
  len: LONGINT;
BEGIN
  len := LENGTH(fn);
  IF len > 0 THEN
    IF root # NIL THEN
      DStr.Assign(root^, result);
      DStr.Append(fn, result)
    ELSE
      DStr.Assign(fn, result)
    END;
    RETURN result
  ELSE
    RETURN NIL
  END
END applyRoot;


--------------------------------------------------------------------------------
--                         Red file support
--------------------------------------------------------------------------------
TYPE
  Filename = ARRAY 256 OF CHAR;
  File = IOChan.ChanId;

  DirPtr = POINTER TO Dir;
  Dir = RECORD
    path: StringT;
    next: DirPtr
  END;

  NodePtr = POINTER TO Node;
  Node = RECORD
    expr: RegComp.Expr;
    dirs: DirPtr;
    level: LONGINT;
    next: NodePtr
  END;

  Format = RECORD
    ok: BOOLEAN;
    dirPos, dirLen: LONGINT;
    namePos,nameLen: LONGINT;
    extPos, extLen: LONGINT
  END;

  SyntaxErrorHandler = PROCEDURE(fname: StringT; line, ps: LONGINT);

  ScanIfPtr = POINTER TO ScanIf;
  ScanRed = RECORD
    in: File;
    fname: StringT;
    lineBf: StringT;
    lineNo: LONGINT;
    ifs: ScanIfPtr;
    false: StringT;
    true: StringT;
    srcBf: StringT
  END;

  ScanIf = RECORD
    val: BOOLEAN;
    else: BOOLEAN;
    elsif: BOOLEAN;
    up: ScanIfPtr
  END;

VAR
  nodes: NodePtr;
  level: LONGINT;
  isPathSep: ARRAY CHAR OF BOOLEAN;
  null: StringT;

--------------------------------------------------------------------------------
PROCEDURE convertCaseToHost(fr-: ARRAY OF CHAR; VAR to: StringT);
BEGIN
  DStr.Assign(fr, to);
<* IF TARGET_FAMILY # "UNIX" THEN *>
  Strings.Capitalize(to^)
<* END *>
END convertCaseToHost;

--------------------------------------------------------------------------------
PROCEDURE convertToPortableNotation * (fname-: ARRAY OF CHAR; VAR str: ARRAY OF CHAR);
VAR
  i, len: LONGINT;
  ch: CHAR;

<* IF TARGET_FAMILY="VMS" THEN *>
  PROCEDURE get;
  BEGIN
    IF i < LEN(fname) THEN
      ch := fname[i];
      INC(i)
    ELSE
      ch := 0C
    END
  END get;
<* END *>

  PROCEDURE put(ch: CHAR);
  BEGIN
    IF ch = '"' THEN
      RETURN
    END;
    IF len < LEN(str) THEN
      str[len] := ch
    END;
    INC(len)
  END put;

BEGIN
  len := 0;
  i := 0;
<* IF TARGET_FAMILY="VMS" THEN *>
  get;
  IF ch # "[" THEN
    LOOP
      WHILE (ch # 0C) AND (ch # ":") DO
        put(ch);
        get
      END;
      IF ch = ":" THEN
        put(":");
        get;
        IF ch = "[" THEN
          EXIT
        END
      ELSE
        EXIT
      END
    END
  END;
  IF ch = "[" THEN
    get;
    IF ch = "." THEN
      put(".");
      get;
      IF ch # "]" THEN
        put("/")
      END
    ELSIF ch = "]" THEN
      put(".");
    ELSIF (ch # "-") THEN
      put("/")
    END;
    WHILE (ch # "]") AND (ch # 0C) DO
      IF ch = "." THEN
        ch := "/"
      ELSIF (ch = "-") THEN
        put(".");
        ch:="."
      END;
      put(ch);
      get
    END;
    IF ch = "]" THEN
      put("/");
      get
    END
  END;
  WHILE (ch # 0C) DO
    put(ch);
    get
  END;
<* ELSIF TARGET_FAMILY="UNIX" THEN *>
  WHILE (i < LEN(fname)) AND (fname[i] # 0C) DO
    put(fname[i]);
    INC(i)
  END;
<* ELSE *>
  WHILE (i < LEN(fname)) AND (fname[i] # 0C) DO
    ch := fname[i];
    INC(i);
    IF ch = "\" THEN
      ch := "/"
    END;
    put(ch)
  END;
<* END *>
  put(0C)
END convertToPortableNotation;

--------------------------------------------------------------------------------
PROCEDURE convertFromPortableNotation * (fname-: ARRAY OF CHAR; VAR str: ARRAY OF CHAR);
VAR
  i, len: LONGINT;
  ch: CHAR;
<* IF TARGET_FAMILY="VMS" THEN *>
  n: LONGINT;
<* END *>

  PROCEDURE put(ch: CHAR);
  BEGIN
    IF len < LEN(str) THEN
      str[len] := ch
    END;
    INC(len)
  END put;

  PROCEDURE get;
  BEGIN
    IF i < LEN(fname) THEN
      ch := fname[i];
      INC(i)
    ELSE
      ch := 0C
    END
  END get;

BEGIN
  len := 0;
  i := 0;
<* IF TARGET_FAMILY="VMS" THEN *>
  n := 0;
  LOOP -- Search last ":" in disk prefix
    WHILE (i < LEN(fname)) AND (fname[i] # 0C) AND (fname[i] # ":") AND
      (fname[i] # "/")
    DO
      INC(i)
    END;
    IF fname[i] = ":" THEN
      n := i;
      INC(i)
    ELSE
      EXIT
    END
  END;
  i := 0;
  IF (n > 0) OR (fname[0] = ":") THEN
    WHILE i <= n DO
      put(fname[i]);
      INC(i)
    END
  END;
  n := LENGTH(fname);
  WHILE (n > i) AND (fname[n] # "/") DO
    DEC(n)
  END;
  IF (n > i) THEN -- path with subdirectories
    put("[");
    IF (fname[i] = "/") THEN
      INC(i) -- nothing to put
    ELSIF (fname[i] = ".") THEN
      IF (i + 1 < LEN(fname)) AND (fname[i + 1] = "/") THEN
        INC(i)
      END
    ELSE
      put(".")
    END;
    WHILE (i < n) DO
      IF (fname[i] = ".") THEN
        IF (i + 1 < LEN(fname)) AND (fname[i + 1] = ".") THEN
          INC(i);
          put("-")
        ELSE
          put(".")
        END
      ELSIF (fname[i] = "/") THEN
        put(".")
      ELSE
        put(fname[i])
      END;
      INC(i)\
    END;
    INC(i);
    put("]")
  ELSIF (i = n) AND (fname[n] = "/") THEN
    -- root dir; no subdirs. As I've found on v850 roots of all volumes are
    -- called "[000000]".
    put("[");
    FOR i := 1 TO 6 DO
      put("0")
    END;
    put("]");
    i := n + 1
  END;
  WHILE (i < LEN(fname)) AND (fname[i] # 0C) DO
    put(fname[i]);
    INC(i)
  END;
<* ELSIF TARGET_FAMILY="UNIX" THEN *>
  WHILE (i < LEN(fname)) AND (fname[i] # 0C) DO
    put(fname[i]);
    INC(i)
  END;
<* ELSE *>
  IF CharClass.IsLetter(fname[0]) AND (LEN(fname) > 1) AND (fname[1] = ":") THEN
    put(fname[0]);
    put(fname[1]);
    i := 2
  END;
  get;
  WHILE ch # 0C DO
    IF ch = '/' THEN
      put('\')
    ELSE
      put(ch)
    END;
    get
  END;
<* END *>
  put(0C)
END convertFromPortableNotation;

--------------------------------------------------------------------------------
PROCEDURE parsePath
  (VAR dirs: DirPtr; path-: ARRAY OF CHAR; i: LONGINT; VAR err: LONGINT);
VAR
  d, last: DirPtr;
  b, e: LONGINT;
  q: CHAR;
BEGIN
  last := NIL;
  LOOP
    WHILE isPathSep[path[i]] OR (path[i] = ' ') DO
      INC(i)
    END;
    b := i;
    q := path[i];
    IF (q = '"') OR (q = "'") THEN
      INC(b);
      INC(i);
      WHILE (path[i] # 0X) AND (path[i] # q) DO
        INC(i)
      END;
      IF path[i] = 0X THEN
        err := i;
        EXIT
      END;
      e := i;
      INC(i)
    ELSE
      WHILE (path[i] # 0X) AND NOT isPathSep[path[i]] DO
        INC(i)
      END;
      e := i;
      IF NOT isPathSep[' '] THEN
        DEC(e);
        WHILE (e >= b) AND (path[e] = ' ') DO
          DEC(e)
        END;
        INC(e)
      END
    END;
    IF b = e THEN
      EXIT
    END;
    NEW(d);
    NEW(d.path, e - b + 1);
    Strings.Extract(path, b, e - b, d.path^);
    convertFromPortableNotation(d.path^, d.path^);
    IF last = NIL THEN
      dirs := d
    ELSE
      last.next := d
    END;
    last := d
  END
END parsePath;

--------------------------------------------------------------------------------
PROCEDURE set
  (pattern-: ARRAY OF CHAR; path-: ARRAY OF CHAR; i: LONGINT; VAR err: LONGINT);
VAR
  e: RegComp.Expr;
  res: LONGINT;
  l, p, n: NodePtr;
  sPatt: StringT;
BEGIN
  convertCaseToHost(pattern, sPatt);
  RegComp.Compile(sPatt^, e, res);
  IF res <= 0 THEN
    err := ABS(res)
  ELSE
    NEW(n);
    n.expr := e;
    n.level := level;
    parsePath(n.dirs, path, i, err);
    IF err < 0 THEN
      l := nodes;
      p := NIL;
      WHILE (l # NIL) AND (l.level = level) DO
        p := l;
        l := l.next
      END;
      IF p = NIL THEN
        nodes := n
      ELSE
        p.next := n
      END;
      n.next := l
    END
  END
END set;

--------------------------------------------------------------------------------
PROCEDURE parseRed(s-: ARRAY OF CHAR; VAR err: LONGINT);
VAR
  i, p: LONGINT;
  len: LONGINT;
  pattern: Filename;
BEGIN
  err := -1;
  i := 0;
  len := LENGTH(s);
  WHILE (i < len) AND CharClass.IsWhiteSpace(s[i]) DO
    INC(i)
  END;
  p := i;
  WHILE (i < len) AND (s[i] # '=') AND NOT CharClass.IsWhiteSpace(s[i]) DO
    INC(i)
  END;
  Strings.Extract(s, p, i - p, pattern);
  WHILE (i < len) AND (s[i] # '=') DO
    INC(i)
  END;
  IF i = len THEN
    err := i
  ELSE
    INC(i);
    WHILE (i < len) AND CharClass.IsWhiteSpace(s[i]) DO
      INC(i)
    END;
    set(pattern, s, i, err)
  END
END parseRed;

--------------------------------------------------------------------------------
-- For WIN32 target, unfortunately, FileName.Create() capitalizes fname and
-- trims it to 8.3 format. That does not meet our needs.
PROCEDURE create(dir-, name-, ext: ARRAY OF CHAR; VAR fname: ARRAY OF CHAR);
<* IF TARGET_FAMILY="WIN32" THEN *>
VAR
  p, len: LONGINT;

  PROCEDURE append(s-: ARRAY OF CHAR);
  VAR
    i, l: LONGINT;
  BEGIN
    i := 0;
    l := len;
    WHILE (p < LEN(fname)) AND (l > 0) DO
      fname[p] := s[i];
      INC(p);
      INC(i);
      DEC(l)
    END
  END append;

  PROCEDURE char(c: CHAR);
  BEGIN
    IF p < LEN(fname) THEN
      fname[p] := c;
      INC(p)
    END
  END char;

VAR
  last: CHAR;
BEGIN
  p := 0;
  len := LENGTH(dir);
  IF len > 0 THEN
    append(dir);
    last := dir[len - 1];
    IF (last # '\') AND (last # ':') THEN
      char('\')
    END
  END;
  len := LENGTH(name);
  append(name);
  len := LENGTH(ext);
  IF len > 0 THEN
    IF ext[0] = '.' THEN
      Strings.Delete(ext, 0, 1);
      DEC(len)
    END;
    IF len > 0 THEN
      char('.');
      append(ext)
    END
  END;
  IF p < LEN(fname) THEN
    fname[p] := 0C
  END
<* ELSE *>
  FileName.Create(dir, name, ext, fname)
<* END *>
END create;

--------------------------------------------------------------------------------
PROCEDURE lookupFile
  (dirs: DirPtr; dir-, name-, ext-: ARRAY OF CHAR; VAR fname: Filename):
  BOOLEAN;
VAR
  fullDir: StringT;
BEGIN
  WHILE dirs # NIL DO
    IF dir # "" THEN
      DStr.Assign(dirs.path^, fullDir);
      IF fullDir[LENGTH(fullDir^)-1] # platform.pathSep THEN
        DStr.Append(platform.pathSep, fullDir);
      END;
      DStr.Append(dir, fullDir);
      create(fullDir^, name, ext, fname)
    ELSE
      create(dirs.path^, name, ext, fname)
    END;
    IF FileSys.Exists(fname) THEN
      RETURN TRUE
    END;
    dirs := dirs.next
  END;
  RETURN FALSE
END lookupFile;

--------------------------------------------------------------------------------
PROCEDURE isFullPath(path-: ARRAY OF CHAR): BOOLEAN;
VAR
  len: LONGINT;
BEGIN
  len := LENGTH(path);
  RETURN
<* IF TARGET_OS="UNIX" THEN *>
          (len > 1) AND ((path[0] = "/") OR (path[0] = "\"));
<* ELSE *>
          (len > 2) AND (path[1] = ":");
<* END *>
--  RETURN (len > 0) AND ((path[0] = '/') OR (path[0] = '\') OR (path[0] = '.') AND
--    ((len = 1) OR (path[1] = '\') OR (path[1] = '/') OR
--    (path[1] = '.') AND ((len = 2) OR (path[2] = '\') OR (path[2] = '/'))) OR
--    (len >= 2) AND (path[1] = ':'))
END isFullPath;

--------------------------------------------------------------------------------
PROCEDURE getFileName * (location-: ARRAY OF CHAR): StringT;
VAR tmp:  ARRAY MAX_PATH_LENGTH OF CHAR;
    name: ARRAY 512  OF CHAR;
    ext:  ARRAY 128  OF CHAR;
    res:  StringT;
BEGIN
  FileName.Get(location, tmp, name, ext);
  create("", name, ext, tmp);
  DStr.Assign(tmp, res);
  RETURN res;
END getFileName;

--------------------------------------------------------------------------------
PROCEDURE getPureFileName * (location-: ARRAY OF CHAR): StringT;
VAR name: ARRAY MAX_PATH_LENGTH OF CHAR;
    result:  StringT;
BEGIN
  FileName.GetName(location, name);
  DStr.Assign(name, result);
  RETURN result;
END getPureFileName;


--------------------------------------------------------------------------------
PROCEDURE look(nm-: ARRAY OF CHAR; VAR fname: Filename): BOOLEAN;
VAR
  l: NodePtr;
  dir, name, ext, nameAndExt: Filename;
  sNm: StringT;
BEGIN
  IF isFullPath(nm) THEN
    RETURN FileSys.Exists(nm)
  END;
  FileName.Get(nm, dir, name, ext);
  create("", name, ext, nameAndExt);
  l := nodes;
  convertCaseToHost(nameAndExt, sNm);
  WHILE l # NIL DO
    IF RegComp.Match(l.expr, sNm^, 0) AND
      lookupFile(l.dirs, dir, name, ext, fname)
    THEN
      RETURN TRUE
    END;
    l := l.next
  END;
  RETURN FileSys.Exists(nm)
END look;

--------------------------------------------------------------------------------
PROCEDURE sLookup(name-: Filename; VAR fname: Filename): BOOLEAN;
VAR
  found: BOOLEAN;
BEGIN
  found := look(name, fname);
  IF NOT found THEN
    fname := name
  END;
  RETURN found
END sLookup;

--------------------------------------------------------------------------------
PROCEDURE lookup * (name-: ARRAY OF CHAR; VAR fname: StringT): BOOLEAN;
VAR
  f: Filename;
  found: BOOLEAN;
BEGIN
  COPY(name, f);
  found := sLookup(f, f);
  IF found THEN
    DStr.Assign(f, fname);
  END;
  RETURN found
END lookup;

--------------------------------------------------------------------------------
PROCEDURE sysLookup * (ext-: ARRAY OF CHAR; VAR fname: StringT);
VAR x:    ARRAY MAX_PATH_LENGTH OF CHAR;
    dir:  ARRAY MAX_PATH_LENGTH OF CHAR;
    name: ARRAY MAX_PATH_LENGTH OF CHAR;
BEGIN
  ProgEnv.ProgramName(x);
  FileName.Get(x, dir, name, x);
  create("", name, ext, x);
  IF lookup(x, fname) THEN
    RETURN;
  END;
  IF exists(x) THEN
    DStr.Assign(x, fname);
    RETURN;
  END;
  IF dir # "" THEN
    FileName.Create(dir, name, ext, x);
    IF exists(x) THEN
      DStr.Assign(x, fname);
      RETURN;
    END;
  END;
  create("", name, ext, x);
  DStr.Assign(x, fname);
END sysLookup;


--------------------------------------------------------------------------------
PROCEDURE setEnv (name-, value-: ARRAY OF CHAR);
BEGIN
<* IF TARGET_FAMILY="WIN32" THEN *>
  Windows.SetEnvironmentVariable(name, value)
<* END *>
END setEnv;

--------------------------------------------------------------------------------
PROCEDURE(VAR r: ScanRed) writeError(pos: LONGINT);
BEGIN
  IF pos > 0 THEN
    Printf.printf("%s (%u,%u): syntax error\n", r.fname^, r.lineNo, pos)
  ELSE
    Printf.printf("%s (%u): syntax error\n", r.fname^, r.lineNo)
  END
END writeError;

--------------------------------------------------------------------------------
PROCEDURE(VAR r: ScanRed) do;
VAR
  ps: LONGINT;
BEGIN
  IF r.lineBf^ # "" THEN
    parseRed(r.lineBf^, ps);
    IF ps >= 0 THEN
      r.writeError(ps)
    END
  END
END do;

--------------------------------------------------------------------------------
PROCEDURE(VAR p: ScanRed) preprocessor(): BOOLEAN;
CONST
  ident  = 1X;
  string = 2X;

TYPE
  Str = ARRAY 256 OF CHAR;

VAR
  ps: LONGINT;
  syps: LONGINT;
  str: Str;
  sy: CHAR;
  err: BOOLEAN;
  skip: BOOLEAN;

  PROCEDURE on(i: ScanIfPtr): BOOLEAN;
  BEGIN
    RETURN (i = NIL) OR (i.val # i.else) AND on(i.up)
  END on;

  PROCEDURE isLetter(): BOOLEAN;
  BEGIN
    IF CharClass.IsWhiteSpace(p.lineBf[ps]) THEN
      RETURN FALSE
    END;
    CASE p.lineBf[ps] OF
    '!', '(', ')', '=', '#', '&', '<', '>', '+', '-', 0X:
      RETURN FALSE
    ELSE
      RETURN TRUE
    END
  END isLetter;

  PROCEDURE next;
  VAR
    j: LONGINT;
  BEGIN
    j := 0;
    WHILE CharClass.IsWhiteSpace(p.lineBf[ps]) DO
      INC(ps)
    END;
    syps := ps;
    LOOP
      IF p.lineBf[ps] = '"' THEN
        INC(ps);
        WHILE (p.lineBf[ps] # 0X) AND (p.lineBf[ps] # '"') DO
          IF j < LEN(str) - 1 THEN
            str[j] := p.lineBf[ps];
            INC(j)
          END;
          INC(ps)
        END;
        str[j] := 0X;
        sy := string;
        IF p.lineBf[ps] = '"' THEN
          INC(ps)
        END;
        RETURN
      ELSIF isLetter() THEN
        IF j < LEN(str) - 1 THEN
          str[j] := p.lineBf[ps];
          INC(j)
        END;
        INC(ps)
      ELSE
        IF j = 0 THEN
          sy := p.lineBf[ps];
          INC(ps)
        ELSE
          str[j] := 0X;
          sy := ident;
          Strings.Capitalize(str)
        END;
        RETURN
      END
    END
  END next;

  PROCEDURE nextAll;
  VAR
    j: LONGINT;
  BEGIN
    j := 0;
    WHILE CharClass.IsWhiteSpace(p.lineBf[ps]) DO
      INC(ps)
    END;
    WHILE p.lineBf[ps] # 0X DO
      IF j < LEN(str) - 1 THEN
        str[j] := p.lineBf[ps];
        INC(j)
      END;
      INC(ps)
    END;
    WHILE (j > 0) AND CharClass.IsWhiteSpace(str[j - 1]) DO
      DEC(j)
    END;
    str[j] := 0X
  END nextAll;

  PROCEDURE psError;
  BEGIN
    err := TRUE;
    p.writeError(syps + 1)
  END psError;

  PROCEDURE syntaxError;
  BEGIN
    psError
  END syntaxError;

  PROCEDURE typeError;
  BEGIN
    psError
  END typeError;

  PROCEDURE notDefined;
  BEGIN
    IF on(p.ifs) THEN
      psError
    END
  END notDefined;

  PROCEDURE boolean(x-: Str): BOOLEAN;
  BEGIN
    IF x = p.false^ THEN
      RETURN FALSE
    ELSIF x = p.true^ THEN
      RETURN TRUE
    ELSE
      typeError;
      RETURN FALSE
    END
  END boolean;

  PROCEDURE ^ expression(VAR z: Str);

  PROCEDURE factor(VAR z: Str);
  VAR
    x: Str;
    v: BOOLEAN;
  BEGIN
    IF sy = '(' THEN
      next;
      expression(z);
      IF sy # ')' THEN
        syntaxError
      ELSE
        next
      END
    ELSIF sy = string THEN
      z := str;
      next
    ELSIF sy # ident THEN
      syntaxError;
      z := ""
    ELSIF str = "NOT" THEN
      next;
      factor(x);
      IF NOT boolean(x) THEN
        COPY(p.true^, z)
      ELSE
        COPY(p.false^, z)
      END
    ELSIF str = "DEFINED" THEN
      next;
      IF sy # ident THEN
        syntaxError;
        z := "";
        RETURN
      END;
      v := ProgEnv.StringLength(str) # 0;
      next;
      IF v THEN
        COPY(p.true^, z)
      ELSE
        COPY(p.false^, z)
      END
    ELSIF skip THEN
      COPY(p.false^, z);
      next;
    ELSE
      IF ProgEnv.StringLength(str) # 0 THEN
        ProgEnv.String(str, z)
      ELSE
        z := "";
        notDefined
      END;
      next
    END
  END factor;

  PROCEDURE term(VAR z: Str);
  VAR
    y: Str;
    zv, sv: BOOLEAN;
  BEGIN
    factor(z);
    LOOP
      IF sy # ident THEN
        EXIT;
      ELSIF str = "AND" THEN
        zv := boolean(z);
        next;
        sv := skip;
        skip := NOT zv;
        factor(y);
        skip := sv;
        IF zv AND boolean(y) THEN
          COPY(p.true^, z)
        ELSE
          COPY(p.false^, z)
        END
      ELSE
        EXIT
      END
    END
  END term;

  PROCEDURE simple(VAR z: Str);
  VAR
    y: Str;
    zv, sv: BOOLEAN;
  BEGIN
    term(z);
    LOOP
      IF sy = '+' THEN
        next;
        term(y);
        Strings.Append(y, z)
      ELSIF sy # ident THEN
        EXIT
      ELSIF str = "OR" THEN
        zv := boolean(z);
        next;
        sv := skip;
        skip := zv;
        term(y);
        skip := sv;
        IF zv OR boolean(y) THEN
          COPY(p.true^, z)
        ELSE
          COPY(p.false^, z)
        END
      ELSE
        EXIT
      END
    END
  END simple;

  PROCEDURE expression(VAR z: Str);
  VAR
    y: Str;
    op: CHAR;
    res: BOOLEAN;
  BEGIN
    simple(z);
    IF (sy = "=") OR (sy = "#") OR (sy = "<") OR (sy = ">") THEN
      op := sy;
      Strings.Capitalize(z);
      next;
      simple(y);
      Strings.Capitalize(y);
      IF op = "=" THEN
        res := z = y
      ELSIF op = "#" THEN
        res := z # y
      ELSIF op = "<" THEN
        res := z < y
      ELSE (*op = ">"*)
        res := z > y
      END;
      IF res THEN
        COPY(p.true^, z)
      ELSE
        COPY(p.false^, z)
      END
    END
  END expression;

  PROCEDURE macros();
  VAR
    j: LONGINT;

    PROCEDURE put(ch: CHAR);
    VAR
      d: StringT;
    BEGIN
      IF j >= LEN(p.lineBf^) - 1 THEN
        NEW(d, LEN(p.lineBf^) * 2);
        p.lineBf[j] := 0X;
        COPY(p.lineBf^, d^);
        p.lineBf := d
      END;
      p.lineBf[j] := ch;
      INC(j);
      p.lineBf[j] := 0X
    END put;

    PROCEDURE puts(s-: ARRAY OF CHAR);
    VAR
      k: LONGINT;
    BEGIN
      k := 0;
      WHILE (k < LEN(s)) AND (s[k] # 0X) DO
        put(s[k]);
        INC(k)
      END
    END puts;

    PROCEDURE macro(VAR i: LONGINT);
    VAR
      s: Str;
      n, len: LONGINT;
      x: StringT;
      ch: CHAR;
    BEGIN
      n := 0;
      ch := p.srcBf[i];
      WHILE (ch # 0X) AND (ch # ')') DO
        IF n < LEN(s) - 1 THEN
          s[n] := ch;
          INC(n)
        END;
        INC(i);
        ch := p.srcBf[i]
      END;
      s[n] := 0X;
      IF ch # ')' THEN
        (* put everything back *)
        puts("$(");
        puts(s);
        RETURN
      END;
      INC(i);
      len := ProgEnv.StringLength(s);
      NEW(x, len + 1);
      ProgEnv.String(s, x^);
      puts(x^)
    END macro;

  VAR
    st, ch: CHAR;
    d: StringT;
    i, k: LONGINT;
  BEGIN
    i := 0;
    j := 0;
    st := 0X;
    p.lineBf[0] := 0X;
    LOOP
      ch := p.srcBf[i];
      INC(i);
      IF ch = 0X THEN
        RETURN
      END;
      IF ((ch = '"') OR (ch = "'")) AND (st = 0X) THEN
        st := ch;
        put(ch)
      ELSIF (st # 0X) AND (ch = st) THEN
        st := 0X;
        put(ch)
      ELSIF (st = 0X) AND (ch = '$') THEN
        ch := p.srcBf[i];
        INC(i);
        IF ch = '!' THEN
          DStr.Assign(p.fname^, d);
          FileName.GetDir(d^, d^);
          IF d[0] = 0X THEN
            DStr.Assign('.', d)
          END;
          k := LENGTH(d^) - 1;
          IF d[k] = '/' THEN
            d[k] := 0X
          END;
          puts(d^)
        ELSIF ch = '(' THEN
          macro(i)
        ELSIF ch = '$' THEN
          put('$')
        ELSIF ch = 0X  THEN
          put('$');
          RETURN
        ELSE
          put('$');
          put(ch)
        END
      ELSE
        put(ch)
      END
    END
  END macros;

  PROCEDURE oValue(nm-: Str): BOOLEAN;
  BEGIN
    IF (sy = '+') OR (sy = '-') THEN
      IF sy = '+' THEN
        setEnv(nm, "1")
      ELSE
        setEnv(nm, "")
      END;
      next;
      IF sy # 0X THEN
        syntaxError;
        RETURN FALSE
      END
    ELSIF sy = '=' THEN
      nextAll;
      setEnv(nm, str)
    ELSE
      syntaxError;
      RETURN FALSE
    END;
    RETURN TRUE
  END oValue;

VAR
  if: ScanIfPtr;
  v: Str;
BEGIN
  macros;
  err := FALSE;
  skip := FALSE;
  ps := 0;
  syps := 0;
  next;
  IF sy = '!' THEN
    next;
    IF sy # ident THEN
      IF on(p.ifs) THEN
        p.do
      END;
      RETURN TRUE
    ELSIF str = "IF" THEN
      next;
      NEW(if);
      if.else := FALSE;
      if.elsif := FALSE;
      if.up := p.ifs;
      expression(v);
      if.val := boolean(v);
      IF (sy # ident) OR (str # "THEN") THEN
        syntaxError;
        RETURN FALSE
      END;
      next;
      IF sy # 0X THEN
        syntaxError;
        RETURN FALSE
      END;
      p.ifs := if;
      RETURN NOT err
    ELSIF str = "ELSIF" THEN
      IF (p.ifs = NIL) OR p.ifs.else THEN
        syntaxError;
        RETURN FALSE
      END;
      next;
      NEW(if);
      if.else := FALSE;
      if.elsif := TRUE;
      if.up := p.ifs;
      expression(v);
      if.val := boolean(v);
      IF (sy # ident) OR (str # "THEN") THEN
        syntaxError;
        RETURN FALSE
      END;
      next;
      IF sy # 0X THEN
        syntaxError;
        RETURN FALSE
      END;
      p.ifs.else := TRUE;
      p.ifs := if;
      RETURN NOT err
    ELSIF str = "ELSE" THEN
      IF (p.ifs = NIL) OR p.ifs.else THEN
        syntaxError;
        RETURN FALSE
      END;
      next;
      IF sy # 0X THEN
        syntaxError;
        RETURN FALSE
      END;
      p.ifs.else := TRUE;
      RETURN TRUE
    ELSIF str = "END" THEN
      next;
      IF (p.ifs = NIL) OR (sy # 0X) THEN
        syntaxError;
        RETURN FALSE
      END;
      WHILE p.ifs.elsif DO
        p.ifs := p.ifs.up
      END;
      p.ifs := p.ifs.up;
      RETURN TRUE
    ELSIF NOT on(p.ifs) THEN
      RETURN TRUE
    ELSIF str = "MESSAGE" THEN
      next;
      expression(v);
      IF sy # 0X THEN
        syntaxError;
        RETURN FALSE
      END;
      Printf.printf("%s, %u: %s", p.fname^, p.lineNo, v);
      RETURN NOT err
    ELSIF (str = "SET") OR (str = "NEW") THEN
      next;
      IF sy # ident THEN
        syntaxError;
        RETURN FALSE
      END;
      v := str;
      next;
      RETURN oValue(v)
    END;
  ELSE
    IF NOT on(p.ifs) THEN
      RETURN TRUE
    END
  END;
  p.do();
  RETURN TRUE
END preprocessor;

--------------------------------------------------------------------------------
PROCEDURE (VAR s: ScanRed) readText(f: File);
VAR
  buf: ARRAY 512 (*ok*) OF CHAR;
  lm, i: LONGINT;
BEGIN
  s.in := f;
  s.ifs := NIL;
  DStr.Assign("FALSE", s.false);
  DStr.Assign("TRUE", s.true);
  lm := 1;
  NEW(s.lineBf, 128);
  NEW(s.srcBf, 128);
  LOOP
    s.lineNo := lm;
    s.srcBf[0] := 0X;
    LOOP
      LOOP
        TextIO.ReadString(f, buf);
        IF IOResult.ReadResult(f) # IOResult.allRight THEN
          EXIT
        END;
        DStr.Append(buf, s.srcBf)
      END;
      INC(lm);
      i := LENGTH(s.srcBf^);
      WHILE (i > 0) AND CharClass.IsWhiteSpace(s.srcBf[i - 1]) DO
        DEC(i)
      END;
      s.srcBf[i] := 0X;
      IF (i = 0) OR (s.srcBf[i - 1] # '\') OR
        (IOResult.ReadResult(f) # IOResult.endOfLine)
      THEN
        EXIT
      END;
      s.srcBf[i - 1] := 0X
    END;
    IF NOT s.preprocessor() THEN
      RETURN
    END;
    IF IOResult.ReadResult(f) = IOResult.endOfInput THEN
      IF s.ifs # NIL THEN
        s.writeError(-1)
      END;
      RETURN
    END;
    ASSERT(IOResult.ReadResult(f) = IOResult.endOfLine);
    TextIO.SkipLine(f)
  END
END readText;

--------------------------------------------------------------------------------
PROCEDURE readRedirection*(f: File; fname-: ARRAY OF CHAR);
VAR
  r: ScanRed;
BEGIN
  DStr.Assign(fname, r.fname);
  r.readText(f)
END readRedirection;

--------------------------------------------------------------------------------
PROCEDURE exeDirectory*(): StringT;
VAR
  dir: StringT;
BEGIN
  NEW(dir, ProgEnv.ProgramNameLength() + 1);
  ProgEnv.ProgramName(dir^);
  deleteNameAndExtension(dir);
  RETURN dir
END exeDirectory;

--------------------------------------------------------------------------------
PROCEDURE defaultLookup*(name-: ARRAY OF CHAR; VAR fname: StringT): BOOLEAN;
VAR
  found: BOOLEAN;
BEGIN
  found := FileSys.Exists(name);
  IF found THEN
    DStr.Assign(name, fname)
  ELSE
    fname := exeDirectory();
    DStr.Append(name, fname);
    found := FileSys.Exists(fname^);
    IF NOT found THEN
      DStr.Assign(name, fname)
    END
  END;
  RETURN found
END defaultLookup;

--------------------------------------------------------------------------------
PROCEDURE useFirst*(nm-: ARRAY OF CHAR; VAR fname: StringT);
VAR
  l: NodePtr;
  dir, name, ext, nameAndExt: Filename;
  fullDir, sNm: StringT;
BEGIN
  IF NOT isFullPath(nm) THEN
    FileName.Get(nm, dir, name, ext);
    create("", name, ext, nameAndExt);
    NEW(fname, 256);
    l := nodes;
    convertCaseToHost(nameAndExt, sNm);
    WHILE l # NIL DO
      IF RegComp.Match(l.expr, sNm^, 0) THEN
        IF l.dirs = NIL THEN
          DStr.Assign(nm, fname)
        ELSIF dir # "" THEN
          DStr.Assign(l.dirs.path^, fullDir);
          DStr.Append(platform.pathSep, fullDir);
          DStr.Append(dir, fullDir);
          create(fullDir^, name, ext, fname^)
        ELSE
          create(l.dirs.path^, name, ext, fname^)
        END;
        RETURN
      END;
      l := l.next
    END
  END;
  DStr.Assign(nm, fname)
END useFirst;

--------------------------------------------------------------------------------
PROCEDURE _isPathSep(ch: CHAR): BOOLEAN;
(* Is "ch" environment variable PATH names separator? *)
BEGIN
  CASE ch OF
  '(', ')':
    RETURN TRUE |
  ';':
    RETURN TRUE | -- is used in xm configuration files
<* IF TARGET_FAMILY="UNIX" THEN *>
  ':':
    RETURN TRUE |
<* END *>
  ELSE
    RETURN FALSE
  END
END _isPathSep;


--------------------------------------------------------------------------------
-- 'from' is treated as a directory if it ends by path separator '/' or '\'
PROCEDURE getRelativeName * (from-: ARRAY OF CHAR; to-: ARRAY OF CHAR): StringT;
VAR full_from, full_to: ARRAY MAX_PATH_LENGTH OF CHAR;
    i, ps, min_length: LONGINT;
    result: StringT;
BEGIN
  IF LENGTH(to) = 0 THEN
    DStr.Assign("", result);
    RETURN result;
  ELSIF LENGTH(from) = 0  THEN
    DStr.Assign(to, result);
    RETURN result;
  END;

  FileSys.FullName(full_from, from);
  FileSys.FullName(full_to,   to);
  convertFromPortableNotation(full_from, full_from);
  convertFromPortableNotation(full_to,   full_to);

  IF full_from = full_to THEN
    IF full_to[LENGTH(full_to)-1] = platform.pathSep THEN
      DStr.Assign(".", result);
      DStr.Append(platform.pathSep, result);
    ELSE
      result := getFileName(full_to);
    END;
    RETURN result;
  END;
  min_length := LENGTH(full_from);
  IF min_length < LENGTH(full_to) THEN
    min_length := LENGTH(full_to);
  END;

<* IF TARGET_OS = "UNIX" THEN *>
  i := 1;
  ps:= 1;
<* ELSE *>
  IF full_from[0] # full_to[0] THEN
    DStr.Assign(full_to, result);
    RETURN result;
  END;
  i  := 2;
  ps := 3;
<* END *>

  WHILE (i < min_length) & (full_from[i] = full_to[i]) DO
    IF full_from[i] = platform.pathSep THEN
      ps := i;
    END;
    INC(i);
  END;
  IF ps < i THEN
    INC(ps);
  END;

  -- convert remainder directories in 'full_from' into set of '..\'
  IF i = LENGTH(full_from) THEN
    DStr.Assign("", result);
  ELSE
    DStr.Assign("", result);
    i := ps;
    WHILE i < LENGTH(full_from) DO
      IF full_from[i] = platform.pathSep THEN
        DStr.Append("..", result);
        DStr.Append(platform.pathSep, result);
      END;
      INC(i);
    END;
  END;

  -- append to 'result' the remainder path from 'full_to'
  Strings.Extract(full_to, ps, LENGTH(full_to) - ps, full_from);
  DStr.Append(full_from, result);

  RETURN result;
END getRelativeName;


--------------------------------------------------------------------------------
PROCEDURE GetCurDirRelativeName * (to-: ARRAY OF CHAR): StringT;
VAR path: ARRAY MAX_PATH_LENGTH OF CHAR;
    from: StringT;
BEGIN
  FileSys.GetCDName(path);
  DStr.Assign(path, from);
  DStr.Append(platform.pathSep, from);
  RETURN getRelativeName(from^, to);
END GetCurDirRelativeName;


--------------------------------------------------------------------------------
VAR
  i: LONGINT;
BEGIN
  DStr.Assign("", null);
  FOR i := 0 TO 255 DO
    isPathSep[CHR(i)] := _isPathSep(CHR(i))
  END;
  ASSERT(~isPathSep[0C])
END FileUtils.