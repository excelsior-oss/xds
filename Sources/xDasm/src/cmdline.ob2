MODULE cmdline;

IMPORT
  SYSTEM,
  adt,
  lstr:= LongStrs,
  env:= ProgEnv,
  io:= Printf,
  opts:= Options;

TYPE INT * = SYSTEM.INT32;

TYPE
  OptionP = POINTER TO OptionDesc;
  OptionDesc = RECORD (adt.NamedElementDesc)
  END;

  BoolOptionP = POINTER TO BoolOptionDesc;
  BoolOptionDesc = RECORD (OptionDesc)
    val, default_val: BOOLEAN;
  END;

  StrEquationP = POINTER TO StrEquationDesc;
  StrEquationDesc = RECORD (OptionDesc)
    val, default_val: lstr.String;
  END;


VAR
  filenames * : adt.List;
  options     : adt.Tree;
  filenames_id: INT;

PROCEDURE Error (fmt-: ARRAY OF CHAR; SEQ x: SYSTEM.BYTE);
BEGIN
  io.printf(fmt, x);
  HALT;
END Error;

PROCEDURE GetFileName * (VAR name: lstr.String);
VAR local_id: INT;
    e: adt.Element;
BEGIN
  filenames.Backup(local_id);
  filenames.Restore(filenames_id);
  filenames.FindNext(e);
  IF e # NIL THEN
    lstr.Assign(e(adt.NamedElement).name^, name);
  ELSE
    lstr.Deallocate(name);
  END;
  filenames.Backup(filenames_id);
  filenames.Restore(local_id);
END GetFileName;

PROCEDURE BoolOption * (name: ARRAY OF CHAR): BOOLEAN;
VAR ne: adt.NamedElement;
    e : adt.Element;
BEGIN
  adt.NewNamedElement(ne, name);
  options.Find(ne, e);
  IF (e # NIL) & (e IS BoolOptionP) THEN
    adt.Deallocate(ne);
    RETURN e(BoolOptionP).val;
  ELSE
    ASSERT(FALSE);
  END;
END BoolOption;

PROCEDURE StrEquation * (name: ARRAY OF CHAR; VAR val: lstr.String);
VAR ne: adt.NamedElement;
    e : adt.Element;
BEGIN
  adt.NewNamedElement(ne, name);
  options.Find(ne, e);
  IF (e # NIL) & (e IS StrEquationP) THEN
    adt.Deallocate(ne);
    lstr.Assign(e(StrEquationP).val^, val);
  ELSE
    ASSERT(FALSE);
  END;
END StrEquation;

PROCEDURE Parse();
VAR
  i, num: INT;
  arg: lstr.String;

  PROCEDURE parse(arg: ARRAY OF CHAR; default_val: BOOLEAN);
  VAR s: lstr.String;
      i, j: INT;
      ne: adt.NamedElement;
      e : adt.Element;
      b_op: BoolOptionP;
      s_eq: StrEquationP;
  BEGIN
    s:= NIL; lstr.Allocate(s, lstr.Length(arg) + 1);
    IF arg[0] = '-' THEN
      i:= 1; j:= 0;
      WHILE (arg[i] # 0X) &
            ((arg[i] >= 'A') & (arg[i] <= 'Z') OR
             (arg[i] >= 'a') & (arg[i] <= 'z'))
      DO
        s[j]:= arg[i]; INC(i); INC(j);
      END;
      s[j]:= 0X;
      IF ((arg[i] = '+') OR (arg[i] = '-')) & (arg[i+1] = 0X) THEN
        NEW(b_op); b_op.SetName(s^);
        b_op.val:= arg[i] = '+';
        options.Find(b_op, e);
        IF default_val THEN
          ASSERT(e # NIL);
          e(BoolOptionP).default_val:= b_op.val;
        ELSIF e = NIL THEN
          options.Insert(b_op);
        ELSE
          e(BoolOptionP).val:= b_op.val;
        END;
      ELSIF arg[i] = '=' THEN
        NEW(s_eq); s_eq.SetName(s^);
        lstr.Allocate(s_eq.val, lstr.Length(arg) - i);
        INC(i); j:= 0;
        WHILE arg[i] # 0X DO
          s_eq.val[j]:= arg[i]; INC(i); INC(j);
        END;
        s_eq.val[j]:= 0X;
        options.Find(s_eq, e);
        IF default_val THEN
          e(StrEquationP).default_val:= s_eq.val;
        ELSIF e = NIL THEN
          options.Insert(s_eq);
        ELSE
          e(StrEquationP).val:= s_eq.val;
        END;
      ELSE
        adt.NewNamedElement(ne, s^);
        options.Find(ne, e);
        IF e # NIL THEN
          IF e IS BoolOptionP THEN
            e(BoolOptionP).val:= e(BoolOptionP).default_val;
          ELSIF e IS StrEquationP THEN
            e(StrEquationP).val:= e(StrEquationP).default_val;
          END;
        ELSE
          (* error *)
          Error("Bad option setting: %s\n", arg);
        END;
      END;
    ELSE
      adt.NewNamedElement(ne, arg);
      filenames.Insert(ne);
    END;
  END parse;

  PROCEDURE getOptions(vals: ARRAY OF CHAR; default_val: BOOLEAN);
  VAR s: ARRAY 100 OF CHAR;
      i, j: INT;
  BEGIN
    i:= 0;
    LOOP
      j:= 0;
      WHILE (vals[i] # 0X) & (vals[i] # ';') DO
        s[j]:= vals[i]; INC(i); INC(j);
      END;
      s[j]:= 0X;
      IF j # 0 THEN parse(s, default_val) END;
      IF vals[i] = ';' THEN
        INC(i);
      ELSE
        EXIT;
      END;
    END;
  END getOptions;
BEGIN
  adt.NewList(filenames);
  adt.NewTree(options);
  arg:= NIL;

  getOptions(opts.Options, FALSE);
  getOptions(opts.Values, TRUE);

  num:= env.ArgNumber();
  FOR i:= 1 TO num DO
    lstr.Allocate(arg, env.ArgLength(i-1) + 1);
    env.GetArg(i-1, arg^);
    parse(arg^, FALSE);
  END;
  filenames.Reset();
  filenames.Backup(filenames_id);
END Parse;

BEGIN
  Parse();
END cmdline.
