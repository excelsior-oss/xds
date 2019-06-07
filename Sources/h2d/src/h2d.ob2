(* H2Def main module *** al 23-Oct-95 *)

<*+ MAIN *>
MODULE h2d;

IMPORT 
  sys  := SYSTEM,
  io   := Printf,
  pe   := ProgEnv,
  cfg  := H2DCfg,
  msg  := H2DMsg,
  objs := H2DObjs,
  file := H2DFile,
  parse:= H2DParse,
  gen  := H2DGen,
  adt,
  lstr := LongStrs;


TYPE
  INT = sys.INT32;

  Option = POINTER TO OptionDesc;
  OptionDesc = RECORD (adt.NamedElementDesc)
    value: lstr.String;
  END;

VAR
  configuration, header, definition: lstr.String;
  e: adt.Element;
  ne: adt.NamedElement;
  h: objs.Header;
  arg_num: INT;
  name_count: INT;
  project_list: adt.List;
  cstdlib: BOOLEAN;
  show_stat: BOOLEAN;

(* Command Line Options *)
CONST
  opt_undef   = 0;
  opt_on      = 1;
  prj_opt_p   = 2;
  prj_opt_prj = 3;

VAR
  prj_name    : lstr.String;
  filenames   : adt.List;
  prj_opt     : INT;
  o_opt       : INT;
  options     : adt.List;



PROCEDURE ParseCommandLine();


VAR i: INT;
    arg: lstr.String;
    ne : adt.NamedElement;
    name, value: ARRAY 100 OF CHAR;
    option : Option;

  PROCEDURE get_option(option: ARRAY OF CHAR; VAR name: ARRAY OF CHAR; VAR value: ARRAY OF CHAR);
  VAR i, j: INT;
  BEGIN
    i:= 1; j:= 0;
    WHILE (option[i] >= 'A') & (option[i] <= 'Z') OR
          (option[i] >= 'a') & (option[i] <= 'z') OR
          (option[i] >= '0') & (option[i] <= '9')
    DO
      name[j]:= CAP(option[i]);
      INC(i); INC(j);
    END;
    name[j]:= 0X;
    WHILE option[i] = ' ' DO INC(i) END;
    IF option[i] = '=' THEN
      INC(i);
      WHILE option[i] = ' ' DO INC(i) END;
      j:= 0;
      IF option[i] = '"' THEN
        INC(i);
        WHILE (option[i] # 0X) & (option[i] # '"') DO
          value[j]:= option[i];
          INC(i); INC(j);
        END;
        IF option[i] = 0X THEN
          msg.Error(71, name);
          RETURN;
        END;
        value[j]:= 0X;
      ELSE
        WHILE (option[i] # 0X) & (option[i] # ' ') DO
          value[j]:= option[i];
          INC(i); INC(j);
        END;
        value[j]:= 0X;
      END;
    ELSIF option[i] = '-' THEN
      COPY("Off", value);
    ELSIF option[i] = '+' THEN
      COPY("On", value);
    ELSE
      msg.Error(71, name);
      RETURN;
    END;
  END get_option;

BEGIN
  adt.NewList(options);
  arg_num:= sys.VAL(INT, pe.ArgNumber());
  i:= 0; arg:= NIL; prj_name:= NIL; adt.NewList(filenames);
  prj_opt:= opt_undef;
  o_opt  := opt_undef;

  WHILE i < arg_num DO
    lstr.Allocate(arg, pe.ArgLength(i) + 1);
    pe.GetArg( i, arg^ ); INC(i);
    IF arg[0] = '=' THEN
      IF arg[2] = 0X THEN
        IF CAP(arg[1]) = 'P' THEN
          IF prj_opt = opt_undef THEN
            prj_opt:= prj_opt_p;
            IF i < arg_num THEN
              lstr.Allocate(prj_name, pe.ArgLength(i) + 1);
              pe.GetArg( i, prj_name^ ); INC(i);
            ELSE
              (* error *)
              msg.Error(63, '');
            END;
          ELSE
            (* error *)
            msg.Error(72, '=p');
            RETURN;
          END;
        ELSIF CAP(arg[1]) = 'O' THEN
          IF o_opt = opt_undef THEN
            o_opt:= opt_on;
          ELSE
            (* error *)
            msg.Error(72, '=o');
            RETURN;
          END;
        ELSE
          msg.Error(73, arg^);
          RETURN;
        END;
      ELSE
        msg.Error(73, arg^);
        RETURN;
      END;
    ELSIF arg[0] = '-' THEN
      get_option(arg^, name, value); IF msg.WasError THEN RETURN END;
      IF name = 'PRJ' THEN
        IF prj_opt = opt_undef THEN
          prj_opt:= prj_opt_prj;
          lstr.Assign(value, prj_name);
        ELSE
          (* error *)
          msg.Error(72, '-prj');
          RETURN;
        END;
      ELSE
        NEW(option); option.SetName(name);
        lstr.Assign(value, option.value);
        options.Insert(option);
      END;
    ELSE
      adt.NewNamedElement(ne, arg^);
      filenames.Insert(ne);
    END;
  END;
END ParseCommandLine;


PROCEDURE get_name(VAR name: lstr.String): BOOLEAN;
VAR e: adt.Element;
BEGIN
  IF (project_list = NIL) OR (prj_opt = prj_opt_prj) THEN
    filenames.FindNext(e);
    IF e # NIL THEN
      name:= e(adt.NamedElement).name;
      cstdlib:= cfg.CStdLib;
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  ELSE
    project_list.FindNext(e);
    IF e # NIL THEN
      lstr.Assign(e(adt.NamedElement).name^, name);
      cstdlib:= e(objs.Module).cstdlib;
    END;
    RETURN e # NIL;
  END;
END get_name;

PROCEDURE Project();
VAR param: lstr.String;
BEGIN
  project_list:= NIL;
  NEW( param, pe.ArgLength(0) + 1 );
  pe.GetArg(0, param^);
  IF param^ = '=p' THEN
    IF arg_num = 2 THEN
      NEW(param, pe.ArgLength(1) + 1);
      pe.GetArg(1, param^);
      project_list:= parse.ParseProject(param^);
      project_list.Reset();
    ELSE
      (* error *)
      msg.Error(63, '');
      HALT;
    END;
  END;
END Project;

PROCEDURE SetOptions();
VAR e: adt.Element;
    err: INT;
BEGIN
  IF ~options.IsEmpty() THEN
    options.FindFirst(e);
    WHILE e # NIL DO
      IF (e(Option).name^ # 'CTYPE') & (e(Option).name^ # 'M2TYPE') THEN
        err:= cfg.GetInfo(e(Option).name^, e(Option).value^);
        IF err >= 0 THEN
          (* error *)
          msg.Error(err, e(Option).name^);
          RETURN;
        END;
      ELSE
        msg.Error(71, e(Option).name^);
        RETURN;
      END;
      options.FindNext(e);
    END;
  END;
END SetOptions;

BEGIN
  ParseCommandLine;
  IF ~msg.WasError THEN
    IF arg_num = 0 THEN msg.Help() END;
    file.CreateName('', file.ProgramName^, 'cfg', configuration);
    parse.ParseConfig(configuration^);
    IF prj_opt # opt_undef THEN
      project_list:= parse.ParseProject(prj_name^);
      project_list.Reset();
      IF prj_opt # prj_opt_p THEN project_list:= NIL END;
    END;
    SetOptions;
    IF ~msg.WasError THEN
      show_stat:= FALSE;
      filenames.Reset();
      WHILE get_name(header) DO
        show_stat:= TRUE;
        msg.WasError:= FALSE;
        definition:= NIL;
        gen.GetOutputFileName(header^, definition);
        NEW(ne); ne.SetName(header^);
        objs.Headers.Find(ne, e);
        IF ( ~msg.WasError ) &
           ( ( ~file.Exist(definition^) OR cfg.Retranslate ) & ( e = NIL )
           ) THEN
          IF cfg.HeadersMerging THEN
            adt.NewList(objs.Headers);
          END;
          objs.CurrentHeader:= objs.SuperHeader;
          parse.Parse(header^, cstdlib);

          objs.Headers.FindLast(e);
          WHILE e # NIL DO
            h:= e(objs.Header);
            gen.GetOutputFileName(h.name^, definition);
            IF ~h.generated & (~file.Exist(definition^) OR cfg.Retranslate) THEN
              h.stat.ShowHeader();
              IF h.parsed THEN
                msg.WasError:= FALSE;
                gen.Generate(h);
              END;
              h.generated:= TRUE;
              h.stat.ShowFooter();
            END;
            objs.Headers.FindPrev(e);
          END;
        END;
      END;
      IF show_stat THEN
        objs.SuperHeader.stat.ShowHeader();
        objs.SuperHeader.stat.ShowFooter();
        msg.GlobalStatistics();
      END;
      IF o_opt = opt_on THEN cfg.PrintOptions() END;
    END;
  END;
END h2d.
