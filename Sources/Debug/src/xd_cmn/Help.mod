<* Storage+ *>

IMPLEMENTATION MODULE Help;

IMPORT arg := ProgEnv;
IMPORT sys := SYSTEM;

IMPORT fil := File;
IMPORT        xStr;
IMPORT txt := Texts;


CONST
  help_ext = 'HLP';

TYPE
  SECTION = RECORD
              Name: xStr.txt_ptr;
              L1  : CARDINAL;
              LN  : CARDINAL;
            END;
  PASECTION = POINTER TO ARRAY OF SECTION;

  HELP = RECORD
           Text    : txt.TEXT;
           Sections: PASECTION;
         END;

VAR
  Help: HELP;


PROCEDURE CreateHelp;
VAR
  i, m, k: CARDINAL;
  s: xStr.txt_ptr;
  prog_fname: xStr.STRING;
  help_fname: xStr.String;


  PROCEDURE Replace;
  VAR
    i : CARDINAL;
  BEGIN
    FOR i := 0 TO LENGTH(s^) DO
      IF s^[i] = '}' THEN s^[i] := 0C; RETURN; END;
    END;
  END Replace;

BEGIN
  xStr.alloc_str(prog_fname, arg.ProgramNameLength());
  arg.ProgramName(prog_fname^);
  fil.ExtractFileName(prog_fname^,help_fname);
  xStr.dealloc_str(prog_fname);
  fil.ChangeExtension(help_fname, help_ext);
  WITH Help DO
    txt.Open(Text, help_fname);
    IF Text <> txt.nil THEN
      m := txt.LastLine(Text);
      IF m = 0 THEN RETURN; END;
      DEC(m);
      k := 0;
      FOR i := 0 TO m DO
        txt.GetLine(Text,i,s);
        IF (s^[0] = '#') AND (s^[1] = '{') THEN INC(k); END;
      END;
      IF k = 0 THEN
         txt.Close(Text);
         RETURN;
      END;
      NEW(Sections,k);
      k := 0;
      FOR i := 0 TO m DO
        txt.GetLine(Text,i,s);
        IF (s^[0] = '#') AND (s^[1] = '{') THEN
          Replace;
          WITH Sections^[k] DO
            Name := sys.ADDADR(s,2);
            L1 := i+1;
          END;
          INC(k);
        END;
      END;
      FOR i := 1 TO k-1 DO
        Sections^[i-1].LN := Sections^[i].L1-2;
      END;
      Sections^[k-1].LN := m;
    END;
  END;
END CreateHelp;


(* Всего секций в Help! *)
PROCEDURE GetNSection (): CARDINAL;
VAR
  N: CARDINAL;
BEGIN
  N := 0;
  WITH Help DO
    IF Sections # NIL THEN N := HIGH(Sections^)+1; END;
  END;
  RETURN N;
END GetNSection;


(* Выдать количество строк в секции *)
PROCEDURE GetNLine (N: CARDINAL): CARDINAL;
VAR
  n: CARDINAL;
BEGIN
  n := 0;
  WITH Help DO
    IF Sections # NIL THEN
      IF (N # 0) AND (N <= GetNSection()) THEN
        WITH Sections^[N-1] DO
          n := LN - L1 + 1;
        END;
      END;
    END;
  END;
  RETURN n;
END GetNLine;


(* Выдать i строку секции, если i=0 - вернет имя секции *)
(* Номера строк  - [1..GetNLine]                        *)
PROCEDURE GetLine (N,i: CARDINAL): xStr.txt_ptr;
CONST
  empty = "";
VAR
  s: xStr.txt_ptr;
BEGIN
  s := sys.ADR(empty);
  WITH Help DO
    IF Sections # NIL THEN
      IF (N # 0) AND (N <= GetNSection()) THEN
        WITH Sections^[N-1] DO
          IF i = 0 THEN
            s := Name;
          ELSIF i <= GetNLine(N) THEN
            txt.GetLine(Text, L1+i-1, s);
          END;
        END;
      END;
    END;
  END;
  RETURN s;
END GetLine;


BEGIN
  Help.Text     := txt.nil;
  Help.Sections := NIL;
END Help.
