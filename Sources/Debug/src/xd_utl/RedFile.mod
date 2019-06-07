<* Storage + *>

IMPLEMENTATION MODULE RedFile;

IMPORT io  := InOut;
IMPORT sys := SYSTEM;
IMPORT fmt := FormStr;
IMPORT arg := ProgEnv;
IMPORT rf  := RndFile;
IMPORT ioc := IOChan;
IMPORT cc  := ChanConsts;
IMPORT rio := RawIO;
IMPORT fs  := FileSys;
IMPORT xfp := xFilePos;

IMPORT fil := File;
IMPORT xs  := xStr;


CONST
  CR   = 15C;        (* разделитель строк *)
  LF   = 12C;        (* разделитель строк *)
  CRLF = 15C;        (* разделитель строк *)
  EOF  = CHR(1AH);   (* ^Z - конец файла *)
  EL   = 0C;         (* конец строки *)


TYPE
  CHARSET = SET OF CHAR;

  Stroka = POINTER TO ARRAY [0..0FFFEH] OF CHAR;

  OneRedStr = RECORD
         PName : ARRAY [0..13] OF CHAR;
         Apath : POINTER TO ARRAY OF Stroka;
         Npath : CARDINAL;
       END;

  Redirection = RECORD
                  buff  : POINTER TO ARRAY OF CHAR;
                  Ap    : POINTER TO ARRAY OF OneRedStr;
                  Np    : CARDINAL;
                END;

VAR
  PathSep : CHAR;
  RedFilePresent: BOOLEAN;
  RedList : Redirection;

PROCEDURE RedirectionCreated (): BOOLEAN;
(* Создан ли уже redirection *)
BEGIN
  RETURN RedFilePresent;
END RedirectionCreated;

PROCEDURE FindRedFile (VAR s:ARRAY OF CHAR) : BOOLEAN;
VAR
  fname : ARRAY [0..64] OF CHAR;
  p     : CARDINAL;
BEGIN
  p := 0;
  LOOP
    IF (s[p] = PathSep) OR (s[p] = ':') THEN
      IF fs.Exists(s) THEN
        RETURN TRUE;
      ELSE
        RETURN FALSE;
      END;
    END;
    INC(p);
    IF p = LENGTH(s) THEN EXIT; END;
  END;
  IF p = LENGTH(s) THEN
    p := 0;
    LOOP
      IF (s[p] = '.') THEN EXIT; END;
      INC(p);
      IF p = LENGTH(s) THEN
        xs.Append('.RED',s);
        EXIT;
      END;
    END;
  END;
  IF fs.Exists(s) THEN RETURN TRUE; END;
  arg.ProgramName(fname); (* взять полное имя программы   *)
  p := LENGTH(fname)-1;
  LOOP
    IF (fname[p] = PathSep) THEN EXIT; END;
    DEC(p);
  END;
  fname[p+1] := EL; (* Оставить PathSep *)
  xs.Append(s,fname);
  COPY(fname,s);
  RETURN fs.Exists(s);
END FindRedFile;


PROCEDURE Create(name-: ARRAY OF CHAR) : RedirectionResults;
VAR
  file  : ioc.ChanId;
  res   : cc.OpenResults;
  size  : CARDINAL;
  flag  : BOOLEAN;
  i,j,k : CARDINAL;
  fname : ARRAY [0..128] OF CHAR;
  Pos   : rf.FilePos;


PROCEDURE SkipBlanks(VAR k:CARDINAL);
BEGIN
  WHILE (RedList.buff^[k] <= ' ') AND (RedList.buff^[k] # 0C) DO INC(k); END;
END SkipBlanks;


PROCEDURE ClearRedirection();
VAR i : CARDINAL;
BEGIN
  RedFilePresent := FALSE;
  IF (RedList.Ap # NIL)  THEN
    FOR i := 0 TO  RedList.Np - 1 DO
      IF (RedList.Ap^[i].Apath # NIL) THEN
        DISPOSE(RedList.Ap^[i].Apath);
        RedList.Ap^[i].Apath := NIL;
    END;
    END;
    DISPOSE(RedList.buff);
    DISPOSE(RedList.Ap);
    RedList.Ap := NIL;
  END;
END ClearRedirection;


BEGIN
  IF RedirectionCreated() THEN
    ClearRedirection();
  END;
  COPY(name,fname);
  IF NOT FindRedFile(fname) THEN RETURN RedFileNotFound; END;
  rf.OpenOld(file,fname,rf.raw,res);
  IF (res # cc.opened) THEN RETURN CantOpen; END;
  Pos := rf.EndPos(file);
  IF NOT xfp.PosToCard(size,Pos) OR (size=0) THEN
    rf.Close(file);
    RETURN RedFileEmpty;
  END;
  NEW(RedList.buff,size+1); (* лишний байт, если файл кончился "по размеру" *)
  rio.Read(file,RedList.buff^);
  rf.Close(file);
  RedList.Np := 0;
  i := 0;
  RedList.buff^[size] := CR;
  flag := TRUE;
  LOOP (* Подсчет числа непустых строк *)
    IF (RedList.buff^[i] = CR) OR (RedList.buff^[i] = LF) THEN
      RedList.buff^[i] := 0C;
      flag := TRUE;
      IF (i>=size) THEN EXIT; END;
    ELSIF (RedList.buff^[i] > 40C) AND flag THEN
      INC(RedList.Np);
      flag := FALSE;
    END;
    INC(i);
  END; (* Подсчет числа непустых строк закончился *)
  IF RedList.Np = 0 THEN RETURN RedFileEmpty; END;
  NEW(RedList.Ap,RedList.Np);
  RedList.Np := 0; (* Счетчик строк *)
  i := 0;    (* Счетчик символов *)
  LOOP (* Разбор строк в red-файле *)
    IF (RedList.buff^[i] <> EL) THEN
      SkipBlanks(i);
      IF RedList.buff^[i] <> EL THEN (* Если строка не вся из пробелов *)
        j := 0;
        LOOP (* Копируем имя файла *)
          IF RedList.buff^[i] IN CHARSET{EL,' ','='} THEN EXIT END;
          RedList.Ap^[RedList.Np].PName[j] := RedList.buff^[i];
          INC(j);
          INC(i);
        END;
        IF (j = 0) THEN RETURN WrongPattern; END; (* кривой шаблон *)
        RedList.Ap^[RedList.Np].PName[j] := EL;
        (* Имя и расширение прочитали *)
        SkipBlanks(i);
        IF RedList.buff^[i] = '=' THEN (* Далее идет список путей поиска *)
          INC(i);
          SkipBlanks(i);
          IF (RedList.buff^[i] = EL) THEN RETURN EmptyListPath; END;
          j := i; (* i на старом месте *)
          RedList.Ap^[RedList.Np].Npath := 1;
          (* Считаем сколько путей в списке *)
          k := j-1;
          WHILE RedList.buff^[j] <> EL DO
            IF RedList.buff^[j] = ';' THEN
              IF j-k <= 1 THEN RETURN EmptyListPath; END;
              INC(RedList.Ap^[RedList.Np].Npath);
              k := j;
            END;
            INC(j);
          END; (* Подсчитали сколько путей *)
          IF RedList.Ap^[RedList.Np].Npath = 0 THEN RETURN EmptyListPath; END;;
          (* Аллоцировали сколько надо - по числу путей *)
          NEW(RedList.Ap^[RedList.Np].Apath,RedList.Ap^[RedList.Np].Npath);
          (* Теперь вернулись на старое место, т.е. перед списком путей *)
          RedList.Ap^[RedList.Np].Npath := 1;
          (* Сюда косит первый элемент *)
          RedList.Ap^[RedList.Np].Apath^[RedList.Ap^[RedList.Np].Npath-1] := sys.ADR(RedList.buff^[i]);
          WHILE RedList.buff^[i] <> EL DO
            IF RedList.buff^[i] = ';' THEN (* Закончился очередной путь  *)
              RedList.buff^[i] := EL;
              INC(i);
              IF RedList.buff^[i] <> EL THEN
                 SkipBlanks(i); (* Пропустим пробелы *)
                 (* Суда косит очередной элемент *)
                 RedList.Ap^[RedList.Np].Apath^[RedList.Ap^[RedList.Np].Npath] := sys.ADR(RedList.buff^[i]);
                INC(RedList.Ap^[RedList.Np].Npath);
              END;
            ELSE
              INC(i);
            END;
          END;
        ELSE
          RETURN EquivExpected; (* хорошо бы поставить "=" ... *)
        END;
        INC(RedList.Np);
      END;
    END;
    IF i >= size THEN
      EXIT
    ELSE
      INC(i);
    END;
  END;
  RedFilePresent := TRUE;
  RETURN Ok;
END Create;



CONST
  red_file_ext = 'RED';  (* Расширение имени red-файла                       *)

PROCEDURE InitRedirection;
VAR
  res: RedirectionResults;
  pro_fname,
  red_fname: xs.String;
BEGIN
  arg.ProgramName(pro_fname);                   (* Взять полное имя программы *)
  fil.ExtractFileName(pro_fname, red_fname);    (* Только имя без пути        *)
  fil.ChangeExtension(red_fname, red_file_ext); (* Сменить расширение         *)
  (* Создание red-файла *)
  res := Create(red_fname);
  MessageCreate(res, red_fname);
END InitRedirection;


PROCEDURE FindMatch(s-:ARRAY OF CHAR; i0:CARDINAL) : CARDINAL;
(* Поиск шаблона в списке с i0 входа, вернет номер входа, где найден шаблон *)
VAR
  i : CARDINAL;
BEGIN
  (* По всем входам *)
  WITH RedList DO
    FOR i := i0 TO Np-1 DO
      IF xs.Match(s,Ap^[i].PName) THEN RETURN i END;
    END;
  END;
  RETURN MAX(CARDINAL);
END FindMatch;


PROCEDURE Read (fname-: ARRAY OF CHAR;VAR fullname: ARRAY OF CHAR) : RedirectionResults;
VAR
  i,i1,j : CARDINAL;
  f: BOOLEAN;
BEGIN
  COPY(fname, fullname);
 <* PUSH *>
 <* WOFF903+ *>
  i := xs.CharPos (fname, ':', f);
 <* POP *>
  IF f THEN
    IF fs.Exists(fname) THEN
      RETURN Ok;
    ELSE
      RETURN NotFound;
    END;
  END;
  IF RedFilePresent THEN
    i := 0;
    LOOP
      i1 := FindMatch (fname, i); (* Найти подходящий вход в список *)
      IF i1 >= RedList.Np THEN EXIT; END;
      i := i1;
      FOR j:=0 TO RedList.Ap^[i].Npath-1 DO
        COPY(RedList.Ap^[i].Apath^[j]^, fullname);
        IF LENGTH(fullname) > 0 THEN
          IF (fullname[LENGTH(fullname)-1] <> PathSep) AND (fullname[LENGTH(fullname)-1] <> ':') THEN
            xs.Append (PathSep, fullname);
          END;
          xs.Append (fname, fullname);
          IF fs.Exists (fullname) THEN RETURN Ok; END;
        END;
      END;
      INC(i);
    END;
  END;
  IF fs.Exists(fname) THEN
    COPY(fname, fullname);
    RETURN Ok;
  ELSE
    RETURN NotFound;
  END;
END Read;


PROCEDURE ReadEx (basepath-, fname-: ARRAY OF CHAR; VAR fullname: ARRAY OF CHAR): RedirectionResults;
VAR
  fullpath: ARRAY [0..1023] OF CHAR;
BEGIN
  fmt.print (fullpath, "%s%s", basepath, fname);
  IF Read (fullpath, fullname) = Ok THEN
    RETURN Ok;
  END;
  IF Read (fname, fullname) = Ok THEN
    RETURN Ok;
  END;
  fil.ExtractFileName (fname, fullpath);
  IF Read (fullpath, fullname) = Ok THEN
    RETURN Ok;
  END;
  RETURN NotFound;
END ReadEx;


PROCEDURE Write (fname-:ARRAY OF CHAR;VAR fullname:ARRAY OF CHAR);
VAR
  i: CARDINAL;
  f: BOOLEAN;
BEGIN
  COPY(fname,fullname);
  IF RedFilePresent THEN
   <* PUSH *>
   <* WOFF903+ *>
    i := xs.CharPos (fname, ':', f);
   <* POP *>
    IF f THEN
      COPY(fname, fullname);
      RETURN;
    END;
    i := FindMatch(fname,0); (* Найти подходящий вход в список *)
    IF i < RedList.Np THEN
      COPY(RedList.Ap^[i].Apath^[0]^, fullname);
      IF LENGTH(fullname) > 0 THEN
        IF (fullname[LENGTH(fullname)-1] <> PathSep) AND (fullname[LENGTH(fullname)-1] <> ':') THEN
          xs.Append (PathSep, fullname);
        END;
        xs.Append (fname, fullname);
      END;
    END;
  END;
END Write;


PROCEDURE MessageCreate (res: RedirectionResults; red_fname-: ARRAY OF CHAR);
VAR
  msg, buf: xs.String;
BEGIN
  IF res <> Ok THEN
    CASE res OF
    | RedFileNotFound: RETURN; -- COPY('Redirection file %s not found.',msg);
    | CantOpen       : COPY('Error opened redirection file %s.',msg);
    | RedFileEmpty   : COPY('Redirection file %s is empty.',msg);
    | WrongPattern   : COPY('Incorrect pattern of file name in redirection file %s.',msg);
    | EquivExpected  : COPY('Expected "=" in redirection file %s.',msg);
    | EmptyListPath  : COPY('Empty path in redirection-file %s.',msg);
    ELSE
      ASSERT(FALSE);
    END;
    fmt.print(buf, msg, red_fname);
    io.WriteString(buf);
    io.WriteLn;
  END;
END MessageCreate;


BEGIN
  RedList.Np := 0; (* Пустой! *)
  RedFilePresent := FALSE;
  PathSep := fil.GetFileSepChar ();
END RedFile.
