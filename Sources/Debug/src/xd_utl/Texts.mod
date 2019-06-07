<* Storage+ *>

IMPLEMENTATION MODULE Texts;

IMPORT sys := SYSTEM;
IMPORT rf  := RndFile;
IMPORT ioc := IOChan;
IMPORT icc := IOConsts;
IMPORT cc  := ChanConsts;
IMPORT xfp := xFilePos;
IMPORT fmt := FormStr;
IMPORT fmo := FormOut;
IMPORT pla := platform;

IMPORT red := RedFile;
IMPORT xStr;

FROM Storage IMPORT ALLOCATE, DEALLOCATE;

CONST
  CR  = 15C;        (* разделитель строк *)
  LF  = CHR(10);    (* перевод строки    *)
  EOF = CHR(1AH);   (* ^Z - конец файла  *)

  TEXT_MAGIC = 59657279H;

  empty_str = '';
  NOLINE    = 'line is not available';


TYPE
  TEXT    = POINTER TO TextRec;

  LINE    = CARDINAL;

  BUFFER  = POINTER TO ARRAY OF CHAR;

  STRINGS = POINTER TO ARRAY OF LINE;

  TextRec = RECORD
              magic   : CARDINAL;                 -- идентификатор
              filename: xStr.STRING;              -- имя файла с текстом
              buffer  : BUFFER;                   -- весь текст
              strings : STRINGS;                  -- строки текста
              N       : CARDINAL;                 -- число строк в тексте
            END;


CONST
  NEW_TEXT_BUFFER = 400H; -- #0
  NEW_TEXT_STRING =  40H; -- #0

(* Создается новый текст *)
PROCEDURE New (VAR t: TEXT);
BEGIN
  NEW(t);
  WITH t^ DO
    magic := TEXT_MAGIC;
    filename := NIL;
    NEW(buffer, NEW_TEXT_BUFFER);
    buffer^[0] := 0C;
    NEW(strings, NEW_TEXT_STRING);
    strings^[0] := 0;
    N := 0;
  END;
END New;


PROCEDURE Open (VAR t: TEXT; name-: ARRAY OF CHAR);
(* создается новый текст на базе указанного файла      *)
VAR
  file: ioc.ChanId;
  res : cc.OpenResults;
  Pos : rf.FilePos;
  str : xStr.String;
  size: CARDINAL;
  ofs : CARDINAL;
  num : CARDINAL;
  tmp : TextRec;

  MODULE CloseFile;
  IMPORT res, file, rf, cc;
  BEGIN
  FINALLY
    IF res = cc.opened THEN
      rf.Close (file);
    END;
  END CloseFile;

BEGIN
  t := NIL;
  res := cc.otherProblem;
  IF red.Read (name, str) = red.NotFound THEN
    NotFound (name);
    RETURN;
  END;
  rf.OpenOld (file, str, rf.raw, res);
  IF res = cc.noSuchFile THEN
    NotFound (str);
    RETURN;
  ELSIF res # cc.opened THEN
    ReadError (str);
    RETURN;
  END;
  Pos := rf.EndPos(file);
  IF NOT xfp.PosToCard (size, Pos) OR (size = 0) THEN
    FileEmpty (str);
    RETURN;
  END;
  WITH tmp DO
    magic := TEXT_MAGIC;
    NEW(buffer, size+1); (* лишний байт, если файл кончился "по размеру" *)
    IF buffer = NIL THEN
      ReadError (str);
      RETURN;
    END;
    ioc.RawRead(file, sys.ADR(buffer^), size, num);
    xStr.alloc_from (filename, str);
  END;
  IF (ioc.ReadResult(file) # icc.allRight) OR (size <> num) THEN
    WITH tmp DO
      xStr.dealloc_str (filename);
      DISPOSE(buffer);
    END;
    ReadError (str);
    RETURN;
  END;

  WITH tmp DO
    buffer^[HIGH(tmp.buffer^)] := EOF;
    ofs := 0; -- смещение в тексте
    num := 1;  -- текущее кол-во строк
    -- Подсчет числа строк
    LOOP
      CASE buffer^[ofs] OF
      | EOF:
        EXIT;
      | CR:
        IF buffer^[ofs+1] = LF THEN INC(ofs); END;
        INC(num);
      | LF:
        INC(num);
      ELSE
      END;
      INC(ofs);
    END;
    -- Построение ключа по строкам
    NEW(strings, num);
    ofs := 0; -- смещение в тексте
    num := 0;  -- текущая позиция в ключе
    strings^[num] := 0;
    LOOP
      CASE buffer^[ofs] OF
      | EOF:
        buffer^[ofs] := 0C;
        EXIT;
      | CR:
        buffer^[ofs] := 0C;
        IF buffer^[ofs+1] = LF THEN INC(ofs); END;
        INC(num);
        strings^[num] := ofs+1;
      | LF:
        buffer^[ofs] := 0C;
        INC(num);
        strings^[num] := ofs+1;
      ELSE
      END;
      INC(ofs);
    END;
  END;
  NEW(t);
  t^ := tmp;
  t^.N := num+1;
END Open;


PROCEDURE Close(VAR t: TEXT);
(* указанный текст уничтожается *)
BEGIN
  IF t # NIL THEN
    WITH t^ DO
      ASSERT(magic = TEXT_MAGIC);
      magic := 0;
      IF filename # NIL THEN
        xStr.dealloc_str(filename);
      END;
      IF buffer # NIL THEN
        DISPOSE(buffer);
      END;
      IF strings # NIL THEN
        DISPOSE(strings);
      END;
    END;
    DISPOSE(t);
    t := NIL;
  END;
END Close;


PROCEDURE LastLine(t: TEXT): CARDINAL;
(* Выдает номер последней строки *)
BEGIN
  IF t = NIL THEN
    RETURN 0
  ELSE
    WITH t^ DO
      ASSERT(magic = TEXT_MAGIC);
      RETURN N;
    END;
  END;
END LastLine;


PROCEDURE GetLine(t: TEXT; i: CARDINAL; VAR s: txt_ptr);
(* выдает "текущий" указатель на i-ую строку текста t  *)
(* если строки нет, то выдает пустую строку - длины 0  *)
BEGIN
  IF t = NIL THEN
    s := sys.ADR(empty_str);
  ELSE
    WITH t^ DO
      ASSERT(magic = TEXT_MAGIC);
      IF i < N THEN (* такая строка в тексте есть *)
        s := sys.ADR(buffer^[strings^[i]]);
      ELSE
        s := sys.ADR(NOLINE);
      END;
    END;
  END;
END GetLine;


(* Добавляет строку в конец текста *)
PROCEDURE AddLine (t: TEXT; f-: ARRAY OF CHAR; SEQ args: sys.BYTE);
VAR
  line    : txt_ptr;
  size    : CARDINAL;
  occupied: CARDINAL;
  required: CARDINAL;
  tmp     : BUFFER;
  str     : STRINGS;
  s       : String;
BEGIN
  IF t # NIL THEN
    fmt.print (s, f, args);
    WITH t^ DO
      ASSERT(magic = TEXT_MAGIC);
      required := LENGTH(s)+1;
      size := HIGH(buffer^)+1;
      IF N = 0 THEN
        occupied := 0;
      ELSE
        GetLine (t, N-1, line);
        occupied := strings^[N-1] + LENGTH(line^) + 1;
      END;
      IF size - occupied < required THEN
        IF size < required THEN
          size := required;
        END;
        NEW(tmp, 2*size);
        sys.MOVE (sys.ADR(buffer^), sys.ADR(tmp^), SIZE(buffer^));
        DISPOSE(buffer);
        buffer := tmp;
      END;
      line := sys.ADR(buffer^[occupied]);
      COPY(s, line^);
      IF N = HIGH(strings^)+1 THEN
        NEW(str, 2*N);
        sys.MOVE (sys.ADR(strings^), sys.ADR(str^), SIZE(strings^));
        DISPOSE(strings);
        strings := str;
      END;
      strings^[N] := occupied;
      INC(N);
    END;
  END;
END AddLine;


(* Вставляет строку в текст перед указанной строкой *)
(* Если такой строки нет, добавляет в конец текста *)
PROCEDURE InsLine (t: TEXT; i: CARDINAL; f-: ARRAY OF CHAR; SEQ args: sys.BYTE);
VAR
  line    : txt_ptr;
  size    : CARDINAL;
  occupied: CARDINAL;
  required: CARDINAL;
  tmp     : BUFFER;
  str     : STRINGS;
  p       : CARDINAL;
  s       : String;
BEGIN
  IF t # NIL THEN
    fmt.print (s, f, args);
    WITH t^ DO
      ASSERT(magic = TEXT_MAGIC);
      IF i < N THEN
        required := LENGTH(s)+1;
        GetLine (t, N-1, line);
        occupied := strings^[N-1] + LENGTH(line^) + 1;
        size := HIGH(buffer^)+1;
        IF size - occupied < required THEN
          IF size < required THEN
            size := required;
          END;
          NEW(tmp, 2*size);
          sys.MOVE (sys.ADR(buffer^), sys.ADR(tmp^), SIZE(buffer^));
          DISPOSE(buffer);
          buffer := tmp;
        END;
        FOR p := occupied-1 TO strings^[i] BY -1 DO
          buffer^[p+required] := buffer^[p];
        END;
        line := sys.ADR(buffer^[strings^[i]]);
        COPY(s, line^);
        IF N-1 = HIGH(strings^) THEN
          NEW(str, 2*N);
          sys.MOVE (sys.ADR(strings^), sys.ADR(str^), SIZE(strings^));
          DISPOSE(strings);
          strings := str;
        END;
        FOR p := N TO i+1 BY -1 DO
          strings^[p] := strings^[p-1]+required;
        END;
        INC(N);
      ELSE
        AddLine (t, s);
      END;
    END;
  END;
END InsLine;


(* Удаляет строку из текста *)
PROCEDURE DelLine (t: TEXT; i: CARDINAL);
VAR
  line    : txt_ptr;
  deleted : CARDINAL;
  occupied: CARDINAL;
  p       : CARDINAL;
BEGIN
  IF t # NIL THEN
    WITH t^ DO
      ASSERT(magic = TEXT_MAGIC);
      IF i < N THEN
        GetLine (t, i, line);
        deleted := LENGTH(line^)+1;
        GetLine (t, N-1, line);
        occupied := strings^[N-1] + LENGTH(line^) + 1;
--        IF occupied-1 = HIGH(buffer^) THEN
--          DEC(occupied);
--        END;
        FOR p := strings^[i] TO occupied-deleted-1 DO
          buffer^[p] := buffer^[p+deleted];
        END;
        FOR p := i+1 TO N-1 DO
          strings^[p-1] := strings^[p]-deleted;
        END;
        DEC(N);
      END;
    END;
  END;
END DelLine;


PROCEDURE GetName (t: TEXT; VAR name: txt_ptr);
(* выдает имя указанного текста  *)
BEGIN
  IF t = NIL THEN
    name := sys.ADR(empty_str);
  ELSE
    WITH t^ DO
      ASSERT(magic = TEXT_MAGIC);
      IF filename = NIL THEN
        name := sys.ADR(empty_str);
      ELSE
        name := sys.ADR(filename^);
      END;
    END;
  END;
END GetName;

(* Устанавливает имя для указанного текста *)
PROCEDURE SetName (t: TEXT; name-: ARRAY OF CHAR);
BEGIN
  IF t # NIL THEN
    WITH t^ DO
      ASSERT(magic = TEXT_MAGIC);
      IF filename # NIL THEN
        xStr.dealloc_str (filename);
      END;
      xStr.alloc_from (filename, name);
    END;
  END;
END SetName;


(* Выдает размер указанного текста *)
PROCEDURE GetSize (t: TEXT): CARDINAL;
BEGIN
  IF t # NIL THEN
    WITH t^ DO
      ASSERT(magic = TEXT_MAGIC);
      IF buffer # NIL THEN
        RETURN HIGH(buffer^)+1;
      END;
    END;
  END;
  RETURN 0;
END GetSize;


<* WOFF301+ *>
PROCEDURE dummy_NotFound (file_name-: ARRAY OF CHAR); (* файл не найден *)
BEGIN
END dummy_NotFound;

PROCEDURE dummy_FileEmpty (file_name-: ARRAY OF CHAR); (* файл пустой *)
BEGIN
END dummy_FileEmpty;

PROCEDURE dummy_ReadError (file_name-: ARRAY OF CHAR); (* ошибка чтения *)
BEGIN
END dummy_ReadError;
<* WOFF301- *>



TYPE
  SEQ_TEXT_BUF = POINTER TO ARRAY OF CHAR;
  SEQ_TEXT_REC = RECORD
                   buffer: SEQ_TEXT_BUF;
                   pos   : CARDINAL;
                   ok    : BOOLEAN;
                 END;
  SEQ_TEXT = POINTER TO SEQ_TEXT_REC;


PROCEDURE SeqOpen (VAR t: SEQ_TEXT; name-: ARRAY OF CHAR);
VAR
  res : rf.OpenResults;
  Pos : xfp.FilePos;
  size: CARDINAL;
  n   : CARDINAL;
  cid : ioc.ChanId;
  ch  : CHAR;
BEGIN
  t := seq_nil;
  rf.OpenOld(cid, name, rf.raw, res);
  IF res # rf.opened THEN t := seq_nil; RETURN; END;
  Pos := rf.EndPos(cid);
  IF NOT xfp.PosToCard(size,Pos) OR (size=0) THEN t := seq_nil; RETURN; END;
  NEW(t);
  NEW(t^.buffer,size+1);
  ioc.RawRead(cid, sys.ADR(t^.buffer^[0]),size, n);
  IF (ioc.ReadResult(cid) # icc.allRight) OR (size <> n) THEN 
    DISPOSE(t^.buffer);
    DISPOSE(t);
    rf.Close(cid);
    RETURN; 
  END;
  rf.Close(cid);
  FOR n := 0 TO size DO
    ch := t^.buffer^[n];
    IF (ch = CR) OR (ch = LF) OR (ch = EOF) THEN t^.buffer^[n] := 0C; END;
  END;
  t^.buffer^[size] := 0C;
  t^.pos := 0;
  t^.ok  := TRUE;
END SeqOpen;


PROCEDURE SeqClose (VAR t: SEQ_TEXT);
BEGIN
  ASSERT(SeqStatus(t));
  DISPOSE(t^.buffer);
  DISPOSE(t);
END SeqClose;


PROCEDURE SeqReadString (VAR t: SEQ_TEXT; VAR s: ARRAY OF CHAR);
VAR
  i: CARDINAL;
BEGIN
  ASSERT(t#NIL);
  WITH t^ DO
    ASSERT( (buffer#NIL) AND ok );
    IF pos > HIGH(buffer^) THEN
      ok := FALSE;
    ELSE
      i := 0;
      WHILE (pos<=HIGH(buffer^)) AND (buffer^[pos] # 0C) DO
        s[i] := buffer^[pos];
        INC(pos);
        INC(i);
      END;
      s[i] := 0C;
      WHILE (pos<=HIGH(buffer^)) AND (buffer^[pos] = 0C) DO INC(pos); END;
      ok := TRUE;
    END;
  END;
END SeqReadString;


PROCEDURE SeqStatus (t: SEQ_TEXT): BOOLEAN;
BEGIN
  ASSERT(t#NIL);
  RETURN t^.ok;
END SeqStatus;


BEGIN
  fmo.LineSeparator(pla.lineSep);
  fmo.TextSeparator(pla.textSep);
  nil := TEXT(NIL);
  seq_nil := SEQ_TEXT(NIL);
  dummyNotFound  := dummy_NotFound;
  dummyReadError := dummy_ReadError;
  dummyFileEmpty := dummy_FileEmpty;
  NotFound  := dummy_NotFound;
  ReadError := dummy_ReadError;
  FileEmpty := dummy_FileEmpty;
END Texts.
