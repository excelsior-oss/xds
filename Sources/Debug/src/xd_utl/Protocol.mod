<* Storage + *>

IMPLEMENTATION MODULE Protocol;

IMPORT sys := SYSTEM;
IMPORT ioc := IOChan;
IMPORT seq := SeqFile;
IMPORT tio := TextIO;
IMPORT std := StdChans;
IMPORT cc  := ChanConsts;
IMPORT arg := ProgEnv;
IMPORT fmt := FormStr;
IMPORT rnd := RndFile;

IMPORT xStr;
IMPORT red := RedFile;
IMPORT txt := Texts;
IMPORT msg := MsgNo;
IMPORT fil := File;
IMPORT opt := Options;

FROM Storage IMPORT ALLOCATE, DEALLOCATE;
FROM Printf IMPORT printf;



PROCEDURE MsgFileNotFound (f_name-: ARRAY OF CHAR);
BEGIN
  printf ('Messages file %s not found.\n', f_name);
END MsgFileNotFound;


PROCEDURE FileEmpty(f_name-: ARRAY OF CHAR);
BEGIN
  WriteMsgNo (msg.File_is_empty, TRUE, TRUE, f_name);
END FileEmpty;

PROCEDURE NotFound(f_name-: ARRAY OF CHAR);
BEGIN
  WriteMsgNo (msg.File_not_found, TRUE, TRUE, f_name);
END NotFound;

PROCEDURE ReadError(f_name-: ARRAY OF CHAR);
BEGIN
   WriteMsgNo (msg.Read_error, TRUE, TRUE, f_name);
END ReadError;


CONST
  msg_file_ext = 'MSG'; (* Расширение имени msg-файла *)

  msg_offs = 4;         (* смещение текста сообщения от начала строки *)
  msg_max  = 1000;      (* Зависит от msg_offs, "999"                 *)

  unknown  = "Text message is not available";

  msg_sys_not_init = "message system not initialized";


<* IF DEST_K26 THEN *>

  res_file_ext = 'RES';

<* ELSIF DEST_XDS THEN *>

  res_file_ext = 'LOG';

<* END *>


TYPE
  MSG_STR = ARRAY [0..msg_max-1] OF xStr.txt_ptr;

VAR
  msgText: txt.TEXT;           (* Текст сообщений *)
  msgStr : POINTER TO MSG_STR; (* Сообщения       *)

  proto_name: xStr.STRING;     (* имя файла для протокола         *)
  Proto_File : ioc.ChanId;     (* файл протокола                  *)
  open : BOOLEAN;              (* открыт ли файл протокола        *)
  safe : BOOLEAN;              (* режим работы с файлом протокола *)
  unknown_str: xStr.txt_ptr;   (* неизвестное сообщение           *)

  sys_not_init_str: xStr.txt_ptr;
  sys_not_init: BOOLEAN;


PROCEDURE make_msg_table;
VAR
  k,i,n,N: CARDINAL;
  str    : xStr.txt_ptr;
  ch     : CHAR;
BEGIN
  NEW(msgStr);
  FOR i := 0 TO msg_max-1 DO msgStr^[i] := unknown_str; END;
  N := txt.LastLine(msgText);
  sys_not_init := TRUE;
  IF N = 0 THEN RETURN; END;
  FOR i := 0 TO N-1 DO
    txt.GetLine(msgText, i, str);
    IF LENGTH(str^) >= msg_offs THEN
      n := 0;
      k := 0;
      LOOP
        ch := str^[k];
        IF (ch<'0') OR ('9'<ch) THEN EXIT; END; (* Пока цифры *)
        n := n * 10 + ORD(ch) - ORD('0');
        INC(k);
        IF k = msg_offs THEN EXIT; END;         (* Уже строка *)
      END;
      IF (n#0) AND (n <= msg_max) THEN
        msgStr^[n-1] := sys.ADDADR(str, msg_offs);
      END;
    END;
  END;
END make_msg_table;


PROCEDURE Get(msg_no: CARDINAL) : xStr.txt_ptr;
BEGIN
  IF sys_not_init THEN
    IF (msg_no#0) AND (msg_no <= msg_max) THEN
      RETURN msgStr^[msg_no-1];
    ELSE
      RETURN unknown_str;
    END;
  ELSE
    RETURN sys_not_init_str;
  END;
END Get;

PROCEDURE GetMsg (no: CARDINAL; VAR s:ARRAY OF CHAR);
(* Возвращает сообщение по номеру *)
VAR
  m : xStr.txt_ptr;
BEGIN
  m := Get(no);
  COPY (m^, s);
END GetMsg;

PROCEDURE Create(VAR name: ARRAY OF CHAR) : BOOLEAN;
VAR
  f_name: xStr.String;
  res   : cc.OpenResults;
BEGIN
  IF open THEN
    seq.Close(Proto_File);
    open := FALSE;
  END;
  IF (proto_name # NIL) THEN
    DISPOSE(proto_name);
    proto_name := NIL;
  END;
  red.Write(name, f_name);
  COPY(f_name, name);
  NEW(proto_name, LENGTH(f_name)+1);
  IF (proto_name # NIL) THEN
    COPY(f_name,proto_name^);
    (* создаем файл для выдачи протокола *)
    rnd.OpenClean(Proto_File,proto_name^,seq.text+seq.write+seq.old, res);
    open := (res=cc.opened);
    IF NOT open THEN
      DEALLOCATE(proto_name, LENGTH(proto_name^)+1);
      proto_name := NIL;
      RETURN FALSE;
    END;
    IF safe THEN
      seq.Close(Proto_File);
      open := FALSE;
    END;
    RETURN TRUE;
  END;
  RETURN FALSE;
END Create;

PROCEDURE dummy_pre; BEGIN END dummy_pre;

PROCEDURE dummy_post; BEGIN END dummy_post;


(* Создание файла для протокола по умолчанию *)
PROCEDURE MakeDefaultProtocolName (VAR name: ARRAY OF CHAR);
VAR
  fname: xStr.String;
BEGIN
  IF opt.tst_name # '' THEN
    fil.ExtractFileName (opt.tst_name, fname);
  ELSIF opt.prog_name # '' THEN
    fil.ExtractFileName (opt.prog_name, fname);
  ELSE
    COPY ('result', fname);
  END;
  fil.ChangeExtension (fname, res_file_ext);
  fil.ModifyFileName (fname, name);
END MakeDefaultProtocolName;


PROCEDURE WriteMsg (text-:ARRAY OF CHAR; screen,file : BOOLEAN);
VAR
  res               : cc.OpenResults;
  curr_dir_protocol : xStr.String;
  def_proto_name    : xStr.String;
  crach             : xStr.txt_ptr;
BEGIN

  pre_msg; (* Процедура, вызываемая до печати *)

  IF screen THEN
    tio.WriteString(std.OutChan(),text);
    tio.WriteLn(std.OutChan());
  END;
  IF file AND to_file THEN
    IF proto_name = NIL THEN
      MakeDefaultProtocolName (def_proto_name);
      COPY(def_proto_name, curr_dir_protocol);
      IF NOT Create(curr_dir_protocol) THEN
        WriteMsgNo(msg.Cant_open_protocol,TRUE,FALSE,curr_dir_protocol);
        curr_dir_protocol := '. ';
        curr_dir_protocol [1] := fil.GetFileSepChar ();
        xStr.Append(def_proto_name, curr_dir_protocol);
        IF NOT Create(curr_dir_protocol) THEN
          WriteMsgNo(msg.Cant_open_protocol,TRUE,FALSE,curr_dir_protocol);
          to_file := FALSE;
          to_screen := TRUE;
        END;
      END;
    END;
    IF proto_name = NIL THEN
      to_file := FALSE;
      to_screen := TRUE;
      IF NOT screen THEN
        tio.WriteString(std.OutChan(),text);
        tio.WriteLn(std.OutChan());
      END;
    ELSE
      IF NOT open THEN
         seq.OpenAppend(Proto_File,proto_name^,seq.text+seq.write,res);
         open := (res = cc.opened);
      END;
      tio.WriteString(Proto_File,text);
      tio.WriteLn(Proto_File);
      IF safe THEN
        seq.Close(Proto_File);
        open := FALSE;
      END;
    END;
  END;

  post_msg; (* Процедура, вызываемяа после печати *)

EXCEPT
  IF ioc.IsChanException() THEN
    crach := Get(msg.ErrorWritingProtocol);
    tio.WriteString(std.ErrChan(), crach^);
  END;
END WriteMsg;

PROCEDURE WriteMsgNo(no:CARDINAL; screen,file:BOOLEAN; SEQ arg:sys.BYTE);
(* Выводит текст сообщения в указаное место (на экран, в файл) по номеру *)
VAR
  buf, format: xStr.String;
BEGIN
  GetMsg(no,format);
  fmt.print(buf, format, arg);
  WriteMsg(buf,screen,file);
END WriteMsgNo;

PROCEDURE SetSafeMode;
BEGIN
  safe := TRUE;
  IF open THEN
    ioc.Flush(Proto_File);
    seq.Close(Proto_File);
    open := FALSE;
  END;
END SetSafeMode;

PROCEDURE SetRegularMode;
BEGIN
  safe := FALSE;
END SetRegularMode;

PROCEDURE DefaultProtocol;
BEGIN
  to_screen := FALSE;
  to_file   := TRUE;
END DefaultProtocol;

PROCEDURE InitProtocol (msg_file_name-: ARRAY OF CHAR);
VAR
  m_name : xStr.String;
  f_name : xStr.String;
  p_name : xStr.String;
  drive  : xStr.String;
  head   : xStr.String;
  tail   : xStr.String;
BEGIN
  sys_not_init := FALSE;
  txt.NotFound  := txt.dummyNotFound;
  txt.ReadError := txt.dummyReadError;
  txt.FileEmpty := txt.dummyFileEmpty;

  arg.ProgramName (p_name);
  IF msg_file_name = '' THEN
    fil.ChangeExtension (p_name, msg_file_ext);
    fil.ExtractFileName (p_name, f_name);
  ELSE
    COPY (msg_file_name, m_name);
    fil.AddExtension (m_name, msg_file_ext);
    fil.SplitPath (m_name, drive, head, f_name);
    fil.SplitPath (p_name, drive, head, tail);
    fmt.print (p_name, "%s%s%s", drive, head, f_name);
    COPY (m_name, f_name);
  END;
  txt.Open (msgText, f_name);
  IF msgText = txt.nil THEN
    txt.NotFound  := MsgFileNotFound;
    txt.Open (msgText, p_name);
    IF msgText = txt.nil THEN
      HALT (11);
    END;
  END;
(*
  IF msg_file_name = '' THEN
    arg.ProgramName(p_name);
    fil.ChangeExtension(p_name, msg_file_ext);
    fil.ExtractFileName(p_name, f_name);
    txt.Open(msgText, f_name);
    IF msgText = txt.nil THEN
      txt.NotFound  := MsgFileNotFound;
      txt.Open(msgText, p_name);
      IF msgText = txt.nil THEN
        HALT (11);
      END;
    END;
  ELSE
    txt.Open(msgText, msg_file_name);
    IF msgText = txt.nil THEN
      HALT (12);
    END;
  END;
*)
  make_msg_table;
  txt.NotFound  := NotFound;
  txt.ReadError := ReadError;
  txt.FileEmpty := FileEmpty;
END InitProtocol;

PROCEDURE Exit;
BEGIN
  SetSafeMode;
END Exit;


BEGIN
  sys_not_init     := FALSE;
  to_screen        := TRUE;
  to_file          := FALSE;
  proto_name       := NIL;
  open             := FALSE;
  safe             := FALSE;
  pre_msg          := dummy_pre;
  post_msg         := dummy_post;
  unknown_str      := sys.ADR(unknown);
  sys_not_init_str := sys.ADR(msg_sys_not_init);
FINALLY
  Exit;
END Protocol.


