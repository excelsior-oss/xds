IMPLEMENTATION MODULE Errors;

IMPORT fmt := FormStr;
IMPORT sys := SYSTEM;

IMPORT msg := MsgNo;
IMPORT pro := Protocol;
IMPORT opt := Options;
IMPORT txt := Texts;
IMPORT        xStr;

IMPORT bas := PckBase;
IMPORT ope := PckOpers;
IMPORT typ := PckTypes;


VAR
  Count_errors  : CARDINAL;
  Count_warnings: CARDINAL;

PROCEDURE DecErrors;
BEGIN
  IF Count_errors # 0 THEN DEC(Count_errors); END;
END DecErrors;

PROCEDURE IncErrors;
BEGIN
  INC(Count_errors);
  IF (Count_errors >= Errors_limit) THEN
    WasError := TRUE;
    HALT (6);
  END;
END IncErrors;


(* Процедура для выдачи сообщений об ошибках.     *)
(* После вызова работа интерпретатора прерывается *)
PROCEDURE Error (error:CARDINAL; SEQ arg:sys.BYTE);
VAR
  m, buf, n : xStr.String;
  name : xStr.txt_ptr;
BEGIN
  txt.GetName(typ.Pakets[typ.CurrPaket].Paket,name);
  bas.FileName(name^, n);
  pro.GetMsg(error,m);
  fmt.print(buf, m, arg);
  pro.WriteMsgNo(msg.Error,TRUE,TRUE,error,n,typ.Pakets[typ.CurrPaket].LineNum, ope.GetCurrentPos()+1,buf,0,0,0);
  IncErrors;
END Error;

(* Процедура для выдачи предупреждений. *)
PROCEDURE Warning (warning:CARDINAL; SEQ arg:sys.BYTE);
VAR
  m, buf, n : xStr.String;
  name : xStr.txt_ptr;
BEGIN
  txt.GetName(typ.Pakets[typ.CurrPaket].Paket,name);
  bas.FileName(name^, n);
  pro.GetMsg(warning,m);
  fmt.print(buf, m, arg);
  pro.WriteMsgNo(msg.Warning,TRUE,TRUE,n,typ.Pakets[typ.CurrPaket].LineNum,ope.GetCurrentPos()+1,buf,0,0,0);
  INC(Count_warnings);
END Warning;

(* Процедура для выдачи предупреждений. *)
PROCEDURE WarningStr (warning-: ARRAY OF CHAR; SEQ arg:sys.BYTE);
VAR
  buf, n : xStr.String;
  name : xStr.txt_ptr;
BEGIN
  txt.GetName(typ.Pakets[typ.CurrPaket].Paket,name);
  bas.FileName(name^, n);
  fmt.print(buf, warning, arg);
  pro.WriteMsgNo(msg.Warning,TRUE,TRUE,n,typ.Pakets[typ.CurrPaket].LineNum,ope.GetCurrentPos()+1,buf,0,0,0);
  INC(Count_warnings);
END WarningStr;


(* Устанавливает лимит ошибок равный нулю                           *)
(* Если после этого вызвать Error, работа интерпретатора завершится *)
PROCEDURE FatalError;
BEGIN
  Errors_limit := 0;
END FatalError;


PROCEDURE FoundErrorsAndWarnings;
BEGIN
  IF (Count_errors <> 0) OR (Count_warnings <> 0) THEN
    pro.WriteMsgNo(msg.FoundErrorsWarnings,TRUE,TRUE,Count_errors,Count_warnings);
  END;
END FoundErrorsAndWarnings;

(* Если не хватило памяти... *)

VAR
  n1 : xStr.txt_ptr;
  m, d : xStr.String;

BEGIN
  Errors_limit := 20;
  Count_errors := 0;
  Count_warnings := 0;
  WasError := FALSE;
FINALLY
  IF NOT opt.in_dialog AND (typ.QuantityPaket#0) THEN
    FoundErrorsAndWarnings;
    txt.GetName(typ.Pakets[0].Paket,n1);
    bas.FileName(n1^, d);
    IF WasError THEN
      pro.GetMsg(msg.Not_normal_term,m);
    ELSE
      pro.GetMsg(msg.Normal_term,m);
    END;
    pro.WriteMsgNo(msg.Term,TRUE,TRUE, d, m);
  END;
END Errors.
