<* Storage + *>

IMPLEMENTATION MODULE Pack;
(* Пакетный отладчик *)

IMPORT brk := CtrlC;

IMPORT xStr;
IMPORT red := RedFile;
IMPORT fil := File;
IMPORT opt := Options;
IMPORT pro := Protocol;
IMPORT msg := MsgNo;
IMPORT err := Errors;
IMPORT txt := Texts;

IMPORT krn := KrnTypes;

IMPORT exp := Expr;

IMPORT typ := PckTypes;
IMPORT bas := PckBase;
IMPORT ope := PckOpers;
IMPORT lst := Lists;

IMPORT key := Keys;

<* IF DEST_K26 THEN *>

IMPORT PckModel;

<* END *>


VAR
  Line: xStr.txt_ptr;

(*▒▒▒▒▒▒▒▒▒▒▒▒▒▒ Сбор меток и их проверка ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒*)
PROCEDURE Label_Collect_And_Check;
VAR
  name1     ,
  name2     : xStr.txt_ptr;
  name3     ,
  name4     : xStr.String;
  i         : CARDINAL;
  FileExt   : BOOLEAN;
  PosStr    : CARDINAL;
  Param     : xStr.String;

BEGIN
  IF typ.Pakets[typ.CurrPaket].Reference THEN RETURN; END;
  (* Номер последней строки *)
  typ.Pakets[typ.CurrPaket].LastLine := txt.LastLine(typ.Pakets[typ.CurrPaket].Paket);
  IF typ.Pakets[typ.CurrPaket].LastLine = 0 THEN RETURN; END;
  DEC(typ.Pakets[typ.CurrPaket].LastLine);
  (* Начинаем с первой *)
  typ.Pakets[typ.CurrPaket].LineNum := 1;
  LOOP
    (* Прочли весь пакет? *)
    IF typ.Pakets[typ.CurrPaket].LineNum-1 > typ.Pakets[typ.CurrPaket].LastLine THEN EXIT END;
    txt.GetLine(typ.Pakets[typ.CurrPaket].Paket,typ.Pakets[typ.CurrPaket].LineNum-1, Line);
    PosStr := 0;
    IF NOT bas.SkipBlanks(Line^, PosStr) AND (PosStr = 0) THEN
      ASSERT(bas.GetParam(Line^, PosStr, Param));
      IF (Param[0] = '#') THEN
        IF (Param = '#IMPORT') OR (Param = '#ИМПОРТ') THEN
          IF bas.GetParam(Line^, PosStr, Param) THEN
            IF exp.CheckFileName(Param, FileExt) THEN
              xStr.Uppercase(Param);
              IF NOT FileExt THEN fil.ChangeExtension(Param, krn.imp_file_ext); END;
              txt.GetName(typ.Pakets[typ.CurrPaket].Paket, name1);
              bas.FileName(name1^, name3);
              pro.WriteMsgNo(msg.Include, TRUE, pro.to_file, name3,
                             typ.Pakets[typ.CurrPaket].LineNum, PosStr+1, Param);
              IF red.Read(Param, name4) = red.Ok THEN
                xStr.Uppercase(name4);
                i := 0;
                LOOP
                  IF i = typ.QuantityPaket THEN
                    txt.Open(typ.Pakets[typ.QuantityPaket].Paket, name4);
                    EXIT;
                  END;
                  txt.GetName(typ.Pakets[i].Paket,name1);
                  IF name1^ = name4 THEN
                    typ.Pakets[typ.QuantityPaket].Paket := typ.Pakets[i].Paket;
                    typ.Pakets[typ.QuantityPaket].Reference := TRUE;
                    EXIT;
                  END;
                  INC(i);
                END;
                IF typ.QuantityPaket < typ.MaxPakets THEN
                  IF typ.Pakets[typ.QuantityPaket].Paket <> txt.nil THEN
                    txt.GetName (typ.Pakets[typ.QuantityPaket].Paket, name1);
                    xStr.Uppercase (name1^);
                    i := typ.CurrPaket;
                    LOOP
                      txt.GetName (typ.Pakets[i].Paket,name2);
                      IF name1^ = name2^ THEN
                        bas.FileName(name1^, name3);
                        err.FatalError;
                        err.Error(msg.Paket_cross_reference, name3);
                      END;
                      IF typ.Pakets[i].RetPaket = MAX(CARDINAL) THEN EXIT; END;
                      i := typ.Pakets[i].RetPaket;
                    END;
                    typ.Pakets[typ.QuantityPaket].RetPaket := typ.CurrPaket;
                    INC(typ.QuantityPaket);
                  END;
                ELSE
                  err.Warning (msg.Too_many_pakets, typ.MaxPakets);
                END;
              ELSE
                err.Error(msg.Include_not_executing, Param);
              END;
            ELSE
              err.Error(msg.Incorrect_paket_name, Param);
            END;
          ELSE
            err.Error(msg.Paket_missed);
          END;
        ELSE
          err.Error(msg.DirectiveUndefined, Param);
        END;
      ELSE
        IF NOT ope.IsOperator(Param) THEN
          IF exp.CheckName(Param, TRUE) THEN
            lst.PutLabel(Param, typ.CurrPaket, typ.Pakets[typ.CurrPaket].LineNum-1);
            IF lst.Duplicate THEN err.Error(msg.Dublicate_label, Param); END;
          ELSE
            err.Error(msg.Incorrect_label);
          END;
        ELSE
          err.Error(msg.Label_equal_operator, Param);
        END;
      END;
    END;
    INC(typ.Pakets[typ.CurrPaket].LineNum);
  END;
  typ.Pakets[typ.CurrPaket].LineNum := 0;
END Label_Collect_And_Check;



(*▒▒▒▒▒▒▒▒▒ Вывод операторов пакета отладки ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒*)
PROCEDURE PrintStatement;
VAR
  name1: xStr.String;
  name2: xStr.txt_ptr;
BEGIN
  IF bas.CheckMode(typ.Pak) THEN
    IF typ.LastPaket <> typ.CurrPaket THEN
      txt.GetName(typ.Pakets[typ.CurrPaket].Paket, name2);
      bas.FileName(name2^, name1);
      pro.WriteMsgNo(msg.Executing_part,pro.to_screen,pro.to_file,name1);
    END;
    pro.WriteMsgNo(msg.On_line, pro.to_screen, pro.to_file, typ.Pakets[typ.CurrPaket].LineNum, Line^);
  END;
  typ.LastPaket := typ.CurrPaket;
END PrintStatement;


(* Исполнение пакета отладки *)
PROCEDURE Debugger;
VAR
  name1  : xStr.txt_ptr;    (* Имя пакета                        *)
  name2  : xStr.String;     (* Имя пакета                        *)
  RC     : CARDINAL;       (* Код возврата вызова оператора     *)
  warning: BOOLEAN;        (* Признак наличия лишних параметров *)


  (* Проверка перехода по ошибке *)
  PROCEDURE JumpByError(): BOOLEAN;
  VAR
    paket ,               (* Номер пакета                  *)
    line  : CARDINAL;     (* Строка в пакете               *)
  BEGIN
    IF lst.GetJumpByError(RC, paket,line) THEN
      pro.WriteMsgNo(msg.JumpByError, pro.to_screen, pro.to_file, RC);
      typ.CurrPaket := paket;
      typ.Pakets[typ.CurrPaket].LineNum := line;
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END JumpByError;


BEGIN
  LOOP
    WITH typ.Pakets[typ.CurrPaket] DO
      IF LineNum > LastLine THEN; (* Если прочли весь пакет. Отладка закончена *)
        IF RetPaket # typ.CurrPaket THEN
          txt.GetName(Paket,name1);
          bas.FileName(name1^, name2);
          err.Error(msg.Not_found_RETURN, name2);
        END;
        EXIT;
      ELSE
        (* Прочитать очередную строку *)
        txt.GetLine(Paket,LineNum,Line);
        (* Сдвинуться на след. строку *)
        INC(LineNum);
        PrintStatement;
        IF ope.Execute(Line^, RC, warning) THEN
          IF RC # 0 THEN
            IF NOT JumpByError() THEN err.Error(RC); END;
          ELSIF warning THEN
            err.Warning(msg.Expected_end_param);
          END;
        ELSE
          err.Error(msg.ExpectedOperatorName);
        END;
      END;
    END;
    IF opt.Stop_Pack THEN EXIT; END;
  END;
END Debugger;


(* Старт пакетного отладчика                 *)
(* Имя пакета должно быть в Options.tst_name *)
PROCEDURE Start;
VAR
  fname: xStr.txt_ptr;
  tst_name: xStr.String;
  i: CARDINAL;
BEGIN
  (* В настоящий момент работаем в пакете *)
  opt.in_dialog := FALSE;

  (* Открыть текст пакета *)
  txt.Open(typ.Pakets[typ.CurrPaket].Paket, opt.tst_name);
  IF typ.Pakets[typ.CurrPaket].Paket <> txt.nil THEN (* Открыт *)
    txt.GetName(typ.Pakets[typ.CurrPaket].Paket, fname);
    xStr.Uppercase(fname^);
    bas.FileName(fname^, tst_name);
    pro.WriteMsgNo(msg.Paket, TRUE, pro.to_file, tst_name);
    INC(typ.QuantityPaket);

    (* Кем импортируется, номер импортирующего пакета *)
    (* Если MAX(CARDINAL) - основной пакет            *)
    typ.Pakets[typ.CurrPaket].RetPaket := MAX(CARDINAL);
    LOOP
        (* Проверить и сохранить метки, при этом импортируются пакеты *)
      Label_Collect_And_Check;
      INC(typ.CurrPaket);
      IF typ.CurrPaket = typ.QuantityPaket THEN EXIT; END;
    END;
    FOR i := 0 TO typ.QuantityPaket-1 DO
      typ.Pakets[i].RetPaket := MAX(LONGCARD);
      typ.Pakets[i].RetLineNum := txt.LastLine(typ.Pakets[i].Paket);
      IF typ.Pakets[i].RetLineNum > 0 THEN
        DEC(typ.Pakets[i].RetLineNum);
      END;
    END;
    (* Основной (головной) пакет *)
    typ.Pakets[0].RetPaket := 0;
  END;
  IF typ.QuantityPaket > 0 THEN
    typ.CurrPaket := 0;
    (* Основной цикл: вертимся, пока не надоест... *)
    Debugger;
  END;
END Start;


VAR
  oldCtrlBreakHandler: brk.BreakHandler;

PROCEDURE ["C"] StopProcessingBatch (): BOOLEAN;
BEGIN
  opt.Stop_Pack := NOT opt.in_dialog;
  err.WasError := opt.Stop_Pack;
  RETURN oldCtrlBreakHandler();
END StopProcessingBatch;


BEGIN
  oldCtrlBreakHandler := brk.SetBreakHandler (StopProcessingBatch);
END Pack.

