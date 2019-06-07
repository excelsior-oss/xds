IMPLEMENTATION MODULE XD_Parse;

IMPORT arg := ProgEnv;
IMPORT ws  := WholeStr;

IMPORT xs  := xStr;
IMPORT pro := Protocol;
IMPORT fil := File;
IMPORT opt := Options;
IMPORT msg := MsgNo;
IMPORT krn := KrnTypes;

FROM Printf IMPORT printf;

IMPORT lst := Lists;


VAR
  OptMode : (nothing, paket, dialog); (* Режим, заданный в командной строке *)

PROCEDURE Options (s-:ARRAY OF CHAR; VAR help, debugee: BOOLEAN): BOOLEAN;
VAR
  ErrorInOptions: BOOLEAN;
  len  : CARDINAL;
  num  : xs.String;
  pos  : CARDINAL;
  found: BOOLEAN;
  res  : ws.ConvResults;
  
BEGIN
  ErrorInOptions := FALSE;
  len := LENGTH(s);
  CASE CAP(s[1]) OF
  |'A': debugee := TRUE;          (* Сразу за этой опцией следует командная строка *)
  |'B': opt.DialogMode := FALSE;  (* Пакетный режим                 *)
        OptMode        := paket;
        ErrorInOptions := (len # 2);

  |'D': opt.DialogMode := TRUE;   (* Диалоговый режим               *)
        OptMode        := dialog;
        ErrorInOptions := (len # 2);

  |'E': opt.CatchExceptInternalError := FALSE; (* Ловить EXCEPT от MainLoop *)
        ErrorInOptions := (len # 2);

  |'J': pro.SetSafeMode();        (* "Безопасный" протокол          *)
        ErrorInOptions := (len # 2);

  |'H': help := TRUE;             (* Help! Выдать полную справку    *)
        ErrorInOptions := (len # 2);

  |'N': opt.name_only := TRUE;    (* Выдавать только имена файлов   *)
        ErrorInOptions := (len # 2);

  |'L': pro.DefaultProtocol;      (* Протокол с именем по умолчанию *)
        ErrorInOptions := (len # 2);

  |'R': ErrorInOptions := TRUE;
        IF s[2] = '=' THEN   
<* PUSH *>
<* WOFF903+ *>
          pos := xs.CharPos (s, ' ', found);
<* POP *>          
          IF NOT found THEN 
            pos := xs.CharPos (s, ',', found);
            IF found AND (pos > 3) THEN
              xs.Extract (s, 3, pos-3, opt.RemoteTransport);
              xs.Extract (s, pos+1, len, opt.RemoteHost);
              opt.RemoteMode := TRUE;
              ErrorInOptions := FALSE;

              pos := xs.CharPos (opt.RemoteHost, ':', found);
              IF found THEN
                xs.Extract (opt.RemoteHost, pos+1, LENGTH(opt.RemoteHost), num);
                opt.RemoteHost [pos] := 0C;
                ws.StrToCard (num, pos, res);
                ErrorInOptions := res # ws.strAllRight;
                opt.RemotePort := pos;
              END;
            END;  
          END;
        END;

  |'S': opt.StopImmediately := TRUE;
        ErrorInOptions := (len # 2);

  |'X': xs.Extract (s, 2, len, num);
        ws.StrToCard (num, pos, res);
        ErrorInOptions := res # ws.strAllRight;
        opt.SetXY (pos, opt.Y);

  |'Y': xs.Extract (s, 2, len, num);
        ws.StrToCard (num, pos, res);
        ErrorInOptions := res # ws.strAllRight;
        opt.SetXY (opt.X, pos);
  
<* IF DEFINED (xd_debug) & xd_debug THEN *>

  |'I': CASE s[2] OF
        |'l': opt.DebugOn(opt.Load);
        |'e': opt.DebugOn(opt.Expr);
        |'a': opt.DebugOn(opt.Another);
        |'i': opt.DebugOn(opt.InfoCache);
        ELSE
          ErrorInOptions := TRUE;
        END;
        ErrorInOptions := ErrorInOptions OR (len # 3);
<* END *>

  ELSE
    ErrorInOptions := TRUE;
  END;
  IF ErrorInOptions THEN
    printf(msg.Parse_Invalid_Option, s);
  END;
  RETURN NOT ErrorInOptions;
END Options;


PROCEDURE ParseCommandLine (VAR help: BOOLEAN): BOOLEAN;
VAR
  i, k, p: CARDINAL;
  a: xs.String;
  f, debugee: BOOLEAN;
BEGIN
   opt.DialogMode := TRUE;
   OptMode := nothing;
   k := arg.ArgNumber();
   help := FALSE;
   debugee := FALSE;
   IF k = 0 THEN RETURN TRUE; END;
   i := 0;
   LOOP                                   (* Разбор опций *)
     IF (i = k) THEN EXIT; END;
     arg.GetArg(i, a);
     IF NOT debugee AND ((a[0] = '/') OR (a[0] = '-')) THEN (* Опции начинаются с '-' или '/' *)
       IF NOT Options(a, help, debugee) THEN RETURN FALSE; END;
       IF help THEN RETURN TRUE; END;
     ELSE
       EXIT;
     END;
     INC(i);
   END;
   IF (i = k) THEN
     IF NOT opt.DialogMode THEN
       printf(msg.Parse_Paket_missed, a);
       HALT (8);
     END;
   ELSE
     IF NOT opt.RemoteMode THEN
       xs.Uppercase(a);
       IF opt.DialogMode THEN
--         fil.AddExtension(a, krn.prg_file_ext);
       ELSE
         fil.AddExtension(a, krn.pkt_file_ext);
       END;
       IF (OptMode # paket) AND fil.CompareExtension(a, krn.prg_file_ext) THEN
         opt.DialogMode := TRUE;
       ELSIF (OptMode # dialog) AND fil.CompareExtension(a, krn.pkt_file_ext) THEN
         opt.DialogMode := FALSE;
       END;
     END;
     IF  opt.DialogMode THEN
       COPY(a, opt.prog_name);
     ELSE
       COPY(a, opt.tst_name);
     END;
     INC(i);
     IF opt.DialogMode THEN
       LOOP
         IF (i = k) THEN EXIT; END;
         arg.GetArg(i, a);
         <* PUSH *>
         <* WOFF903+ *>
         p := xs.CharPos (a, ' ', f);
         <* POP *>
         IF f THEN
           xs.Insert ('"', 0, a);
           xs.Append ('"', a);
         END;
         xs.Append(a,   opt.prog_args);
         xs.Append(' ', opt.prog_args);
         INC(i);
       END;
     ELSE
      <* IF xd_batch_included THEN *>
       WHILE i < k DO
         arg.GetArg(i, a);
         lst.PutArg (a);
         INC(i);
       END;
      <* ELSE *>
       IF (i < k) THEN
         printf(msg.Parse_Extra_param_ignored, i+1);
       END;
      <* END *>
     END;
   END;
   RETURN TRUE;
END ParseCommandLine;


END XD_Parse.

