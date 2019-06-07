-- Главный модуль компоненты xd_srv: сервер для удаленной отладки

MODULE XD_Serv;

<* Storage+ *>
<* ALIGNMENT = "4" *>

IMPORT sys := SYSTEM;
IMPORT arg := ProgEnv;
IMPORT fmt := FormStr;
IMPORT ws  := WholeStr;

IMPORT Printf;

IMPORT eve := Events;
IMPORT opt := Options;

IMPORT xs  := xStr;

IMPORT kt  := KrnTypes;
IMPORT kpr := Krn_Prog;
IMPORT mem := Krn_Mem;
IMPORT thr := Krn_Thrs;

IMPORT eth := Threads;

IMPORT med := Media;
IMPORT rmt := RemTypes;

-- **********************************************
IMPORT seq := SeqFile;
IMPORT tio := TextIO;
IMPORT icc := IOConsts;
IMPORT ioc := IOChan;
-- **********************************************



VAR
  PrintStat: BOOLEAN;
  WriteLog: BOOLEAN;
  MasterListen: BOOLEAN;
  Channel: rmt.TRN_CHANNEL;
  Commands: ARRAY rmt.COMMAND OF CARDINAL;



-- **********************************************

PROCEDURE WriteToLog  (s-: ARRAY OF CHAR; SEQ arg: sys.BYTE);
VAR
  outfile: seq.ChanId;
  res: seq.OpenResults;
  t: xs.String;
BEGIN
  fmt.print (t, "xd_srv_log_%d", Channel);
  seq.OpenAppend (outfile, t, seq.text+seq.old, res);
  IF res # seq.opened THEN
    HALT (1);
  END;
  fmt.print (t, s, arg);
  tio.WriteString (outfile, t);
  seq.Close (outfile);
END WriteToLog;

-- **********************************************


PROCEDURE Init;
VAR
  command: rmt.COMMAND;
BEGIN
  PrintStat := FALSE;
  WriteLog := FALSE;
  MasterListen := TRUE;
  Channel := rmt.INVALID_CHANNEL;
  -- так как сейчас целевой исполнитель будет работать на сервере
  opt.KernelInRemoteMode := TRUE;
  FOR command := MIN(rmt.COMMAND) TO MAX(rmt.COMMAND) DO
    Commands[command] := 0;
  END;
END Init;


PROCEDURE printf (s-: ARRAY OF CHAR; SEQ arg: sys.BYTE);
BEGIN
  IF PrintStat THEN
    Printf.printf (s, arg);
  END;
  IF WriteLog THEN
    WriteToLog (s, arg);
  END;
END printf;


PROCEDURE Help;
BEGIN
  printf("Usage:   xd_srv [ ('-'|'/') options ]\n");
  printf('\nOptions: R=<transport>   use transport\n');
  HALT (0);
END Help;


PROCEDURE Options (s-: ARRAY OF CHAR): BOOLEAN;
VAR
  ErrorInOptions: BOOLEAN;
  len  : CARDINAL;
  num  : xs.String;
  pos  : CARDINAL;
  found: BOOLEAN;
  param: xs.String;
  res  : ws.ConvResults;

BEGIN
  len := LENGTH(s);
  CASE CAP(s[1]) OF
  |'R': ErrorInOptions := TRUE;
        IF s[2] = '=' THEN
<* PUSH *>
<* WOFF903+ *>
          pos := xs.CharPos (s, ' ', found);
<* POP *>
          IF NOT found THEN
            xs.Extract (s, 3, MAX(CARDINAL), opt.RemoteTransport);
            xs.Uppercase (opt.RemoteTransport);
            ErrorInOptions := FALSE;

            pos := xs.CharPos (opt.RemoteTransport, ':', found);
            IF found THEN
              xs.Extract (opt.RemoteTransport, pos+1, LENGTH(opt.RemoteTransport), num);
              opt.RemoteTransport [pos] := 0C;
              ws.StrToCard (num, pos, res);
              ErrorInOptions := res # ws.strAllRight;
              opt.RemotePort := pos;
            END;
          END;
        END;

  |'S': opt.StopImmediately := TRUE;
        ErrorInOptions := (len # 2);

  |'P': PrintStat := TRUE;
        ErrorInOptions := (len # 2);

  |'L': WriteLog := TRUE;
        ErrorInOptions := (len # 2);

  |'C': xs.Extract (s, 3, MAX(CARDINAL), param);
        Channel := xs.StrToCard (param, 16, ErrorInOptions);
        ErrorInOptions := NOT ErrorInOptions;

  |'D': MasterListen := FALSE;
        ErrorInOptions := (len # 2);

 <* IF DEFINED (xd_debug) & xd_debug THEN *>
  |'I': ErrorInOptions := FALSE;
        CASE s[2] OF
        |'l': opt.DebugOn(opt.Load);
        |'e': opt.DebugOn(opt.Expr);
        |'a': opt.DebugOn(opt.Another);
        ELSE
          ErrorInOptions := TRUE;
        END;
        ErrorInOptions := ErrorInOptions OR (len # 3);
 <* END *>

  ELSE
    ErrorInOptions := TRUE;
  END;
  RETURN NOT ErrorInOptions;
END Options;


PROCEDURE ParseCommandLine;
VAR
  i, k: CARDINAL;
  a: xs.String;
BEGIN
   k := arg.ArgNumber();
   IF k = 0 THEN Help; END;
   i := 0;
   LOOP                                   (* Разбор опций *)
     IF (i = k) THEN EXIT; END;
     arg.GetArg(i, a);
     IF (a[0] = '/') OR (a[0] = '-') THEN (* Опции начинаются с '-' или '/' *)
       IF NOT Options (a) THEN Help; END;
     ELSE
       EXIT;
     END;
     INC(i);
   END;
END ParseCommandLine;


PROCEDURE InitTransport;
VAR
  rc: CARDINAL;
BEGIN
  IF MasterListen THEN
    printf ('\nWait, server listening...');
    rc := med.MasterListen (VAL(rmt.TRN_PORT, opt.RemotePort), Channel);
    IF rc # 0 THEN
      printf('\nError %u', rc);
      HALT (rc);
    END;
    IF Channel = rmt.INVALID_CHANNEL THEN
      printf('\nInvalid channel');
      HALT (1);
    END;
  END;
  printf ('\nChannel = %d\nWait, server connecting...', Channel);
  rc := med.ServerConnect (Channel);
  IF rc # 0 THEN
    printf('\nError %u', rc);
    HALT (rc);
  END;
END InitTransport;


VAR
  arg_size  : CARDINAL;
  arg_buf   : ARRAY [0..1023] OF sys.CARD8;
  arg_addr  : sys.ADDRESS;
  result    : CARDINAL;
  name      : POINTER TO xs.String;
  prog_args : POINTER TO xs.String;
  str       : xs.String;
  prog_info : kt.EXEC_INFO;
  debug_info: POINTER TO ARRAY OF sys.CARD8;
  Cache     : kt.REGISTER_CACHE;
  len, i    : CARDINAL;
  source    : kt.ADDRESS;
  dest      : kt.ADDRESS;
  GoMode    : kpr.GO_MODE;
  access    : kt.ATTRIBS;
  res       : BOOLEAN;
  mem_buf   : ARRAY [0..4095] OF sys.CARD8;
  mem_addr  : sys.ADDRESS;
  exp       : kt.EXPORTS;
  trace_type: eve.ACCESS_TYPE;
  Index     : CARDINAL;
  command   : rmt.COMMAND;


PROCEDURE SendQueueEvents;
VAR
  i, k: CARDINAL;
BEGIN
  k := eve.QuantityEvents();
  len := 0;
  result := 0;
  FOR i := 1 TO k DO
    eve.GetEvent;
    ASSERT(eve.AddEvent(eve.LastEvent));

   <* IF DEFINED(xd_debug) & xd_debug THEN *>
    IF PrintStat THEN
      eve.PrintEvent(eve.LastEvent);
    END;
   <* END *>

    WITH eve.LastEvent DO
      IF (Event = eve.CompCreated) AND (Component.N_Objects # 0) THEN
        INC(len, SIZE(Component.Objects^))
      END;
    END;
  END;
  IF NOT med.SendResult(result, k*SIZE(eve.LastEvent)+len) THEN printf('\nSend result error'); END;
  FOR i := 1 TO k DO
    eve.GetEvent;
    IF NOT med.Send(sys.ADR(eve.LastEvent), SIZE(eve.LastEvent)) THEN printf('\nSend result error'); END;
    WITH eve.LastEvent DO
      IF (Event = eve.CompCreated) AND (Component.N_Objects # 0) THEN
        IF NOT med.Send(sys.ADR(Component.Objects^), SIZE(Component.Objects^)) THEN printf('\nSend result error'); END;
      END;
    END;
  END;
END SendQueueEvents;


BEGIN
  Init;
  ParseCommandLine;
  InitTransport;

  LOOP
    printf('\n');
    command := med.ReceiveCommand(arg_size);
    IF (arg_size # 0) AND NOT med.Receive (sys.ADR(arg_buf), arg_size) THEN
      printf("Command argument wasn't received.");
      HALT (10);
    END;
    INC (Commands[command]);
    CASE command OF
    | rmt.StartProgram:
      printf('\nStart program');
      IF CHAR(arg_buf[arg_size]) = 0C THEN
        name := sys.ADR(arg_buf[0]);
        prog_args := sys.ADR(arg_buf[LENGTH(name^)+1]);
        COPY(name^, str);
        printf("\nProgram %s, ", str);
        COPY(prog_args^, str);
        printf("arguments %s", str);
        result := kpr.StartProgram(name^, prog_args^);
      ELSE
        result := MAX(CARDINAL);
      END;
      IF result = 0 THEN
        SendQueueEvents;
      ELSE
        printf('\nStart program error, result %u', result);
        IF NOT med.SendResult (result, 0) THEN printf('\nSend result error'); END;
      END;

     | rmt.GetExecInfo:
      printf('\nGet exec info');
      IF kpr.GetProgramInfo(prog_info) THEN
        result := 0;
      ELSE
        result := MAX(CARDINAL);
      END;
      IF NOT med.SendResult(result, SIZE(prog_info) + SIZE(prog_info.Objects^)) THEN printf('\nSend result error'); END;
      IF NOT med.Send(sys.ADR(prog_info), SIZE(prog_info)) THEN printf('\nSend result error'); END;
      IF NOT med.Send(sys.ADR(prog_info.Objects^), SIZE(prog_info.Objects^)) THEN printf('\nSend result error'); END;

    | rmt.GetDebugInfo:
      printf('\nGet debug information');
      result := MAX(CARDINAL);
      IF arg_size = SIZE(prog_info) THEN
        sys.MOVE(sys.ADR(arg_buf), sys.ADR(prog_info), arg_size);
        IF prog_info.DebugInfoSize # 0 THEN
          IF debug_info # NIL THEN
            IF prog_info.DebugInfoSize > HIGH(debug_info^)+1 THEN
              DISPOSE(debug_info);
              NEW(debug_info, prog_info.DebugInfoSize);
            END;
          ELSE
            NEW(debug_info, prog_info.DebugInfoSize);
          END;
          IF debug_info = NIL THEN
            printf('\nOut of memory (possibly heaplimit is too small)');
            HALT (1);
          END;
          IF kpr.GetDebugInfo(prog_info, sys.ADR(debug_info^[0])) THEN result := 0; END;
        END;
      END;
      IF result # 0 THEN
        IF NOT med.SendResult(result, 0) THEN printf('\nSend result error'); END;
      ELSE
        IF NOT med.SendResult(result, prog_info.DebugInfoSize) THEN printf('\nSend result error'); END;
        IF NOT med.Send(sys.ADR(debug_info^[0]), prog_info.DebugInfoSize) THEN printf('\nSend result error'); END;
      END;

    | rmt.QuitDebug:
      printf('\nQuit debug');
      kpr.UnloadProgram;
      IF NOT med.SendResult (0, 0) THEN
        printf('\nSend result error');
      END;
      med.Disconnect;
      EXIT;

    | rmt.GetRegisterCache:
      printf('\nGet register cache');
      IF mem.GetRegisterCache(Cache) THEN
        result := 0;
        IF NOT med.SendResult(result, SIZE(Cache)) THEN printf('\nSend result error'); END;
        IF NOT med.Send(sys.ADR(Cache), SIZE(Cache)) THEN printf('\nSend result error'); END;
      ELSE
        result := 1;
        IF NOT med.SendResult(result, 0) THEN printf('\nSend result error'); END;
      END;

    | rmt.PutRegisterCache:
      printf('\nPut register cache');
      result := MAX(CARDINAL);
      IF arg_size = SIZE(Cache) THEN
        sys.MOVE(sys.ADR(arg_buf), sys.ADR(Cache), arg_size);
        IF mem.PutRegisterCache(Cache) THEN result := 0; END;
      END;
      IF NOT med.SendResult(result, 0) THEN printf('\nSend result error'); END;

    | rmt.GetMem:
      printf('\nGet memory');
      result := MAX(CARDINAL);
      IF arg_size = (SIZE(source)+SIZE(len)) THEN
        sys.MOVE(sys.ADR(arg_buf), sys.ADR(source), SIZE(source));
        sys.MOVE(sys.ADR(arg_buf[SIZE(source)]), sys.ADR(len), SIZE(len));
        printf("\nSource %$8X, length %i", source, len);
        IF len # 0 THEN
          IF len > SIZE(mem_buf) THEN
            IF debug_info # NIL THEN
              IF len > HIGH(debug_info^)+1 THEN
                DISPOSE(debug_info);
                NEW(debug_info, len);
              END;
            ELSE
              NEW(debug_info, len);
            END;
            mem_addr := sys.ADR(debug_info^[0]);
          ELSE
            mem_addr := sys.ADR(mem_buf);
          END;
          IF mem.Get(source, mem_addr, len) THEN result := 0; END;
        END;
      END;
      IF result # 0 THEN
        IF NOT med.SendResult(result, 0) THEN printf('\nSend result error'); END;
      ELSE
        IF NOT med.SendResult(result, len) THEN printf('\nSend result error'); END;
        IF NOT med.Send(mem_addr, len) THEN printf('\nSend result error'); END;
      END;

    | rmt.PutMem:
      printf('\nPut memory');
      result := MAX(CARDINAL);
      IF arg_size > SIZE(dest) THEN
        sys.MOVE(sys.ADR(arg_buf), sys.ADR(dest), SIZE(dest));
        len := arg_size - SIZE(dest);
        printf("\nDest %$8X, length %i", dest, len);
        IF mem.Put(dest, sys.ADR(arg_buf[SIZE(dest)]), len) THEN result := 0; END;
      END;
      IF NOT med.SendResult(result, 0) THEN printf('\nSend result error'); END;

    | rmt.Execute:
      printf('\nExecute program');
      IF arg_size = SIZE(GoMode) THEN
        sys.MOVE(sys.ADR(arg_buf), sys.ADR(GoMode), SIZE(GoMode));
        kpr.Execute(GoMode);
        SendQueueEvents;
      ELSE
        result := MAX(CARDINAL);
        IF NOT med.SendResult(result, 0) THEN printf('\nSend result error'); END;
      END;

    | rmt.GetSegmentInfo:
      printf('\nGet segment info');
      result := MAX(CARDINAL);
      IF arg_size = SIZE(dest) THEN
        sys.MOVE(sys.ADR(arg_buf), sys.ADR(dest), SIZE(dest));
        printf("\nDest %$8X", dest);
        IF mem.GetSegmentInfo (dest, source, len, access) THEN result := 0; END;
      END;
      IF result = 0 THEN
        IF NOT med.SendResult(result, SIZE(source)+SIZE(len)+SIZE(access)) THEN printf('\nSend result error'); END;
        IF NOT med.Send(sys.ADR(source), SIZE(source)) THEN printf('\nSend result error'); END;
        IF NOT med.Send(sys.ADR(len), SIZE(len)) THEN printf('\nSend result error'); END;
        IF NOT med.Send(sys.ADR(access), SIZE(access)) THEN printf('\nSend result error'); END;
      ELSE
        IF NOT med.SendResult(result, 0) THEN printf('\nSend result error'); END;
      END;

    | rmt.IsExecutableSeg:
      printf('\nIs executable segment');
      IF arg_size = SIZE(dest) THEN
        result := 0;
        sys.MOVE(sys.ADR(arg_buf), sys.ADR(dest), SIZE(dest));
        printf("\nDest %$8X", dest);
        res := mem.IsAddrFromExecutableSeg(dest);
        IF NOT med.SendResult(result, SIZE(res)) THEN printf('\nSend result error'); END;
        IF NOT med.Send(sys.ADR(res), SIZE(res)) THEN printf('\nSend result error'); END;
      ELSE
        result := MAX(CARDINAL);
        IF NOT med.SendResult(result, 0) THEN printf('\nSend result error'); END;
      END;

    | rmt.ReadExport:
      printf('\nRead Export');
      result := MAX(CARDINAL);
      IF (arg_size # 0) AND (CHAR(arg_buf[arg_size-1]) = 0C) THEN
        name := sys.ADR(arg_buf[0]);
        COPY(name^, str);
        printf("Export for %s", str);
        IF kpr.ReadExport (name^, exp) THEN
          result := 0;
        END;
      END;
      IF result = 0 THEN
        len := 4; -- N export record
        FOR i := 0 TO HIGH(exp^) DO
          WITH exp^[i] DO
            INC(len, 4+4+4+LENGTH(name)+1); -- obj, offset, lname, name
          END;
        END;
        IF NOT med.SendResult (result, len) THEN printf('\nSend result error'); END;
        len := HIGH(exp^)+1;
        IF NOT med.Send(sys.ADR(len), 4) THEN printf('\nSend result error'); END;
        FOR i := 0 TO HIGH(exp^) DO
          WITH exp^[i] DO
            len := obj;
            IF NOT med.Send(sys.ADR(len), 4) THEN printf('\nSend result error'); END;
            len := offset;
            IF NOT med.Send(sys.ADR(len), 4) THEN printf('\nSend result error'); END;
            len := LENGTH(name)+1;
            IF NOT med.Send(sys.ADR(len), 4) THEN printf('\nSend result error'); END;
            IF NOT med.Send(sys.ADR(name), len) THEN printf('\nSend result error'); END;
          END;
        END;
      ELSE
        printf('\nRead export error', result);
        IF NOT med.SendResult (result, 0) THEN printf('\nSend result error'); END;
      END;

    | rmt.NThreads:
      printf('\nGet number of threads ');
      IF arg_size = 0 THEN
        i := eth.NThreads ();
        printf("\nThreads = %d", i);
        result := 0;
        IF NOT med.SendResult (result, SIZE(i)) THEN printf('\nSend result error'); END;
        IF NOT med.Send (sys.ADR(i), SIZE(i)) THEN printf('\nSend result error'); END;
      ELSE
        result := MAX(CARDINAL);
        printf('\nWrong argument');
        IF NOT med.SendResult (result, 0) THEN printf('\nSend result error'); END;
      END;

    | rmt.GetThreadDescription:
      printf('\nGet thread description');
      IF arg_size = SIZE(i) THEN
        sys.MOVE(sys.ADR(arg_buf), sys.ADR(i), arg_size);
        printf("\nTID, n=%d", i);
        thr.GetThreadDescription (i, str);
        result := 0;
        IF NOT med.SendResult (result, LENGTH(str)+1) THEN printf('\nSend result error'); END;
        IF NOT med.Send (sys.ADR(str), LENGTH(str)+1) THEN printf('\nSend result error'); END;
      ELSE
        result := MAX(CARDINAL);
        printf('\nWrong argument');
        IF NOT med.SendResult (result, 0) THEN printf('\nSend result error'); END;
      END;

    | rmt.SwitchToThread:
      printf('\nSwitch to thread');
      IF arg_size = SIZE(i) THEN
        sys.MOVE(sys.ADR(arg_buf), sys.ADR(i), arg_size);
        printf("\nTID, n=%d", i);
        result := 0;
        res := thr.SwitchToThread (i);
        IF NOT med.SendResult (result, 1) THEN printf('\nSend result error'); END;
        IF NOT med.Send (sys.ADR(res), 1) THEN printf('\nSend result error'); END;
      ELSE
        result := MAX(CARDINAL);
        printf('\nWrong argument');
        IF NOT med.SendResult (result, 0) THEN printf('\nSend result error'); END;
      END;

    | rmt.SuspendThread:
      printf('\nSuspend thread');
      IF arg_size = SIZE(i) THEN
        sys.MOVE(sys.ADR(arg_buf), sys.ADR(i), arg_size);
        printf("\nTID, n=%d", i);
        result := 0;
        res := thr.SuspendThread (i);
        IF NOT med.SendResult (result, 1) THEN printf('\nSend result error'); END;
        IF NOT med.Send (sys.ADR(res), 1) THEN printf('\nSend result error'); END;
      ELSE
        result := MAX(CARDINAL);
        printf('\nWrong argument');
        IF NOT med.SendResult (result, 0) THEN printf('\nSend result error'); END;
      END;

    | rmt.ResumeThread:
      printf('\nResume thread');
      IF arg_size = SIZE(i) THEN
        sys.MOVE(sys.ADR(arg_buf), sys.ADR(i), arg_size);
        printf("\nTID, n=%d", i);
        result := 0;
        res := thr.ResumeThread (i);
        IF NOT med.SendResult (result, 1) THEN printf('\nSend result error'); END;
        IF NOT med.Send (sys.ADR(res), 1) THEN printf('\nSend result error'); END;
      ELSE
        result := MAX(CARDINAL);
        printf('\nWrong argument');
        IF NOT med.SendResult (result, 0) THEN printf('\nSend result error'); END;
      END;

    | rmt.SetTrace:
      printf("\nSet trace");
      IF arg_size = SIZE(trace_type)+SIZE(dest)+SIZE(len) THEN
        arg_addr := sys.ADR(arg_buf);
        i := SIZE(trace_type);
        sys.MOVE(arg_addr, sys.ADR(trace_type), i);
        arg_addr := sys.ADDADR(arg_addr, i);
        i := SIZE(dest);
        sys.MOVE(arg_addr, sys.ADR(dest), i);
        arg_addr := sys.ADDADR(arg_addr, i);
        i := SIZE(len);
        sys.MOVE(arg_addr, sys.ADR(len), i);
        printf("\nTrace type %X, dest %$8X, lenght %i", trace_type, dest, len);
        IF mem.SetTrace (trace_type, dest, len, Index) THEN
          result := 0;
          IF NOT med.SendResult (result, SIZE(Index)) THEN printf('\nSend result error'); END;
          IF NOT med.Send (sys.ADR(Index), SIZE(Index)) THEN printf('\nSend result error'); END;
        ELSE
          result := MAX(CARDINAL);
          IF NOT med.SendResult (result, 0) THEN printf('\nSend result error'); END;
        END;
      ELSE
        result := MAX(CARDINAL);
        printf('\nWrong argument');
        IF NOT med.SendResult (result, 0) THEN printf('\nSend result error'); END;
      END;


    | rmt.RemoveTrace:
      printf("\nRemove trace");
      IF arg_size = SIZE(Index) THEN
        sys.MOVE(sys.ADR(arg_buf), sys.ADR(Index), SIZE(Index));
        printf("\nTrace index %i", Index);
        IF mem.RemoveTrace (Index) THEN
          result := 0;
          IF NOT med.SendResult (result, SIZE(Index)) THEN printf('\nSend result error'); END;
          IF NOT med.Send (sys.ADR(Index), SIZE(Index)) THEN printf('\nSend result error'); END;
        ELSE
          printf('\nError remove trace');
          result := MAX(CARDINAL);
          IF NOT med.SendResult (result, 0) THEN printf('\nSend result error'); END;
        END;
      ELSE
        printf('\nWrong argument');
        result := MAX(CARDINAL);
        IF NOT med.SendResult (result, 0) THEN printf('\nSend result error'); END;
      END;

    | rmt.UnloadProgram:
      printf("\nUnload program");
      IF arg_size = 0 THEN
        kpr.UnloadProgram;
        result := 0;
      ELSE
        printf('\nWrong argument');
        result := MAX(CARDINAL);
      END;
      IF NOT med.SendResult (result, 0) THEN printf('\nSend result error'); END;

    | rmt.RestartProgram:
      printf("\nRestart program");
      IF arg_size = 0 THEN
        res := kpr.RestartProgram();
        result := 0;
        IF NOT med.SendResult (result, SIZE(res)) THEN printf('\nSend result error'); END;
        IF NOT med.Send (sys.ADR(res), SIZE(res)) THEN printf('\nSend result error'); END;
      ELSE
        printf('\nWrong argument');
        result := MAX(CARDINAL);
        IF NOT med.SendResult (result, 0) THEN printf('\nSend result error'); END;
      END;

    ELSE
      printf('\nSomething error, command code %u', ORD(command));
      med.Disconnect;
      EXIT;
    END;
  END;
FINALLY
  printf("\ncommand count\n");
  FOR command := MIN(rmt.COMMAND) TO MAX(rmt.COMMAND) DO
    printf("%-20s %d\n", rmt.CommandsNames[command], Commands[command]);
  END;
  med.Disconnect;
END XD_Serv.
