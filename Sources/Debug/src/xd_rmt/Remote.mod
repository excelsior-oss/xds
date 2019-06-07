<* Storage+ *>
<* ALIGNMENT = "1" *>

IMPLEMENTATION MODULE Remote;

IMPORT sys := SYSTEM;
IMPORT fmt := FormStr;

IMPORT kt  := KrnTypes;
IMPORT kpr := Krn_Prog;
IMPORT thr := Krn_Thrs;

IMPORT med := Media;
IMPORT rmt := RemTypes;

IMPORT opt := Options;
IMPORT eve := Events;
IMPORT xStr;

FROM Events IMPORT ACCESS_TYPE;


PROCEDURE GetRegisterCache(VAR RegisterCache: kt.REGISTER_CACHE): BOOLEAN;
VAR
  code, len: CARDINAL;
BEGIN
  IF NOT med.SendCommand (rmt.GetRegisterCache, 0) THEN RETURN FALSE; END;
  IF NOT med.ReceiveResult(code, len) THEN RETURN FALSE; END;
  RETURN (code = 0) AND (len = SIZE(RegisterCache)) AND
         med.Receive(sys.ADR(RegisterCache), SIZE(RegisterCache));
END GetRegisterCache;


PROCEDURE PutRegisterCache(RegisterCache: kt.REGISTER_CACHE): BOOLEAN;
VAR
  code, len: CARDINAL;
BEGIN
  IF NOT med.SendCommand (rmt.PutRegisterCache, SIZE(RegisterCache)) THEN RETURN FALSE; END;
  IF NOT med.Send(sys.ADR(RegisterCache), SIZE(RegisterCache)) THEN RETURN FALSE; END;
  RETURN med.ReceiveResult(code, len) AND (code = 0) AND (len = 0);
END PutRegisterCache;


PROCEDURE Get(source: kt.ADDRESS; dest: sys.ADDRESS; len: CARDINAL): BOOLEAN;
VAR
  code, ln: CARDINAL;
BEGIN
  IF NOT med.SendCommand (rmt.GetMem, SIZE(source)+SIZE(len)) THEN RETURN FALSE; END;
  IF NOT med.Send(sys.ADR(source), SIZE(source)) THEN RETURN FALSE; END;
  IF NOT med.Send(sys.ADR(len), SIZE(len)) THEN RETURN FALSE; END;
  IF NOT med.ReceiveResult(code, ln) THEN RETURN FALSE; END;
  RETURN (code = 0) AND (ln = len) AND med.Receive(dest, len);
END Get;


PROCEDURE Put(dest: kt.ADDRESS; source: sys.ADDRESS; len: CARDINAL): BOOLEAN;
VAR
  code: CARDINAL;
BEGIN
  IF NOT med.SendCommand (rmt.PutMem, SIZE(dest)+len) THEN RETURN FALSE; END;
  IF NOT med.Send(sys.ADR(dest), SIZE(dest)) THEN RETURN FALSE; END;
  IF NOT med.Send(source, len) THEN RETURN FALSE; END;
  RETURN med.ReceiveResult(code, len) AND (code = 0) AND (len = 0);
END Put;


PROCEDURE SetTrace (Access: ACCESS_TYPE; addr: kt.ADDRESS; len: CARDINAL; VAR Index: CARDINAL): BOOLEAN;
VAR
  code: CARDINAL;
BEGIN
  IF NOT med.SendCommand (rmt.SetTrace, SIZE(Access)+SIZE(addr)+SIZE(len)) THEN RETURN FALSE; END;
  IF NOT med.Send (sys.ADR(Access), SIZE(Access)) THEN RETURN FALSE; END;
  IF NOT med.Send (sys.ADR(addr), SIZE(addr)) THEN RETURN FALSE; END;
  IF NOT med.Send (sys.ADR(len), SIZE(len)) THEN RETURN FALSE; END;
  IF NOT med.ReceiveResult (code, len) OR (code # 0) OR (len # SIZE(Index)) THEN RETURN FALSE; END;
  IF NOT med.Receive (sys.ADR(Index), SIZE(Index)) THEN RETURN FALSE; END;
  RETURN TRUE;
END SetTrace;


PROCEDURE RemoveTrace (VAR index: CARDINAL): BOOLEAN;
VAR
  code, len: CARDINAL;
BEGIN
  IF NOT med.SendCommand (rmt.RemoveTrace, SIZE(index)) THEN RETURN FALSE; END;
  IF NOT med.Send (sys.ADR(index), SIZE(index)) THEN RETURN FALSE; END;
  IF NOT med.ReceiveResult (code, len) OR (code # 0) OR (len # SIZE(index)) THEN RETURN FALSE; END;
  IF NOT med.Receive (sys.ADR(index), SIZE(index)) THEN RETURN FALSE; END;
  RETURN TRUE;
END RemoveTrace;


PROCEDURE GetSegmentInfo(    addr  : kt.ADDRESS;
                         VAR begin : kt.ADDRESS;
                         VAR len   : CARDINAL;
                         VAR access: kt.ATTRIBS): BOOLEAN;
VAR
  code: CARDINAL;
BEGIN
  IF NOT med.SendCommand (rmt.GetSegmentInfo, SIZE(addr)) THEN RETURN FALSE; END;
  IF NOT med.Send(sys.ADR(addr), SIZE(addr)) THEN RETURN FALSE; END;
  IF NOT med.ReceiveResult(code, len) OR (code # 0) OR
     (len # SIZE(begin)+SIZE(len)+SIZE(access)) THEN RETURN FALSE; END;
  IF NOT med.Receive(sys.ADR(begin), SIZE(begin)) THEN RETURN FALSE; END;
  IF NOT med.Receive(sys.ADR(len), SIZE(len)) THEN RETURN FALSE; END;
  IF NOT med.Receive(sys.ADR(access), SIZE(access)) THEN RETURN FALSE; END;
  RETURN TRUE;
END GetSegmentInfo;


PROCEDURE IsAddrFromExecutableSeg (addr: kt.ADDRESS): BOOLEAN;
VAR
  result: BOOLEAN;
  code, len: CARDINAL;
BEGIN
  IF NOT med.SendCommand (rmt.IsExecutableSeg, SIZE(addr)) THEN RETURN FALSE; END;
  IF NOT med.Send(sys.ADR(addr), SIZE(addr)) THEN RETURN FALSE; END;
  IF NOT med.ReceiveResult(code, len) OR (code # 0) OR (len # SIZE(result)) THEN RETURN FALSE; END;
  IF NOT med.Receive(sys.ADR(result), SIZE(result)) THEN RETURN FALSE; END;
<* PUSH *>
<* WOFF304+ *>
  RETURN result;
<* POP *>
END IsAddrFromExecutableSeg;


PROCEDURE StartProgram (name-: ARRAY OF CHAR; args-: ARRAY OF CHAR): CARDINAL;
VAR
  event: eve.EVENT;
  error: BOOLEAN;

  MODULE InternalError;
  IMPORT eve, event, error;
  BEGIN
  FINALLY
    IF error THEN
      WITH event DO
        IP := 0;
        Event := eve.InternalError;
        ErrorNo := MAX(CARDINAL);
        ErrorContext := 0;
      END;
      ASSERT(eve.AddEvent(event));
    END;
  END InternalError;

VAR
  code  : CARDINAL;
  len   : CARDINAL;
  ln    : CARDINAL;
  pline : xStr.txt_ptr;
  buf   : ARRAY [0..1023] OF CHAR;
BEGIN
  error := FALSE;
  code := med.ClientConnect (opt.RemoteHost, VAL(rmt.TRN_PORT, opt.RemotePort));
  IF code # 0 THEN RETURN code; END;
  pline := sys.ADR(buf);
  COPY(name, pline^);
  len := LENGTH(pline^)+1;
  pline := sys.ADR(buf[len]);
  COPY(args, pline^);
  INC(len, LENGTH(pline^)+1);
  IF NOT med.SendCommand (rmt.StartProgram, len) THEN RETURN 2; END;
  IF NOT med.Send (sys.ADR(buf), len) THEN RETURN 2; END;
  IF NOT med.ReceiveResult (code, len) THEN RETURN 3; END;
  IF code # 0 THEN RETURN code; END;
  error := TRUE;
  ln := 0;
  LOOP
    IF ln = len THEN EXIT; END;
    ASSERT( (ln < len) AND (SIZE(event) <= len-ln) );
    IF NOT med.Receive (sys.ADR(event), SIZE(event)) THEN RETURN 3; END;
    INC(ln, SIZE(event));
    WITH event DO
      IF (Event = eve.CompCreated) AND (Component.N_Objects # 0) THEN
        NEW(Component.Objects, Component.N_Objects);
        IF NOT med.Receive(sys.ADR(Component.Objects^[0]), SIZE(Component.Objects^)) THEN RETURN 3; END;
        INC(ln, SIZE(Component.Objects^));
      END;
    END;
    ASSERT(eve.AddEvent(event));
  END;
  error := FALSE;
  RETURN 0;
END StartProgram;


PROCEDURE GetProgramInfo (VAR inf: kt.EXEC_INFO): BOOLEAN;
VAR
  code: CARDINAL;
  len : CARDINAL;
BEGIN
  IF NOT med.SendCommand (rmt.GetExecInfo, 0) THEN RETURN FALSE; END;
  IF NOT med.ReceiveResult (code, len) THEN RETURN FALSE; END;
  IF (code = 0) AND (len > SIZE(kt.EXEC_INFO)) THEN
    IF NOT med.Receive(sys.ADR(inf), SIZE(kt.EXEC_INFO)) THEN RETURN FALSE; END;
    IF len # SIZE(kt.EXEC_INFO) + inf.N_Objects*SIZE(kt.OBJECT) THEN RETURN FALSE END;
    NEW(inf.Objects, inf.N_Objects);
    IF NOT med.Receive(sys.ADR(inf.Objects^), len - SIZE(kt.EXEC_INFO)) THEN RETURN FALSE; END;
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END GetProgramInfo;


PROCEDURE GetDebugInfo(exec_info: kt.EXEC_INFO; info_addr: sys.ADDRESS): BOOLEAN;
VAR
  code: CARDINAL;
  len : CARDINAL;
BEGIN
  IF NOT med.SendCommand (rmt.GetDebugInfo, SIZE(exec_info)) THEN RETURN FALSE; END;
  IF NOT med.Send (sys.ADR(exec_info), SIZE(exec_info)) THEN RETURN FALSE; END;
  IF (NOT med.ReceiveResult (code, len)) OR (code # 0) OR (len # exec_info.DebugInfoSize) THEN RETURN FALSE; END;
  RETURN med.Receive(info_addr, len);
END GetDebugInfo;



PROCEDURE UnloadProgram;
VAR
  code: CARDINAL;
  len : CARDINAL;
BEGIN
<* PUSH *>
<* WOFF900+ *>
<* WOFF903+ *>
  IF med.SendCommand (rmt.UnloadProgram, 0) AND
     med.ReceiveResult (code, len)
  THEN
  END;
<* POP *>
END UnloadProgram;


PROCEDURE RestartProgram (): BOOLEAN;
VAR
  code: CARDINAL;
  len : CARDINAL;
  res : BOOLEAN;
BEGIN
  res := FALSE;
  IF NOT med.SendCommand (rmt.RestartProgram, 0) THEN RETURN FALSE; END;
  IF NOT med.ReceiveResult (code, len) OR (code#0) OR (len#SIZE(res)) THEN RETURN FALSE; END;
  IF NOT med.Receive (sys.ADR(res), SIZE(res)) THEN RETURN FALSE; END;
  RETURN res;
END RestartProgram;


PROCEDURE Execute (go_mode: kpr.GO_MODE);
VAR
  event: eve.EVENT;
  error: BOOLEAN;

  MODULE InternalError;
  IMPORT eve, event, error;
  BEGIN
  FINALLY
    IF error THEN
      WITH event DO
        IP := 0;
        Event := eve.InternalError;
        ErrorNo := MAX(CARDINAL);
        ErrorContext := 0;
      END;
      ASSERT(eve.AddEvent(event));
    END;
  END InternalError;

VAR
  code, len, ln: CARDINAL;

BEGIN
  error := TRUE;
  IF NOT med.SendCommand (rmt.Execute, SIZE(go_mode)) THEN RETURN; END;
  IF NOT med.Send (sys.ADR(go_mode), SIZE(go_mode)) THEN RETURN; END;
  IF NOT med.ReceiveResult (code, len) OR (code#0) THEN RETURN; END;
  ln := 0;
  LOOP
    IF ln = len THEN EXIT; END;
    ASSERT( (ln < len) AND (SIZE(event) <= len-ln) );
    IF NOT med.Receive (sys.ADR(event), SIZE(event)) THEN RETURN; END;
    INC(ln, SIZE(event));
    WITH event DO
      IF (Event = eve.CompCreated) AND (Component.N_Objects # 0) THEN
        NEW(Component.Objects, Component.N_Objects);
        IF NOT med.Receive(sys.ADR(Component.Objects^[0]), SIZE(Component.Objects^)) THEN RETURN; END;
        INC(ln, SIZE(Component.Objects^));
      END;
    END;
    ASSERT(eve.AddEvent(event));
  END;
  error := FALSE;
END Execute;


PROCEDURE ReadExport (full_name: ARRAY OF CHAR; VAR exp: kt.EXPORTS): BOOLEAN;
VAR
  error: BOOLEAN;
  code : CARDINAL;
  len  : CARDINAL;
  read : CARDINAL;
  lname: CARDINAL;
  N    : CARDINAL;
  curr : CARDINAL;

  MODULE Dispose;
  IMPORT error, exp;
  BEGIN
  FINALLY
    IF error AND (exp # NIL) THEN DISPOSE(exp); END;
  END Dispose;

BEGIN
  exp := NIL;
  error := TRUE;
  IF NOT med.SendCommand (rmt.ReadExport, LENGTH(full_name)+1) THEN RETURN FALSE; END;
  IF NOT med.Send (sys.ADR(full_name),  LENGTH(full_name)+1) THEN RETURN FALSE; END;
  IF NOT med.ReceiveResult (code, len) OR (code#0) THEN RETURN FALSE; END;
  N := 0;
  IF NOT med.Receive (sys.ADR(N), 4) OR (N=0) THEN RETURN FALSE; END;
  read := 4;
  NEW(exp, N);
  curr := 0;
  lname := 0;
  LOOP
    WITH exp^[curr] DO
      IF NOT med.Receive (sys.ADR(obj), 4) THEN RETURN FALSE; END;
      INC(read, 4);
      IF NOT med.Receive (sys.ADR(offset), 4) THEN RETURN FALSE; END;
      INC(read, 4);
      IF NOT med.Receive (sys.ADR(lname), 4) THEN RETURN FALSE; END;
      INC(read, 4);
      IF NOT med.Receive (sys.ADR(name), lname) THEN RETURN FALSE; END;
      INC(read, lname);
    END;
    INC(curr);
    IF curr = N THEN EXIT; END;
  END;
  error := len # read;
  RETURN NOT error;
END ReadExport;


PROCEDURE GetThreadDescription (i: CARDINAL; VAR buf: ARRAY OF CHAR);
VAR
  code, len: CARDINAL;
BEGIN
  IF NOT med.SendCommand (rmt.GetThreadDescription, SIZE(i)) OR
     NOT med.Send (sys.ADR(i), SIZE(i)) OR
     NOT med.ReceiveResult (code, len) OR (code#0) OR
     NOT med.Receive (sys.ADR(buf), len) THEN
     fmt.print (buf, '???');
  END;
END GetThreadDescription;


PROCEDURE NThreads (): CARDINAL;
VAR
  code: CARDINAL;
  len : CARDINAL;
  N   : CARDINAL;
BEGIN
  IF NOT med.SendCommand (rmt.NThreads, 0) THEN
    RETURN 0;
  END;
  IF NOT med.ReceiveResult (code, len) OR (code#0) OR (len # SIZE(N)) THEN
    RETURN 0;
  END;
  IF NOT med.Receive (sys.ADR(N), SIZE(N)) THEN
    RETURN 0;
  END;
  RETURN N;
END NThreads;


PROCEDURE ThreadCommand (command: rmt.COMMAND; i: CARDINAL): BOOLEAN;
VAR
  code: CARDINAL;
  len : CARDINAL;
  res : BOOLEAN;
BEGIN
  IF NOT med.SendCommand (command, SIZE(i)) THEN RETURN FALSE; END;
  IF NOT med.Send (sys.ADR(i), SIZE(i)) THEN RETURN FALSE; END;
  IF NOT med.ReceiveResult (code, len) OR (code#0) OR (len#1) THEN RETURN FALSE; END;
  res := FALSE;
  RETURN med.Receive (sys.ADR(res), len) AND res;
END ThreadCommand;


PROCEDURE SwitchToThread (i: CARDINAL): BOOLEAN;
BEGIN
  RETURN ThreadCommand (rmt.SwitchToThread, i);
END SwitchToThread;


PROCEDURE SuspendThread (i: CARDINAL): BOOLEAN;
BEGIN
  RETURN ThreadCommand (rmt.SuspendThread, i);
END SuspendThread;


PROCEDURE ResumeThread (i: CARDINAL): BOOLEAN;
BEGIN
  RETURN ThreadCommand (rmt.ResumeThread, i);
END ResumeThread;


PROCEDURE Quit;
VAR
  code, len: CARDINAL;
BEGIN
  sys.EVAL(med.SendCommand (rmt.QuitDebug, 0));
  sys.EVAL(med.ReceiveResult (code, len));
END Quit;


PROCEDURE CloseConnection ();
BEGIN
  IF opt.RemoteMode AND med.IsConnected() THEN
    Quit();
    med.Disconnect;
  END;
END CloseConnection;

BEGIN
FINALLY
  CloseConnection;
END Remote.
