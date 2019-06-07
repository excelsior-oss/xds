MODULE xProfMem;

IMPORT sys := SYSTEM;
IMPORT arg := ProgEnv;
IMPORT fmt := FormStr;

IMPORT pro := Protocol;
IMPORT xs  := xStr;
IMPORT opt := Options;
IMPORT msg := MsgNo;
IMPORT fil := File;

IMPORT dt  := DI_Types;
IMPORT tls := DI_Tools;

IMPORT kt  := KrnTypes;
IMPORT kex := KrnExec;

<* IF env_target = 'x86nt' THEN *>
IMPORT dbg := Krn_Dbg;
<* END *>

IMPORT erc := ExeReact;
IMPORT eve := Events;
IMPORT exe := ExeMain;
IMPORT mem := Exe_Mem;

IMPORT stk := CallStk;
IMPORT xcp := XCPT_Msg;

IMPORT pt  := PrfTypes;
IMPORT pb  := PrfBuild;

FROM Printf IMPORT printf;



CONST
  PRODUCT   = "memory trace utility";
  COPYRIGHT = "1998-2001 Excelsior";


PROCEDURE Copyright;
BEGIN
  printf("\n%s, %s, Version %s\n(c) %s\n\n", pt.IDENTKEY, PRODUCT, pt.VERSION, COPYRIGHT);
END Copyright;


CONST
  DEFAULT_DEPTH = 2;


VAR
  prog_name      : xs.String;
  prog_args      : xs.String;
  depth          : CARDINAL;
  show           : BOOLEAN;
  used_mem_addr  : kt.ADDRESS;


(*
 MODULE xrMM;

 PROCEDURE allocate (VAR l: types.X2C_LINK; size: CARDINAL);
 PROCEDURE dealloc  (n: types.X2C_LINK);

  X2C_LINK_STR = RECORD
    CASE :INTEGER OF
      |0: td   : X2C_TD; (* must have offset = 0, X2C_GET_TD *)
      |1: next : X2C_LINK;
      |2: dummy: CARD32;
    END;
    CASE :INTEGER OF
      |0: size : CARD32; (* must be 32 bits width! *)
      |1: tags : SET32;
    END;
  END;

*)

CONST
  PROC_NO = 2;

TYPE
  PROC_NAME = ARRAY [0..31] OF CHAR;

  PROC = RECORD
           Ready: BOOLEAN;
           Name : PROC_NAME;
           Inx  : CARDINAL;
         END;


  PROCS = ARRAY [0..PROC_NO-1] OF PROC;


VAR
  Procs: PROCS;


PROCEDURE Init;
VAR
  com: dt.ComNo;
BEGIN
 <* IF env_target = 'x86nt' THEN *>
  dbg.SetTitle ("The XDS Profiler");
 <* END *>
  prog_name       := '';
  prog_args       := '';
  depth           := DEFAULT_DEPTH;
  show            := FALSE;
  pb.MemTraceResults := pt.MEM_TRACE_RESULT{ 0, 0, 0, 0, 0, 0 };
  WITH Procs[0] DO
    Name := "xrMM_allocate";
    Ready := FALSE;
  END;
  WITH Procs[1] DO
    Name := "xrMM_dealloc";
    Ready := FALSE;
  END;
  IF NOT tls.FindPublicByName ("X2C_usedmem", com, used_mem_addr) THEN
    used_mem_addr := 0;
  END;
END Init;


PROCEDURE UsedMem (): CARDINAL;
VAR
  used: CARDINAL;
BEGIN
  IF used_mem_addr > 0 THEN
    used := 0;
    IF mem.Get (used_mem_addr, sys.ADR(used), 4) THEN
      RETURN used;
    ELSE
      RETURN 0;
    END;
  ELSIF pb.MemTraceResults.alloc_mem > pb.MemTraceResults.dealloc_mem THEN
    RETURN pb.MemTraceResults.alloc_mem - pb.MemTraceResults.dealloc_mem;
  ELSE
    RETURN 0;
  END;
END UsedMem;



PROCEDURE Help;
BEGIN
  printf("Usage:   xprofmem [ ('-'|'/') options ] program [ arguments ]\n\n");
  printf('Options: d=<depth>    depth of call stack\n');
  printf('         s            show call stack\n');
  printf('         default: depth=%u\n', DEFAULT_DEPTH);
  printf('                  show=off\n');
  HALT (0);
END Help;


PROCEDURE Options (opt-: ARRAY OF CHAR);
VAR
  s : xs.String;
  ok: BOOLEAN;
BEGIN
  CASE CAP(opt[1]) OF
  | 'D' : IF opt[2] # '=' THEN Help; END;
          xs.Extract(opt, 3, LENGTH(opt), s);
          depth := xs.StrToCard(s, 10, ok);
          IF NOT ok THEN Help; END;
  | 'S' : IF opt[2] # 0C THEN Help; END;
          show := TRUE;
  ELSE
    Help;
  END;
END Options;


PROCEDURE ParseCommandLine;
VAR
  k, i, p: CARDINAL;
  a: xs.String;
  f: BOOLEAN;
BEGIN
  k := arg.ArgNumber();
  IF k = 0 THEN Help; END;
  i := 0;
  LOOP
    IF (i = k) THEN EXIT; END;
    arg.GetArg(i, a);
    IF (a[0] = '/') OR (a[0] = '-') THEN
      Options(a);
    ELSE
      EXIT;
    END;
    INC(i);
  END;
  IF i = k THEN Help; END;
  arg.GetArg(i, prog_name);
  fil.AddExtension(prog_name, 'exe');
  prog_args := '';
  INC(i);
  WHILE i < k DO
    arg.GetArg(i, a);
    <* PUSH *>
    <* WOFF903+ *>
    p := xs.CharPos (a, ' ', f);
    <* POP *>
    IF f THEN
      xs.Insert ('"', 0, a);
      xs.Append ('"', a);
    END;
    INC(i);
    xs.Append(a, prog_args);
    xs.Append(' ', prog_args);
  END;
END ParseCommandLine;



PROCEDURE InternalError (data: erc.DATA);
BEGIN
  ASSERT(data=erc.DATA(NIL));
  ASSERT(eve.LastEvent.Event = eve.InternalError);
  printf ("Error: some internal profiler error #%u\n", eve.LastEvent.ErrorNo);
  HALT (1);
END InternalError;



PROCEDURE SetBreakpoints;
VAR
  com : dt.ComNo;
  addr: kt.ADDRESS;
BEGIN
  WITH Procs[0] DO
    IF NOT Ready THEN
      Ready := tls.FindPublicByName (Name, com, addr);
      IF Ready THEN
        ASSERT(exe.SetBreakPoint(addr, Inx));
      END;
    END;
  END;
  WITH Procs[1] DO
    IF NOT Ready THEN
      Ready := tls.FindPublicByName (Name, com, addr);
      IF Ready THEN
        ASSERT(exe.SetBreakPoint(addr, Inx));
      END;
    END;
  END;
END SetBreakpoints;


PROCEDURE ComponentCreated (data: erc.DATA);
VAR
  com: dt.ComNo;
  inx: dt.INDEX;
BEGIN
  ASSERT(data=erc.DATA(NIL));
  ASSERT(eve.LastEvent.Event = eve.CompCreated);
  SetBreakpoints;
  IF (used_mem_addr = 0) AND NOT tls.FindPublicByName ("X2C_usedmem", com, used_mem_addr) THEN
    used_mem_addr := 0;
  END;
  ASSERT(tls.FindComponentByHandle (eve.LastEvent.Component.Handle, com));
  ASSERT(tls.GetIndex (com, inx));
  pb.AddTableComp (0, 0, eve.LastEvent.Component.full_name, inx);
END ComponentCreated;



PROCEDURE ProcessingBreakpoints (data: erc.DATA);
VAR
  size: CARDINAL;
  call: stk.CALL;
  num : CARDINAL;
  name: xs.txt_ptr;
  d   : CARDINAL;
  used: pt.MEM_USED;
  addr: kt.ADDRESS;
  obj : dt.OBJECT;
  com : dt.ComNo;
BEGIN
  ASSERT(data=erc.DATA(NIL));
  ASSERT(eve.LastEvent.Event = eve.BreakpointHit);
  IF eve.QueryEvent (eve.SingleStep) THEN
    ASSERT(eve.AddEvent(eve.LastEvent));
    RETURN;
  END;
  IF eve.LastEvent.BreakpointInd = exe.TmpBreakpointIndex THEN
    exe.RemoveTmpBreakPoint();
    exe.StopExec;
  ELSIF Procs[0].Ready AND (eve.LastEvent.BreakpointInd = Procs[0].Inx) THEN
    -- allocate
    size := UsedMem();
    IF pb.MemTraceResults.alloc_max_total < size THEN
      pb.MemTraceResults.alloc_max_total := size;
    END;
    ASSERT(mem.Get (mem.GetSP()+8, sys.ADR(size), 4));
    IF pb.MemTraceResults.alloc_max_size < size THEN
      pb.MemTraceResults.alloc_max_size := size;
    END;
    INC(pb.MemTraceResults.alloc_mem, size);
    INC(pb.MemTraceResults.alloc_count);
    stk.ResetCallStack;
    stk.ScanCallStack;
    IF stk.SkipProcWithoutDebugInfo (addr, obj) THEN
      WITH used DO
        com := tls.ObjectCom(obj);
        ASSERT(tls.FindComObjByAddrInCom (com, addr, d));
        Object := d;
        Offset := addr - dt.Components.Components^[com].EI.Objects^[Object].Begin;
        ASSERT(tls.GetIndex (com, Index));
        Size := size;
        Addr := 0;
      END;
      IF show THEN
        printf ("allocate ");
        printf (" %u\n", size);
        ASSERT(stk.GetFirstProc (num));
        d := 0;
        LOOP
          IF d = depth THEN EXIT; END;
          stk.GetCall (num, call);
          WITH call DO
            IF tls.EqualObjects (Object, dt.Invalid_Object) THEN
              EXIT;
            ELSE
              IF tls.SourceByAddrInMod (com, mod, call_addr, line) THEN
                printf ("  %5u ", line);
              ELSE
                printf ("       ");
              END;
              IF tls.ModName (com, mod, name) THEN
                printf (" %s.", name^);
              END;
              printf ("%s\n", Name);
            END;
          END;
          IF num = 0 THEN EXIT; END;
          INC(d);
          INC(num);
        END;
      END;
    ELSE
      stk.GetCall (0, call);
      WITH used DO
        ASSERT(tls.FindComObjByAddr (call.call_addr, com, d));
        Object := d;
        Offset := call.call_addr - dt.Components.Components^[com].EI.Objects^[Object].Begin;
        ASSERT(tls.GetIndex (com, Index));
        Size := size;
        Addr := 0;
      END;
    END;
    pb.AddTableMemUsed (used);
  ELSIF Procs[1].Ready AND (eve.LastEvent.BreakpointInd = Procs[1].Inx) THEN
    -- deallocate
    INC(pb.MemTraceResults.dealloc_count);
    IF show THEN
      printf ("dealloc\n");
    END;
  ELSE
    ASSERT(FALSE);
  END;
END ProcessingBreakpoints;



PROCEDURE ExceptionReaction (data: erc.DATA);
VAR
  text: xs.String;
  m   : xs.String;
  xcpt: xs.String;
BEGIN
  ASSERT(data=erc.DATA(NIL));
  ASSERT(eve.LastEvent.Event = eve.Exception);
  IF eve.QueryEvent (eve.SingleStep) THEN
    ASSERT(eve.AddEvent(eve.LastEvent));
    RETURN;
  END;
  CASE eve.LastEvent.Exception_ID OF
  | eve.OutOfMemory     : (* Доступ по адресу вне диапазона адресов *)
    pro.GetMsg(msg.BreakOutMem, m);
    fmt.print(text, m, CARDINAL(eve.LastEvent.XCPT_INFO_1));
  | eve.ProgramException: (* Программное прерывание                 *)
    WITH eve.LastEvent DO
      IF CARDINAL(XCPT_INFO_1) = 0 THEN
       <* IF DEST_K26 THEN *>
        fmt.print(text, 'Программа завершилась');
       <* ELSE *>
        fmt.print(text, 'Program finished');
       <* END *>
      ELSE
        pro.GetMsg(msg.BreakProgInt, m);
        xcp.Get_XCPT_Msg(CARDINAL(XCPT_INFO_1), CARDINAL(XCPT_INFO_4), xcpt);
        fmt.print(text, m, xcpt, CARDINAL(XCPT_INFO_2));
      END;
    END;
  | eve.WriteProtected  : (* Запись в защищенную область памяти     *)
    pro.GetMsg(msg.BreakWrProt, m);
    xcp.Get_XCPT_Msg(CARDINAL(eve.LastEvent.XCPT_INFO_4), 0, xcpt);
    fmt.print(text, m, xcpt, CARDINAL(eve.LastEvent.XCPT_INFO_1));
  | eve.UserException   : (* Исполнение прервано пользователем      *)
    pro.GetMsg(msg.User_React_Default,text);
  END;
  pro.GetMsg (msg.Last_address, m);
  printf (m, eve.LastEvent.IP, text);
  printf ("\n\n");
  exe.StopExec;
  kex.ProgramContextOk := FALSE;
  exe.StopQueueProc;
  eve.ClearQueue;
  erc.StopReact;
  erc.CancelReact;
END ExceptionReaction;


PROCEDURE InitReactions;
BEGIN
  erc.AddAction (eve.InternalError,  NIL, InternalError);
  erc.AddAction (eve.Exception,      NIL, ExceptionReaction);
  erc.AddAction (eve.BreakpointHit,  NIL, ProcessingBreakpoints);
  erc.AddAction (eve.CompCreated,    NIL, ComponentCreated);
END InitReactions;


PROCEDURE LoadProgram;
VAR
  rc : CARDINAL;
  tmp: xs.String;
  inx: dt.INDEX;
  txt: xs.txt_ptr;
BEGIN
--  pb.AddTableComp (0, 0, prog_name, 0);
  rc := kex.LoadProgram(prog_name, prog_args);
  IF rc # 0 THEN
    pro.GetMsg (rc, tmp);
    printf (tmp, prog_name, prog_args);
    HALT (2);
  END;
  ASSERT(tls.GetIndex (0, inx));
  ASSERT(tls.ComFullName (0, txt));
  pb.AddTableComp (0, 0, txt^, inx);
END LoadProgram;


PROCEDURE ExecuteProgram;

  MODULE WriteProtocol;

  IMPORT xs, fil, pt, pb, prog_name, printf;

  VAR
    tmp: xs.String;

  BEGIN
  FINALLY
    fil.ExtractFileName (prog_name, tmp);
    IF pb.WriteProtocol (tmp, pt.TRACE_MEMORY) < 0 THEN
      printf('Can not write trace file. (%s)\n', tmp);
    ELSE
      printf('Trace file was successfully created. (%s)\n', tmp);
    END;
  END WriteProtocol;

BEGIN
  printf("Program %s is running...\n", prog_name);
  exe.Go;
  kex.UnloadProgram;
  printf("Program terminated.\n\n");
EXCEPT
  printf("Ctrl-Break pressed, program stopped.\n\n");
  RETURN;
END ExecuteProgram;



BEGIN
  Copyright;
  pro.InitProtocol ("xd.msg");
  Init;
  ParseCommandLine;
  InitReactions;
  LoadProgram;
  SetBreakpoints;
  ExecuteProgram;
END xProfMem.