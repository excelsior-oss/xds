-- Загрузка программы, отгрузка, рестарт, остановы, исполнение,
-- режимы исполнения, получение времен исполнения.
-- Модуль является системо-зависимым.

IMPLEMENTATION MODULE Krn_Prog;

<* Storage+ *>

IMPORT sys := SYSTEM;
IMPORT io  := InOut;
IMPORT rf  := RndFile;
IMPORT ioc := IOChan;
IMPORT pio := POSIXIOChan;
IMPORT xfp := xFilePos;
IMPORT rio := RawIO;
--IMPORT brk := CtrlC;
--IMPORT wio := Win32IOChan;
IMPORT env := ProgEnv;
IMPORT  fs := FileSys;

--IMPORT win := Windows;
IMPORT lnx := Linux;


IMPORT xs  := xStr;
IMPORT fil := File;
IMPORT opt := Options;
IMPORT msg := MsgNo;

IMPORT eve := Events;
IMPORT dt  := DI_Types;

IMPORT kmem:= Krn_Mem;
IMPORT kt  := KrnTypes;
IMPORT dbg := Krn_Dbg;
IMPORT kth := Krn_Thrs;
IMPORT dth := Def_Thrs;

IMPORT rmt := Remote;
IMPORT re  := ReadExp;
IMPORT od  := OutDebug;

IMPORT thr := Threads;
IMPORT prg := PrgNames;


FROM Krn_Dbg IMPORT H_Thread;--, H_Process;--, Id_Process, Id_Thread;
FROM Printf IMPORT printf;

FROM SYSTEM IMPORT INT32, CARD32;




VAR
  info    : kt.EXEC_INFO;
  Loaded  : BOOLEAN;
  Finished: BOOLEAN;

  name_save, args_save: xs.String;
  argv: POINTER TO lnx.ARGS_ARRAY;

(*
  Title, Cmd_Line: xs.String;
  startup_info   : win.STARTUPINFO;
  security_attrs : win.SECURITY_ATTRIBUTES;
  creation_flags : win.CREATE_SET;
*)

  di: lnx.debug_event;

--VAR
--  signal: INT32;

VAR
  cont_flag: BOOLEAN; -- TRUE if exception was not handled



<* IF DEFINED(xd_debug) & xd_debug THEN *>

PROCEDURE PrintNotification;
BEGIN
END PrintNotification;

<* END *>

PROCEDURE MyStrCopy(src: lnx.PCHAR; VAR dest: kt.PROGRAM_NAME);
VAR
  i: INTEGER;
  ch: CHAR;
BEGIN
  i := 0;
  LOOP
    ch := src^[i];
    dest[i] := ch;
    IF ch=0C THEN RETURN; END;
    INC (i);
  END;
END MyStrCopy;




(* Получить текущее значение IP *)
PROCEDURE GetIP (): kt.ADDRESS;
VAR
  RegisterCache: kt.REGISTER_CACHE;
BEGIN
  IF kmem.GetRegisterCache (RegisterCache) THEN
    RETURN RegisterCache.Eip + RegisterCache.BaseCs;
  ELSE
    RETURN 0;
  END;
END GetIP;



(*
PROCEDURE AddThread (Id: CARDINAL; handle: lnx.THANDLE): kth.THREAD_INX;
VAR
  inx: kth.THREAD_INX;
BEGIN
  inx := thr.NewThread ();
  thr.Threads.Threads^[inx].Handle := handle;
  thr.Threads.Threads^[inx].ID := Id;
  RETURN inx;
END AddThread;


PROCEDURE FindThreadById (Id: lnx.TID; VAR inx: kth.THREAD_INX): BOOLEAN;
VAR
  i: CARDINAL;
BEGIN
  IF thr.Threads.Count > 0 THEN
    FOR i := 0 TO thr.Threads.Count - 1 DO
      IF thr.Threads.Threads^[i].ID = Id THEN
        inx := i;
        RETURN TRUE;
      END;
    END;
  END;
  RETURN FALSE;
END FindThreadById;


PROCEDURE GetThreadHandle (inx: kth.THREAD_INX): lnx.THANDLE;
BEGIN
  RETURN thr.Threads.Threads^[inx].Handle;
END GetThreadHandle;


PROCEDURE RemoveThread (Id: CARDINAL);
VAR
  inx: kth.THREAD_INX;
BEGIN
  ASSERT (FindThreadById (Id, inx));
  thr.ThreadKilled (inx);
END RemoveThread;
*)

PROCEDURE AddThread (handle: lnx.THANDLE): kth.THREAD_INX;
VAR
  inx: kth.THREAD_INX;
BEGIN
--printf ("AddThread: handle=0x%x\n", handle);
  inx := thr.NewThread ();
  thr.Threads.Threads^[inx].Handle := handle;
  RETURN inx;
END AddThread;


PROCEDURE FindThreadByHandle (handle: lnx.THANDLE; VAR inx: kth.THREAD_INX): BOOLEAN;
VAR
  i: CARDINAL;
BEGIN
  IF thr.Threads.Count > 0 THEN
    FOR i := 0 TO thr.Threads.Count - 1 DO
      IF thr.Threads.Threads^[i].Handle = handle THEN
        inx := i;
        RETURN TRUE;
      END;
    END;
  END;
  RETURN FALSE;
END FindThreadByHandle;


PROCEDURE GetThreadHandle (inx: kth.THREAD_INX): lnx.THANDLE;
BEGIN
  RETURN thr.Threads.Threads^[inx].Handle;
END GetThreadHandle;


PROCEDURE RemoveThread (handle: lnx.THANDLE);
VAR
  inx: kth.THREAD_INX;
BEGIN
  IF NOT FindThreadByHandle (handle, inx) THEN
    ASSERT (FALSE);
  END;
  thr.ThreadKilled (inx);
END RemoveThread;


(*
PROCEDURE FillDebugInfo(VAR info: kt.EXEC_INFO): BOOLEAN;
BEGIN
  info.DebugInfoSize := 0;
  RETURN TRUE;
END FillDebugInfo;
*)
PROCEDURE FillDebugInfo(VAR info: kt.EXEC_INFO): BOOLEAN;

VAR
  file : rf.ChanId;
  res  : rf.OpenResults;

  PROCEDURE read(VAR buf: ARRAY OF sys.LOC): BOOLEAN;
    VAR x: CARDINAL;
  BEGIN
    x:=SIZE(buf);
    ioc.RawRead(file, sys.ADR(buf), x, x);
    RETURN x=SIZE(buf);
  END read;

VAR
  entry: RECORD
           name: ARRAY [0..3] OF CHAR;
           offs: CARDINAL;
         END;
  fp   : xfp.FilePos;
  ps   : CARDINAL;
BEGIN
  printf ("*** FillDebugInfo: '%s', info.hFile: %d\n\n", info.full_name, info.hFile);

--  IF info.hFile = 0 THEN
--    rf.OpenOld(file, info.full_name, rf.raw, res);
--  END;

  pio.MakeChannel(file, sys.ADDRESS(info.hFile), info.full_name, rf.raw + rf.read, res);
  IF res # rf.opened THEN RETURN FALSE END;
  fp := rf.EndPos(file);


  IF NOT xfp.PosToCard(ps, fp) THEN RETURN FALSE END;
  IF ps <= 8 THEN RETURN FALSE END;
  xfp.CardToPos(fp, ps-8);
  rf.SetPos(file, fp);

  IF NOT read(entry) THEN RETURN FALSE END;

  IF entry.offs < ps THEN
    COPY(entry.name, info.DebugInfoTag);
    info.DebugInfoStart := ps - entry.offs;
    info.DebugInfoSize := entry.offs;
   
    xfp.CardToPos(fp, info.DebugInfoStart);
    rf.SetPos(file, fp);
    IF NOT read(entry) OR (entry.name[0] # info.DebugInfoTag[0])
       OR (entry.name[1] # info.DebugInfoTag[1])
       OR (entry.name[2] # info.DebugInfoTag[2])
       OR (entry.name[3] # info.DebugInfoTag[3])
    THEN
      info.DebugInfoTag  := '';
      info.DebugInfoSize := 0;
    END;
  ELSE
    info.DebugInfoSize := 0;
  END;

  -- until we fix linker 
--  info.DebugInfoSize := 0;

--  rf.Close(file);
  RETURN TRUE;
END FillDebugInfo;



PROCEDURE BuildObjects(name: lnx.PCHAR; base: CARDINAL; VAR info: kt.EXEC_INFO): BOOLEAN;
TYPE
  PBUFFER_TYPE = POINTER TO ARRAY OF CHAR;
VAR
  bfd: lnx.BFD;
  begin, end, flags: CARDINAL;
  i, j, number, phdr_size: INTEGER;

  code_object_initialized: BOOLEAN;
  buffer: PBUFFER_TYPE;

  module_name: kt.PROGRAM_NAME;

  loadable: BOOLEAN;
BEGIN
  code_object_initialized := FALSE;
  bfd := lnx.my_bfd_open (name^);
  IF bfd = NIL THEN
    RETURN FALSE;
  END;

  MyStrCopy(name, module_name);


  IF NOT lnx.my_bfd_check_format(bfd) THEN
    lnx.my_bfd_close(bfd);

    number := 1;
--    printf ("*** BuildObjects (bad bfd): '%s', %d\n", module_name, number);

    info.StartupEntry := 040000b30H; -- durty patch :(

    NEW(info.Objects, number);
    info.N_Objects := number;
    info.Code_Object := 0;

    WITH info.Objects^[0] DO
      Attributes := kt.ATTRIBS{kt.bit_32, kt.read, kt.write, kt.execute};
      Begin := 0;
      RelocationBase := Begin;
      End := 0FFFF0000H;
    END;
    RETURN TRUE;

--    RETURN FALSE;
  END;


  phdr_size := lnx.my_bfd_get_elf_phdr_upper_bound (bfd);

  IF phdr_size <= 0 THEN
    lnx.my_bfd_close(bfd);
    RETURN FALSE;
  END;

  NEW (buffer, phdr_size);
  IF buffer=NIL THEN
    lnx.my_bfd_close(bfd);
    RETURN FALSE;
  END;

  number := lnx.my_bfd_get_elf_phdrs (bfd, buffer^);

  IF number = 0 THEN
    DISPOSE (buffer);
    lnx.my_bfd_close(bfd);
    RETURN FALSE;
  END;

  info.StartupEntry := lnx.my_bfd_get_startup_entry (bfd) + base;
--  printf ("*** BuildObjects: '%s', info.StartupEntry: 0x%x, base: 0x%x\n", 
--          module_name, info.StartupEntry, base);

  NEW(info.Objects, number);
  info.N_Objects := number;

--  printf ("*** BuildObjects: '%s', %d\n", module_name, number);

  FOR i := 0 TO number-1 DO
    WITH info.Objects^[i] DO
      loadable := lnx.my_bfd_get_segment_info (buffer^, i, base, begin, end, flags);

      Attributes := kt.ATTRIBS{kt.bit_32};
      IF (flags & lnx.PF_R) # 0 THEN
        INCL (Attributes, kt.read);
      END;
      IF (flags & lnx.PF_W) # 0 THEN
        INCL (Attributes, kt.write);
      END;
      IF (flags & lnx.PF_X) # 0 THEN
        INCL (Attributes, kt.execute);
        IF NOT code_object_initialized THEN
          info.Code_Object := i;
          code_object_initialized := TRUE;
        END;
      END;
      Begin := begin;
      RelocationBase := begin - base;
      End := end;

--      printf ("*** segment: begin=0x%$8x, idx=%d, loadable=%d\n", Begin, i, loadable);
    END;
  END;
  DISPOSE (buffer);
  lnx.my_bfd_close(bfd);
  RETURN TRUE;
END BuildObjects;


PROCEDURE ProcessEvents(add_step, do_singlestep: BOOLEAN);
VAR
  event   : eve.EVENT;
--  i, rc   : CARDINAL;
  inx     : kth.THREAD_INX;

--  id_process: lnx.PID;
--  id_thread : lnx.TID;

  buf: xs.String;
BEGIN
  LOOP
(*
    id_process := Id_Process;
    id_thread  := Id_Thread;
    REPEAT
      IF NOT ContinueDebugEvent(id_process, id_thread, do_singlestep, cont_flag) THEN
        ASSERT(FALSE);
      END;
      cont_flag := TRUE;
      IF NOT opt.RemoteMode THEN
        IF NOT WaitForDebugEvent(di, opt.SSS_Delay) THEN
          dbg.SwitchToDebuggee;
          sys.EVAL( WaitForDebugEvent(di, INFINITE) );
        END;
      ELSE
        sys.EVAL( WaitForDebugEvent(di, INFINITE) );
      END;

<* IF DEFINED(xd_debug) & xd_debug THEN *>
    PrintNotification;
<* END *>

      id_process := di.pid;
      id_thread  := di.tid;
    UNTIL Id_Process = id_process;
*)

    IF NOT lnx.continue_debug_event(H_Thread, do_singlestep, cont_flag) THEN
      ASSERT(FALSE)
    END;
    sys.EVAL( lnx.wait_for_debug_event(di) );
(*
    IF FindThreadByHandle (di.thread_info, inx) THEN
--      H_Thread := GetThreadHandle (inx);
      H_Thread := di.thread_info;
    ELSE
--      ASSERT(di.type = lnx.DE_THREAD_CREATED, CARDINAL(di.type));
    END;
*)
    H_Thread := di.thread_info;
    -- ensure we know about this thread
    IF NOT FindThreadByHandle (H_Thread, inx) THEN
      ASSERT ((di.type = lnx.DE_THREAD_EXITED) OR 
              (di.type = lnx.DE_ASK_AGAIN), CARDINAL(di.type))
    END;

    
    WITH di DO
      CASE type OF

      | lnx.DE_THREAD_CREATED:
printf ("*** ProcessEvents: DE_THREAD_CREATED\n");
        WITH thread DO
          WITH event DO
            IP := GetIP ();
            Event := eve.ThreadCreated;
            Thread := AddThread (new_thread_info);
          END;
        END;
        IF NOT eve.AddEvent(event) THEN
          ASSERT(FALSE)
        END;
--        kmem.SynchronizeTraceForAllThreads ();

      | lnx.DE_THREAD_EXITED:
printf ("*** ProcessEvents: DE_THREAD_EXITED\n");
        WITH event DO
          IP := 0;--GetIP ();
          Event := eve.ThreadCreated;
--          Event := eve.ThreadDestroyed;
          IF NOT FindThreadByHandle (thread.new_thread_info, Thread) THEN
            ASSERT (FALSE)
          END;
        END;
        RemoveThread (thread.new_thread_info);
        IF NOT eve.AddEvent(event) THEN
          ASSERT(FALSE)
        END;

      | lnx.DE_LOAD_SOLIB:
printf ("*** ProcessEvents: DE_LOAD_SOLIB\n");
        WITH event DO
          IP := GetIP ();
          Event := eve.CompCreated;
          Stopable := TRUE;
          Component := kt.EmptyExecInfo;
          Component.Handle := kt.ADDRESS(solib.base);
          Component.hFile := CARDINAL(solib.hfile);
printf ("*** ProcessEvents: hfile: %d\n", solib.hfile);
          IF NOT BuildObjects(solib.name, solib.base, Component) THEN
            ASSERT (FALSE)
          END;
          MyStrCopy(solib.name, Component.full_name);
          fil.ExtractFileName (Component.full_name, Component.short_name);
(*
          IF prg.ReadName (LoadDll.lpImageName, LoadDll.fUnicode, Component.full_name) THEN
            fil.ExtractFileName (Component.full_name, Component.short_name);
            sys.EVAL (prg.RetrieveModuleInfo (LoadDll.hFile, Component, FALSE));
          END;
*)
          sys.EVAL (FillDebugInfo(Component));
          IF solib.is_program # 0 THEN
printf ("*** ProcessEvents: solib: '%s' is a program\n", Component.full_name);
            info := Component;
          ELSE
printf ("*** ProcessEvents: solib: '%s'\n", Component.full_name);
          END;
        END;
        IF NOT eve.AddEvent(event) THEN
          ASSERT(FALSE)
        END;
        RETURN;


      | lnx.DE_UNLOAD_SOLIB:
printf ("*** ProcessEvents: DE_UNLOAD_SOLIB\n");
        WITH event DO
          IP := GetIP ();
          Event := eve.CompDestroyed;
          Handle := kt.ADDRESS(solib.base);
        END;
        IF NOT eve.AddEvent(event) THEN
          ASSERT(FALSE)
        END;
        RETURN;


      | lnx.DE_EXCEPTION:
        WITH exception DO
          CASE type OF
          | lnx.EE_SINGLESTEP:
printf ("*** ProcessEvents: EE_SINGLESTEP\n");

              IF add_step THEN
                WITH event DO
                  IP    := kt.ADDRESS(pc);
                  Event := eve.SingleStep;
                END;
                IF NOT eve.AddEvent(event) THEN
                  ASSERT(FALSE)
                END;
              END;
              RETURN;

          | lnx.EE_BREAKPOINTHIT:
printf ("*** ProcessEvents: EE_BREAKPOINTHIT\n");
--              IF dwFirstChance # 0 THEN
                event.IP            := kt.ADDRESS(pc); --kt.ADDRESS(ExceptionAddress);
                event.Event         := eve.BreakpointHit;
                event.BreakpointInd := 0;
                IF NOT eve.AddEvent(event) THEN
                  ASSERT(FALSE)
                END;
                RETURN;
(*
              ELSE
                event.IP           := kt.ADDRESS(ExceptionAddress);
                event.Event        := eve.Exception;
                event.Exception_ID := eve.ProgramException;
                event.XCPT_INFO_1  := ExceptionCode;
                event.XCPT_INFO_2  := ExceptionAddress;
                event.XCPT_INFO_3  := CARDINAL(1);
                ASSERT(eve.AddEvent(event));
                RETURN;
              END;
*)        | lnx.EE_HIDDEN_EXCEPTION:
printf ("*** ProcessEvents: EE_HIDDEN_EXCEPTION\n");
            cont_flag := FALSE; -- exception to ignore
--            IF (opt.ExceptionOnFirstChance) --OR (dwFirstChance = 0) OR (ExceptionFlags = win.EXCEPTION_NONCONTINUABLE)
--            THEN
              event.IP           := kt.ADDRESS(pc); --kt.ADDRESS(ExceptionAddress);
              event.Event        := eve.Exception;
              event.Exception_ID := eve.ProgramException;
              event.XCPT_INFO_1  := CARDINAL(signal);--ExceptionCode;
              event.XCPT_INFO_2  := CARDINAL(event.IP);--ExceptionAddress;
              event.XCPT_INFO_3  := CARDINAL(1);--dwFirstChance;
              IF NOT eve.AddEvent(event) THEN
                ASSERT(FALSE)
              END;
              RETURN;
          ELSE
printf ("*** ProcessEvents: DE_EXCEPTION\n");
            cont_flag := FALSE; -- exception was not handled
--            IF (opt.ExceptionOnFirstChance) --OR (dwFirstChance = 0) OR (ExceptionFlags = win.EXCEPTION_NONCONTINUABLE)
--            THEN
              event.IP           := kt.ADDRESS(pc); --kt.ADDRESS(ExceptionAddress);
              event.Event        := eve.Exception;
              event.Exception_ID := eve.ProgramException;
              event.XCPT_INFO_1  := CARDINAL(signal);--ExceptionCode;
              event.XCPT_INFO_2  := CARDINAL(event.IP);--ExceptionAddress;
              event.XCPT_INFO_3  := CARDINAL(1);--dwFirstChance;
              IF NOT eve.AddEvent(event) THEN
                ASSERT(FALSE)
              END;
              RETURN;
--            END;
--          END;
          END;
        END;
      | lnx.DE_INITIALIZED:
printf ("*** ProcessEvents: DE_INITIALIZED\n");
        event.IP            := GetIP(); --kt.ADDRESS(ExceptionAddress);
        event.Event         := eve.BreakpointHit;
        event.BreakpointInd := 0;
        IF NOT eve.AddEvent(event) THEN
          ASSERT(FALSE)
        END;
        RETURN;

      | lnx.DE_ASK_AGAIN:
--printf ("*** ProcessEvents: DE_ASK_AGAIN\n");
        -- ask for event once more

      | lnx.DE_EXITPROCESS:
printf ("*** ProcessEvents: DE_EXITPROCESS\n");

        WITH exit_process DO
          WITH event DO
            IP := kt.ADDRESS(pc);
            Event := eve.ThreadCreated;
            IF FindThreadByHandle (H_Thread, Thread) THEN
              RemoveThread (H_Thread);
              IF NOT eve.AddEvent(event) THEN
                ASSERT(FALSE)
              END;
            END;
          END;
          Finished := TRUE;
          event.IP := kt.ADDRESS(pc);
          event.Event := eve.Exception;
          event.Exception_ID:= eve.ProgramException;
          event.XCPT_INFO_1 := CARDINAL(0);
          event.XCPT_INFO_2 := event.IP;
          event.XCPT_INFO_3 := CARDINAL(0);
          event.XCPT_INFO_4 := CARDINAL(exit_code);
          IF NOT eve.AddEvent(event) THEN
            ASSERT(FALSE)
          END;
          RETURN;
        END;
(*
      | win.OUTPUT_DEBUG_STRING_EVENT:
        WITH DebugString DO
          IF nDebugStringLength > 1 THEN
            ASSERT(kmem.Get(CARDINAL(lpDebugStringData), sys.ADR(buf), nDebugStringLength));
            buf[nDebugStringLength-2] := 0C;
            od.AddOutputDebugString (buf);
          END;
        END;
*)
      ELSE
        ASSERT(FALSE, CARDINAL(di.type));
      END;
    END;
  END;
END ProcessEvents;


PROCEDURE Execute (go_mode: GO_MODE);
VAR
  i    : CARDINAL;
  Regs : kt.REGISTER_CACHE;
  flags: BITSET;
  
BEGIN
  IF NOT opt.RemoteMode THEN
    CASE go_mode.mode OF
    | SingleStep:
      printf('DEBUGINFO: Execute: SingleStep\n');
(*
      IF thr.Threads.Count > 0 THEN
        FOR i := 0 TO thr.Threads.Count - 1 DO
          IF thr.Threads.Threads^[i].Handle # H_Thread THEN
            sys.EVAL (thr.SuspendThread (i));
          END;
        END;
      END;
*)
      <* IF DEFINED(xd_debug) & xd_debug THEN *>
        IF opt.Debug(opt.Another) THEN
          printf('\n =============================== \n');
          printf(  ' Execute from IP  = %$8X : Single Step, Add Step = %u\n', Regs.Eip, go_mode.add_step);
          printf(  ' =============================== \n\n');
        END;
      <* END *>

      -- set step mode on
      ProcessEvents(go_mode.add_step, TRUE); -- SINGLESTEP
      
(*
      IF thr.Threads.Count > 0 THEN
        FOR i := 0 TO thr.Threads.Count - 1 DO
          IF thr.Threads.Threads^[i].Handle # H_Thread THEN
            sys.EVAL (thr.ResumeThread (i));
          END;
        END;
      END;
*)
  
    | Go:
      printf('DEBUGINFO: Execute: Go\n');

      <* IF DEFINED(xd_debug) & xd_debug THEN *>
        IF opt.Debug(opt.Another) THEN
          printf('\n =============================== \n');
          printf(  ' Execute from IP = %$8X : Go \n', Regs.Eip);
          printf(  ' =============================== \n\n');
        END;
      <* END *>

      -- set step mode off
      ProcessEvents(FALSE, FALSE);
    END;
  ELSE
    rmt.Execute (go_mode);
  END;
END Execute;



PROCEDURE ParseArguments ();
VAR
  argc, len, i, j: INTEGER;
  ch: CHAR;
  quoted, inside_arg: BOOLEAN;
BEGIN
  len := LENGTH (args_save);
  argc := 0;
  i := 0;
  quoted := FALSE;

  ch := args_save[i];
  WHILE i < len DO
    IF ch = '"' THEN
      quoted := NOT quoted;
      args_save[i] := 0C;
      IF NOT quoted THEN
        INC (argc);
      END;

    ELSIF NOT quoted AND (ch = ' ') THEN
      args_save[i] := 0C;
      IF args_save[i-1] # 0C THEN
        INC (argc);
      END;

    END;

    INC (i);
    ch := args_save[i];
  END;

  NEW (argv, argc+2);
  argv^[0] := sys.ADR (name_save);
  argv^[argc+1] := NIL;

  i := 0;
  j := 1;
  inside_arg := FALSE;
  ch := args_save[i];

  WHILE j <= argc DO
    IF ch # 0C THEN
      IF NOT inside_arg THEN
        argv^[j] := sys.ADR (args_save[i]);
        INC (j);
        inside_arg := TRUE;
      END;

    ELSE
      inside_arg := FALSE;
    END;

    INC (i);
    ch := args_save[i];
  END;

END ParseArguments;


PROCEDURE StartProgram (name-: ARRAY OF CHAR; args-: ARRAY OF CHAR): CARDINAL;
VAR
  event  : eve.EVENT;
  go_mode: GO_MODE;
--  rc     : CARDINAL;
--  tmp    : xs.String;
  main_thread : lnx.THANDLE;
BEGIN
  IF NOT opt.RemoteMode THEN
    IF Loaded THEN
      RETURN msg.ProgramAlredyStarted;
    END;
    info := kt.EmptyExecInfo;
    IF NOT prg.SearchProgram (name, info.full_name) THEN
      RETURN msg.ProgramNotFound;
    END;
    fil.ExtractFileName (info.full_name, info.short_name);

--    COPY(info.full_name, name_save);
    COPY(name, name_save);
    COPY(args, args_save);
    ParseArguments ();

    main_thread := lnx.create_process (name_save, argv^);
    IF main_thread = NIL THEN
      RETURN msg.CannotCreateProcess 
    END;

    H_Thread  := main_thread;
--    H_Process := proc;

    WITH event DO
      IP := GetIP ();
      Event := eve.ThreadCreated;
      Thread := AddThread (main_thread);
    END;

    IF NOT eve.AddEvent(event) THEN
      ASSERT(FALSE)
    END;
--    kmem.SynchronizeTraceForAllThreads ();
(*
    IF NOT lnx.continue_debug_event(H_Thread, FALSE, FALSE) THEN
      ASSERT(FALSE);
    END;
    sys.EVAL( lnx.wait_for_debug_event(di) );
*)
(*
    IF NOT BuildObjects(0, info) THEN
      RETURN msg.InvalidProgramHeader;
    END;
*)
(*
    IF prg.ReadName (lpImageName, fUnicode, tmp) THEN
      COPY (tmp, info.full_name);
      fil.ExtractFileName (tmp, info.short_name);
    END;
*)
--    sys.EVAL (prg.RetrieveModuleInfo (hFile, info, FALSE));
    info.Handle := CARDINAL(H_Thread);
--    info.hFile  := CARDINAL(hFile);


(*
    sys.EVAL (FillDebugInfo(info));
    WITH event DO
      IP := GetIP();
      Event := eve.CompCreated;
      Stopable := TRUE;
      Component := info;
    END;
    ASSERT(eve.AddEvent(event));
*)
    cont_flag := TRUE;

    go_mode.mode := Go;
    LOOP
      Execute(go_mode);
      IF eve.QueryEvent (eve.BreakpointHit) THEN
        LOOP
          eve.GetEvent;
(*
          IF eve.LastEvent.Event = eve.CompCreated THEN
            eve.LastEvent.Stopable := FALSE;
          ELS
*)
          IF eve.LastEvent.Event = eve.BreakpointHit THEN
            EXIT;
          END;
          IF NOT eve.AddEvent(eve.LastEvent) THEN
            ASSERT(FALSE)
          END;
        END;
        EXIT;
      ELSIF eve.QueryEvent (eve.Exception) THEN
        EXIT;
      END;
    END;

    Loaded := TRUE;
    Finished := FALSE;

--    dbg.Find_Debuggee_HWND;

    RETURN 0;
  ELSE
    RETURN rmt.StartProgram (name, args);
  END;
END StartProgram;


PROCEDURE UnloadProgram;
BEGIN
  IF NOT opt.RemoteMode THEN
    IF NOT Finished THEN 
      sys.EVAL( lnx.terminate_process() );
--      H_Process := NIL;
      Finished := TRUE;
    END;
    thr.ClearThreads ();
    Loaded := FALSE;
    od.Clear ();
  ELSE
    rmt.UnloadProgram;
  END;
END UnloadProgram;

PROCEDURE RestartProgram (): BOOLEAN;
VAR
  event  : eve.EVENT;
  go_mode: GO_MODE;
--  rc     : CARDINAL;
--  tmp    : xs.String;
  main_thread : lnx.THANDLE;
BEGIN
  UnloadProgram();

  main_thread := lnx.create_process (name_save, argv^);
  IF main_thread = NIL THEN
    RETURN FALSE; 
  END;

--  H_Process := proc;
--  H_Thread  := lnx.get_main_thread_handle(H_Process);
  H_Thread := main_thread;

  WITH event DO
    IP := GetIP ();
    Event := eve.ThreadCreated;
    Thread := AddThread (H_Thread);
  END;
  IF NOT eve.AddEvent(event) THEN
    ASSERT(FALSE)
  END;
  info.Handle := CARDINAL(H_Thread);

  cont_flag := TRUE;

  go_mode.mode := Go;
  LOOP
    Execute(go_mode);
    IF eve.QueryEvent (eve.BreakpointHit) THEN
      LOOP
        eve.GetEvent;
(*
        IF eve.LastEvent.Event = eve.CompCreated THEN
          eve.LastEvent.Stopable := FALSE;
        ELS
*)
        IF eve.LastEvent.Event = eve.BreakpointHit THEN
          EXIT;
        END;
        IF NOT eve.AddEvent(eve.LastEvent) THEN
          ASSERT(FALSE)
        END;
      END;
      EXIT;
    ELSIF eve.QueryEvent (eve.Exception) THEN
      EXIT;
    END;
  END;

  Loaded := TRUE;
  Finished := FALSE;

  RETURN TRUE;
END RestartProgram;


PROCEDURE GetProgramInfo(VAR inf: kt.EXEC_INFO): BOOLEAN;
BEGIN
  IF NOT opt.RemoteMode THEN
    IF NOT Loaded THEN
      RETURN FALSE;
    END;
(*
    IF NOT FillDebugInfo(info) THEN
      RETURN FALSE;
    END;
*)
    inf := info;
    RETURN TRUE;
  ELSE
    RETURN rmt.GetProgramInfo (inf);
  END;
END GetProgramInfo;



PROCEDURE GetDebugInfo(info: kt.EXEC_INFO; info_addr: sys.ADDRESS): BOOLEAN;
VAR
  file : rf.ChanId;
  res  : rf.OpenResults;
  fp   : xfp.FilePos;
  len  : CARDINAL;
BEGIN
  IF NOT opt.RemoteMode THEN
    IF info.DebugInfoSize = 0 THEN
      RETURN FALSE;
    END;
    IF opt.KernelInRemoteMode THEN
      -- поскольку невозможно смапировать файл на удаленной машине
      pio.MakeChannel(file, sys.ADDRESS(info.hFile), info.full_name, rf.raw + rf.read, res);
      IF res # rf.opened THEN
        RETURN FALSE;
      END;
      xfp.CardToPos(fp, info.DebugInfoStart);
      rf.SetPos(file, fp);
      ioc.RawRead (file, info_addr, info.DebugInfoSize, len);
      RETURN len = info.DebugInfoSize;
    ELSE
      RETURN FALSE;
    END;
  ELSE
    RETURN rmt.GetDebugInfo (info, info_addr);
  END;
END GetDebugInfo;


PROCEDURE ReadExport (full_name-: ARRAY OF CHAR; VAR exp: kt.EXPORTS): BOOLEAN;
BEGIN
  IF opt.RemoteMode THEN
    RETURN rmt.ReadExport (full_name, exp);
  ELSE
--    RETURN FALSE;
    RETURN re.ReadExport (full_name, exp);
  END;
END ReadExport;


PROCEDURE ["C"] CtrlBreakHandler (): BOOLEAN;
BEGIN
  IF ProgramExecuting THEN
  END;
  RETURN TRUE;
END CtrlBreakHandler;


VAR
--  oldBreakHandler: brk.BreakHandler;

BEGIN
  lnx.init_linux_layer ();

  ProgramExecuting := FALSE;
  Finished := TRUE;
  argv := NIL;
--  oldBreakHandler := brk.SetBreakHandler (CtrlBreakHandler);
FINALLY
  UnloadProgram ();
  IF argv # NIL THEN
    DISPOSE (argv);
  END;
END Krn_Prog.