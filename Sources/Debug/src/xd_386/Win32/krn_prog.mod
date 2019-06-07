-- Загрузка программы, отгрузка, рестарт, остановы, исполнение,
-- режимы исполнения, получение времен исполнения.
-- Модуль является системо-зависимым.

IMPLEMENTATION MODULE Krn_Prog;

<* Storage+ *>

IMPORT sys := SYSTEM;
IMPORT io  := InOut;
IMPORT rf  := RndFile;
IMPORT ioc := IOChan;
IMPORT xfp := xFilePos;
IMPORT rio := RawIO;
IMPORT brk := CtrlC;
IMPORT wio := Win32IOChan;
IMPORT env := ProgEnv;
IMPORT  fs := FileSys;

IMPORT win := Windows;

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

FROM Krn_Dbg IMPORT H_Thread, H_Process, Id_Process, Id_Thread;
FROM Printf IMPORT printf;


VAR
  info    : kt.EXEC_INFO;
  Loaded  : BOOLEAN;
  Finished: BOOLEAN;

  Title, Cmd_Line: xs.String;
  startup_info   : win.STARTUPINFO;
  security_attrs : win.SECURITY_ATTRIBUTES;
  creation_flags : win.CREATE_SET;


  pi: win.PROCESS_INFORMATION;
  di: win.DEBUG_EVENT;


<* IF DEFINED(xd_debug) & xd_debug THEN *>

PROCEDURE PrintNotification;
VAR
  buf: xs.String;
BEGIN
  IF NOT opt.Debug(opt.Another) THEN RETURN; END;
  io.WriteString('----------------------------------------'); io.WriteLn;
  WITH di DO
    CASE dwDebugEventCode OF
    | win.OUTPUT_DEBUG_STRING_EVENT:
      WITH DebugString DO
        IF nDebugStringLength > 0 THEN
          ASSERT(kmem.Get(CARDINAL(lpDebugStringData), sys.ADR(buf), nDebugStringLength));
          io.WriteString('Output String = '); io.WriteString(buf); io.WriteLn;
        END;
      END;

    | win.CREATE_PROCESS_DEBUG_EVENT:
      io.WriteString('Thread Created'); io.WriteLn;
      WITH CreateProcessInfo DO
        io.WriteString('  hFile        = '); io.WriteHex(CARDINAL(hFile),    0); io.WriteLn;
        io.WriteString('  hProcess     = '); io.WriteHex(CARDINAL(hProcess), 0); io.WriteLn;
        io.WriteString('  hThread      = '); io.WriteHex(CARDINAL(hThread),  0); io.WriteLn;
        io.WriteString('  pBaseOfImage = '); io.WriteHex(CARDINAL(lpBaseOfImage),0); io.WriteLn;
        io.WriteString('  DebugInfoOffset = '); io.WriteHex(dwDebugInfoFileOffset,0); io.WriteLn;
        io.WriteString('  DebugInfoSize   = '); io.WriteCard(nDebugInfoSize,0); io.WriteLn;
        io.WriteString('  StartAddress    = '); io.WriteHex(CARDINAL(lpStartAddress),0); io.WriteLn;
      END;
    | win.CREATE_THREAD_DEBUG_EVENT:
      io.WriteString('Thread Created'); io.WriteLn;
      WITH CreateThread DO
        io.WriteString('  hThread      = '); io.WriteHex(CARDINAL(hThread),  0); io.WriteLn;
        io.WriteString('  StartAddress    = '); io.WriteHex(CARDINAL(lpStartAddress),0); io.WriteLn;
      END;
     | win.EXCEPTION_DEBUG_EVENT:
       io.WriteString('Exception ');
       WITH Exception DO
         WITH ExceptionRecord DO
           CASE ExceptionCode OF
           | win.EXCEPTION_SINGLE_STEP:
             io.WriteString('SingleStep');

           | win.EXCEPTION_BREAKPOINT:
             io.WriteString('Breakpoint ');
             io.WriteHex(CARDINAL(ExceptionAddress), 0);
           ELSE
             io.WriteString('Unknown '); io.WriteHex(ExceptionCode, 0); io.WriteLn;
             io.WriteString('Address = '); io.WriteHex(CARDINAL(ExceptionAddress), 0); io.WriteLn;
             io.WriteString('FirstChance = '); io.WriteCard(dwFirstChance, 0); io.WriteLn;
             io.WriteString('Flags = '); io.WriteCard(ExceptionFlags, 0); io.WriteLn;


           END;
           io.WriteLn;
         END;
       END;

     | win.LOAD_DLL_DEBUG_EVENT:
       io.WriteString('Dll loaded'); io.WriteLn;
       IF prg.ReadName (LoadDll.lpImageName, LoadDll.fUnicode, buf) THEN
         io.WriteString(buf); io.WriteLn;
       END;

     | win.UNLOAD_DLL_DEBUG_EVENT:
       io.WriteString('Dll unloaded'); io.WriteLn;
       IF prg.ReadName (LoadDll.lpImageName, LoadDll.fUnicode, buf) THEN
         io.WriteString(buf); io.WriteLn;
       END;

    ELSE
      io.WriteString('Unknown Yet ');
      io.WriteInt(INTEGER(dwDebugEventCode), 0); io.WriteLn;
    END;
  END;
END PrintNotification;

<* END *>



(* Получить текущее значение IP *)
PROCEDURE GetIP (): kt.ADDRESS;
VAR
  RegisterCache: kt.REGISTER_CACHE;
BEGIN
  IF kmem.GetRegisterCache (RegisterCache) THEN
    RETURN RegisterCache.Eip + RegisterCache.SegCs;
  ELSE
    RETURN 0;
  END;
END GetIP;





PROCEDURE AddThread (Id: CARDINAL; handle: win.HANDLE): kth.THREAD_INX;
VAR
  inx: kth.THREAD_INX;
BEGIN
  inx := thr.NewThread ();
  thr.Threads.Threads^[inx].Handle := handle;
  thr.Threads.Threads^[inx].ID := Id;
  RETURN inx;
END AddThread;


PROCEDURE FindThreadById (Id: win.DWORD; VAR inx: kth.THREAD_INX): BOOLEAN;
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


PROCEDURE GetThreadHandle (inx: kth.THREAD_INX): win.HANDLE;
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
  fH   : win.HANDLE;

BEGIN
--  rf.OpenOld(file, info.full_name, rf.raw, res);
  ASSERT(win.DuplicateHandle(win.GetCurrentProcess(),
    win.HANDLE(info.hFile),
    win.GetCurrentProcess(),
    fH, win.ACCESS_MASK{}, FALSE, win.DUPLICATE_SAME_ACCESS));

  wio.MakeChannel(file, fH, info.full_name, rf.raw + rf.read, res);
  IF res # rf.opened THEN
    win.CloseHandle(fH);
    RETURN FALSE;
  END;
  fp := rf.EndPos(file);


  IF NOT xfp.PosToCard(ps, fp) THEN
    rf.Close(file);
    RETURN FALSE;
  END;
  IF ps <= 8 THEN
    rf.Close(file);
    RETURN FALSE;
  END;
  xfp.CardToPos(fp, ps-8);
  rf.SetPos(file, fp);

  IF NOT read(entry) THEN
    rf.Close(file);
    RETURN FALSE;
  END;

  IF entry.offs < ps THEN
    COPY(entry.name, info.DebugInfoTag);
    info.DebugInfoStart := ps - entry.offs;
    info.DebugInfoSize := entry.offs;
   
    xfp.CardToPos(fp, info.DebugInfoStart);
    rf.SetPos(file, fp);
    IF NOT read(entry) OR (entry.name # info.DebugInfoTag) THEN
      info.DebugInfoTag  := '';
      info.DebugInfoSize := 0;
    END;
  ELSE
    info.DebugInfoSize := 0;
  END;
  
  rf.Close(file);
  RETURN TRUE;
END FillDebugInfo;

PROCEDURE BuildObjects(base: kt.ADDRESS; VAR info: kt.EXEC_INFO): BOOLEAN;
VAR
  CodeBegin: kt.ADDRESS;
  pos, i   : CARDINAL;
  header   : win.IMAGE_NT_HEADERS;
  Section   : win.IMAGE_SECTION_HEADER;

BEGIN  
  CodeBegin := 0;
  pos := kt.ADDRESS(base);
  IF NOT kmem.Get(pos, sys.ADR(CodeBegin), 2) THEN RETURN FALSE; END;
  IF CodeBegin # win.IMAGE_DOS_SIGNATURE THEN RETURN FALSE; END;
  INC(pos, SIZE(win.IMAGE_DOS_HEADER));
  IF NOT kmem.Get(pos-4, sys.ADR(CodeBegin), 4) THEN RETURN FALSE; END; 
  pos := kt.ADDRESS(base) + CodeBegin;
  IF NOT kmem.Get(pos, sys.ADR(header), SIZE(header)) THEN RETURN FALSE; END;
  INC(pos, SIZE(header));
  CodeBegin := kt.ADDRESS(base) + header.OptionalHeader.BaseOfCode;
  IF header.Signature # win.IMAGE_NT_SIGNATURE THEN RETURN FALSE; END;

  info.StartupEntry := base + header.OptionalHeader.AddressOfEntryPoint;

  NEW(info.Objects, header.FileHeader.NumberOfSections);
  info.N_Objects := header.FileHeader.NumberOfSections;
  
  FOR i := 1 TO header.FileHeader.NumberOfSections DO
    IF NOT kmem.Get(pos, sys.ADR(Section), SIZE(Section)) THEN RETURN FALSE; END; 
    INC(pos, SIZE(Section));
    WITH info.Objects^[i-1] DO
      Begin := Section.VirtualAddress + base;
      IF Section.VirtualSize = 0 THEN
        End := Begin + Section.SizeOfRawData - 1;
      ELSE
        End := Begin + Section.VirtualSize - 1;
      END;
      IF Begin = CodeBegin THEN
        info.Code_Object := i - 1;
      END;
      Attributes := kt.ATTRIBS{};
      
      IF BITSET(win.IMAGE_SCN_MEM_EXECUTE)*BITSET(Section.Characteristics) # {} THEN
        INCL(Attributes, kt.execute);
      END;
      IF BITSET(win.IMAGE_SCN_MEM_READ)*BITSET(Section.Characteristics) # {} THEN
        INCL(Attributes, kt.read);
      END;
      IF BITSET(win.IMAGE_SCN_MEM_WRITE)*BITSET(Section.Characteristics) # {} THEN
        INCL(Attributes, kt.write);
      END;
      IF BITSET(win.IMAGE_SCN_MEM_16BIT)*BITSET(Section.Characteristics) = {} THEN
        INCL(Attributes, kt.bit_32);
      END;
    END;
  END;
  RETURN TRUE;
END BuildObjects;


PROCEDURE ["C"] WaitForDebugEvent (VAR di: win.DEBUG_EVENT; delay: CARDINAL): BOOLEAN;
BEGIN
  RETURN win.WaitForDebugEvent(di, delay);
(*
  IF delay # win.INFINITE THEN
    RETURN win.WaitForDebugEvent(di, delay);
  ELSE
    LOOP
      IF win.WaitForDebugEvent(di, 1000) THEN RETURN TRUE; END;
    END;
  END;
*)
END WaitForDebugEvent;


VAR
  cont_flag: CARDINAL;

PROCEDURE ProcessEvents(add_step: BOOLEAN);
VAR
  event   : eve.EVENT;
  i, rc   : CARDINAL;
  DR6, DR7: BITSET;
  Context : win.CONTEXT;
  inx     : kth.THREAD_INX;

  id_process: win.DWORD;
  id_thread : win.DWORD;

  buf: xs.String;
BEGIN
  LOOP
    id_process := Id_Process;
    id_thread  := Id_Thread;
    REPEAT
--      IF NOT WasUserBreak THEN
        IF NOT win.ContinueDebugEvent(id_process, id_thread, cont_flag) THEN
          ASSERT(FALSE, win.GetLastError());
        END;
--      ELSE
--        WasUserBreak := FALSE;
--      END;
      cont_flag := win.DBG_CONTINUE;
      IF NOT opt.RemoteMode THEN
        IF NOT WaitForDebugEvent(di, opt.SSS_Delay) THEN
          dbg.SwitchToDebuggee;
          WaitForDebugEvent(di, win.INFINITE);
        END;
      ELSE
        WaitForDebugEvent(di, win.INFINITE);
      END;

<* IF DEFINED(xd_debug) & xd_debug THEN *>
    PrintNotification;
<* END *>

      id_process := di.dwProcessId;
      id_thread  := di.dwThreadId;
    UNTIL Id_Process = id_process;

    IF FindThreadById (di.dwThreadId, inx) THEN
      H_Thread := GetThreadHandle (inx);
    ELSE
      ASSERT(di.dwDebugEventCode = win.CREATE_THREAD_DEBUG_EVENT, CARDINAL(di.dwDebugEventCode));
    END;
    Id_Thread := di.dwThreadId;

    WITH di DO
      CASE dwDebugEventCode OF
      | win.CREATE_THREAD_DEBUG_EVENT:
        WITH CreateThread DO
          WITH event DO
            IP := GetIP ();
            Event := eve.ThreadCreated;
            Thread := AddThread (di.dwThreadId, hThread);
          END;
        END;
        ASSERT(eve.AddEvent(event));
        kmem.SynchronizeTraceForAllThreads ();

      | win.LOAD_DLL_DEBUG_EVENT:
        WITH event DO
          IP := GetIP ();
          Event := eve.CompCreated;
          Stopable := TRUE;
          Component := kt.EmptyExecInfo;
          Component.Handle := kt.ADDRESS(LoadDll.lpBaseOfDll);
          Component.hFile := CARDINAL(LoadDll.hFile);
          ASSERT(BuildObjects(CARDINAL(LoadDll.lpBaseOfDll), Component));
          IF prg.ReadName (LoadDll.lpImageName, LoadDll.fUnicode, Component.full_name) THEN
            fil.ExtractFileName (Component.full_name, Component.short_name);
            IF LoadDll.hFile = NIL THEN
              LoadDll.hFile := win.CreateFileA (Component.full_name,
                                                win.GENERIC_READ,
                                                win.FILE_SHARE_READ,
                                                NIL,
                                                win.OPEN_EXISTING,
                                                win.FILE_BIT_SET{},
                                                NIL);
              ASSERT (LoadDll.hFile # win.INVALID_HANDLE_VALUE);
              Component.hFile := CARDINAL(LoadDll.hFile);
            END;
            sys.EVAL (prg.RetrieveModuleInfo (H_Process, Component, FALSE));
          ELSE
            rc := prg.RetrieveModuleInfo (H_Process, Component, TRUE);
            IF rc # 0 THEN
              COPY ("_Windows_error_restart_Windows_", Component.short_name);
              COPY ("_Windows_error_restart_Windows_", Component.full_name);
            END;
          END;
          sys.EVAL (FillDebugInfo(Component));
        END;
        ASSERT(eve.AddEvent(event));
        RETURN;

      | win.UNLOAD_DLL_DEBUG_EVENT:
        WITH event DO
          IP := GetIP ();
          Event := eve.CompDestroyed;
          Handle := kt.ADDRESS(UnloadDll.lpBaseOfDll);
        END;
        ASSERT(eve.AddEvent(event));
        RETURN;

      | win.EXCEPTION_DEBUG_EVENT:
        WITH Exception DO
          WITH ExceptionRecord DO
            CASE ExceptionCode OF
            | win.EXCEPTION_SINGLE_STEP:
              Context.ContextFlags := win.CONTEXT_DEBUG_REGISTERS + win.CONTEXT_CONTROL;
              IF NOT win.GetThreadContext(H_Thread, Context) THEN
                WITH event DO
                  IP := GetIP ();
                  Event  := eve.InternalError;
                  ErrorNo := win.GetLastError();
                END;
                ASSERT(eve.AddEvent(event));
                RETURN
              END;
              DR6 := BITSET(Context.Dr6);
              DR7 := BITSET(Context.Dr7);

              FOR i := 0 TO 3 DO
                IF (i IN DR6) AND (i*2 IN DR7) THEN
                  event.IP             := Context.Eip;
                  event.MemAccess_Ind  := i+1;
                  event.Event          := eve.MemoryAccess;
                  ASSERT(eve.AddEvent(event));
                END;
              END;

              IF add_step THEN
                WITH event DO
                  IP    := Context.Eip;
                  Event := eve.SingleStep;
                END;
                ASSERT(eve.AddEvent(event));
              END;
              RETURN;

            | win.EXCEPTION_BREAKPOINT:
              IF dwFirstChance # 0 THEN
                event.IP            := kt.ADDRESS(ExceptionAddress);
                event.Event         := eve.BreakpointHit;
                event.BreakpointInd := 0;
                ASSERT(eve.AddEvent(event));
                RETURN;
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
            ELSE
              cont_flag := win.DBG_EXCEPTION_NOT_HANDLED;
              IF (opt.ExceptionOnFirstChance) OR (dwFirstChance # 1)
                -- По последним данным, вроде бы нет необходимости 
                -- проверять эти флаги, так как на следующий раз выставится
                -- dwFirstChance = 1, мы все равно остановим программу
                -- OR (ExceptionFlags = win.EXCEPTION_NONCONTINUABLE)
              THEN
                event.IP           := kt.ADDRESS(ExceptionAddress);
                event.Event        := eve.Exception;
                event.Exception_ID := eve.ProgramException;
                event.XCPT_INFO_1  := ExceptionCode;
                event.XCPT_INFO_2  := ExceptionAddress;
                event.XCPT_INFO_3  := dwFirstChance;
                ASSERT(eve.AddEvent(event));
                RETURN;
              END;
            END;
          END;
        END;

      | win.EXIT_THREAD_DEBUG_EVENT:
        WITH event DO
          IP := GetIP ();
          Event := eve.ThreadCreated;
          ASSERT (FindThreadById (di.dwThreadId, Thread));
        END;
        RemoveThread (di.dwThreadId);
        ASSERT(eve.AddEvent(event));

      | win.EXIT_PROCESS_DEBUG_EVENT:
        Finished := TRUE;
        event.IP := GetIP ();
        event.Event := eve.Exception;
        event.Exception_ID:= eve.ProgramException;
        event.XCPT_INFO_1 := CARDINAL(0);
        event.XCPT_INFO_2 := event.IP;
        event.XCPT_INFO_3 := CARDINAL(0);
        event.XCPT_INFO_4 := CARDINAL(ExitProcess.dwExitCode);
        ASSERT(eve.AddEvent(event));
        RETURN;

      | win.RIP_EVENT:
        printf('RIP-debugging event (system debugging error).\n');
        WITH RipInfo DO
          printf('Error that caused RIP debug event = %u at address = 0x%$8X\n', dwError, GetIP ());
          IF CARDINAL(dwType) # 0 THEN
            printf('Additional information about the type of error:\n');
            CASE win.SLE_ENUM(dwType) OF
            | win.SLE_ERROR:
              printf('\tInvalid data was passed to the function that failed.\n');
              printf('\tThis caused the application to fail.\n');
            | win.SLE_MINORERROR:
              printf('\tInvalid data was passed to the function, but the\n');
              printf('\terror probably will not cause the application to fail.\n');
            | win.SLE_WARNING:
                printf('\tPotentially invalid data was passed to the function,\n');
              printf('\tbut the function completed processing.\n');
            ELSE
              printf('\tInvalid data for additional information.\n');
            END;
          END;
        END;
        printf('XD halted.\n');
        HALT (2);

      | win.OUTPUT_DEBUG_STRING_EVENT:
        WITH DebugString DO
          IF nDebugStringLength > 1 THEN
            ASSERT(kmem.Get(CARDINAL(lpDebugStringData), sys.ADR(buf), nDebugStringLength));
            buf[nDebugStringLength-2] := 0C;
            od.AddOutputDebugString (buf);
          END;
        END;

      ELSE
        ASSERT(FALSE, CARDINAL(di.dwDebugEventCode));
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
      IF thr.Threads.Count > 0 THEN
        FOR i := 0 TO thr.Threads.Count - 1 DO
          IF thr.Threads.Threads^[i].Handle # H_Thread THEN
            sys.EVAL (thr.SuspendThread (i));
          END;
        END;
      END;

      -- set step mode on
      ASSERT(kmem.GetRegisterCache(Regs));
      flags := BITSET(Regs.EFlags);
      INCL(flags, 16);
      INCL(flags, 8);
      Regs.EFlags := CARDINAL(flags);
      ASSERT(kmem.PutRegisterCache(Regs));

      <* IF DEFINED(xd_debug) & xd_debug THEN *>
        IF opt.Debug(opt.Another) THEN
          printf('\n =============================== \n');
          printf(  ' Execute from IP  = %$8X : Single Step, Add Step = %u\n', Regs.Eip, go_mode.add_step);
          printf(  ' =============================== \n\n');
        END;
      <* END *>

      ProcessEvents(go_mode.add_step);
      
      IF thr.Threads.Count > 0 THEN
        FOR i := 0 TO thr.Threads.Count - 1 DO
          IF thr.Threads.Threads^[i].Handle # H_Thread THEN
            sys.EVAL (thr.ResumeThread (i));
          END;
        END;
      END;
  
    | Go:
      <* IF DEFINED(xd_debug) & xd_debug THEN *>
        IF opt.Debug(opt.Another) THEN
          printf('\n =============================== \n');
          printf(  ' Execute from IP = %$8X : Go \n', Regs.Eip);
          printf(  ' =============================== \n\n');
        END;
      <* END *>

      -- set step mode off
      ASSERT(kmem.GetRegisterCache(Regs));
      flags := BITSET(Regs.EFlags);
      INCL(flags, 16);
      EXCL(flags, 8);
      Regs.EFlags := CARDINAL(flags);
      ASSERT(kmem.PutRegisterCache(Regs));
  
      ProcessEvents(FALSE);
    END;
  ELSE
    rmt.Execute (go_mode);
  END;
END Execute;





PROCEDURE StartProgram (name: ARRAY OF CHAR; args-: ARRAY OF CHAR): CARDINAL;
VAR
  event  : eve.EVENT;
  go_mode: GO_MODE;
  tmp    : xs.String;
BEGIN
  IF NOT opt.RemoteMode THEN
    IF Loaded THEN
      RETURN msg.ProgramAlredyStarted;
    END;
    fil.AddExtension(name, kt.prg_file_ext);
    info := kt.EmptyExecInfo;
    IF NOT prg.SearchProgram (name, info.full_name) THEN
      RETURN msg.ProgramNotFound;
    END;
    fil.ExtractFileName (info.full_name, info.short_name);
    Title := 'The debuggee: ';
    xs.Append(info.full_name, Title);
    xs.Append(' ', Title);
    xs.Append(args, Title);

    security_attrs := win.SECURITY_ATTRIBUTES{ SIZE(win.SECURITY_ATTRIBUTES), NIL, TRUE };

    WITH startup_info DO
      cb              := SIZE(startup_info);
      lpReserved      := NIL;
      lpDesktop       := NIL;
      lpTitle         := sys.ADR(Title);
      dwFlags         := win.STARTF_SET{};
      cbReserved2     := 0;
      lpReserved2     := NIL;
      -- здесь можно подставить файлы, которые можно будет задать
      -- через параметры. можно сделать еще в окне Load задание
      -- трех имен файлов
     <* IF DEFINED(xd_debug) AND xd_debug THEN *>
      -- dwFlags         := win.STARTF_USESTDHANDLES;
      -- hStdOutput      := win.GetStdHandle (win.STD_OUTPUT_HANDLE);
      -- hStdError       := win.GetStdHandle (win.STD_ERROR_HANDLE);
     <* END *>
    END;
  
    creation_flags := win.DEBUG_PROCESS+win.DEBUG_ONLY_THIS_PROCESS+win.CREATE_NEW_CONSOLE+win.CREATE_NEW_PROCESS_GROUP+win.NORMAL_PRIORITY_CLASS;

    COPY(info.full_name, Cmd_Line);
    xs.Append(' ', Cmd_Line);
    xs.Append(args, Cmd_Line);

    IF NOT win.CreateProcess ( NIL                     -- address of module name
                             , sys.ADR(Cmd_Line)       -- address of command line
                             , NIL -- sys.ADR(security_attrs) -- address of process security attributes
                             , NIL -- sys.ADR(security_attrs) -- address of thread security attributes
                             , TRUE                    -- new process inherits handles
                             , creation_flags          -- creation flags
                             , NIL                     -- address of new environment block
                             , NIL                     -- address of current directory name
                             , startup_info            -- address of STARTUPINFO
                             , pi)                     -- address of PROCESS_INFORMATION
    THEN
      RETURN msg.CannotCreateProcess 
    END;
    Id_Process := pi.dwProcessId;
    Id_Thread  := pi.dwThreadId;
  
    win.ContinueDebugEvent(Id_Process, Id_Thread, win.DBG_CONTINUE);
  
    WaitForDebugEvent(di , win.INFINITE);
    WITH di DO
      ASSERT(dwDebugEventCode = win.CREATE_PROCESS_DEBUG_EVENT);
      WITH CreateProcessInfo DO
        H_Thread  := hThread;
        H_Process := hProcess;
        
        WITH event DO
          IP := GetIP ();
          Event := eve.ThreadCreated;
          Thread := AddThread (Id_Thread, hThread);
        END;
        ASSERT(eve.AddEvent(event));
        kmem.SynchronizeTraceForAllThreads ();

        IF NOT BuildObjects(CARDINAL(lpBaseOfImage), info) THEN
          RETURN msg.InvalidProgramHeader;
        END;
        IF prg.ReadName (lpImageName, fUnicode, tmp) THEN
          COPY (tmp, info.full_name);
          fil.ExtractFileName (tmp, info.short_name);
        END;
        info.Handle := CARDINAL(lpBaseOfImage);
        info.hFile  := CARDINAL(hFile);
        sys.EVAL (prg.RetrieveModuleInfo (H_Process, info, FALSE));
      END;
    END;

    sys.EVAL (FillDebugInfo(info));
    WITH event DO
      IP := GetIP();
      Event := eve.CompCreated;
      Stopable := TRUE;
      Component := info;
    END;
    ASSERT(eve.AddEvent(event));

    cont_flag := win.DBG_CONTINUE;
   
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
          ASSERT(eve.AddEvent(eve.LastEvent));
        END;
        EXIT;
      ELSIF eve.QueryEvent (eve.Exception) THEN
        EXIT;
      END;
    END;
   
    Loaded := TRUE;
    Finished := FALSE;

    dbg.Find_Debuggee_HWND;

    RETURN 0;
  ELSE
    RETURN rmt.StartProgram (name, args);
  END;
END StartProgram;


PROCEDURE UnloadProgram;
BEGIN
  IF NOT opt.RemoteMode THEN
    IF NOT Finished THEN 
      win.TerminateProcess(H_Process, 0);
      REPEAT
        ASSERT(win.ContinueDebugEvent(di.dwProcessId, di.dwThreadId, win.DBG_CONTINUE));
        WaitForDebugEvent(di , win.INFINITE);
      UNTIL di.dwDebugEventCode = win.EXIT_PROCESS_DEBUG_EVENT;
      Finished := TRUE;
    END;
    -- А сейчас надо убить не только сессию, но и процесс
    -- иначе отлаживаемый процесс остается жить
    win.ContinueDebugEvent(di.dwProcessId, di.dwThreadId, win.DBG_CONTINUE);
    -- win.CloseHandle(H_Process);
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
BEGIN
  IF NOT opt.RemoteMode THEN
    IF NOT win.CreateProcess(NIL, sys.ADR(Cmd_Line), NIL, NIL, TRUE,
                      win.DEBUG_PROCESS+win.DEBUG_ONLY_THIS_PROCESS+win.CREATE_NEW_CONSOLE+
                      win.CREATE_NEW_PROCESS_GROUP+win.NORMAL_PRIORITY_CLASS, NIL, NIL,
                      startup_info, pi)  THEN RETURN FALSE END;
  
    Id_Process    := pi.dwProcessId;
    Id_Thread     := pi.dwThreadId;
  
    cont_flag     := win.DBG_CONTINUE;
    Finished      := FALSE;
  

    win.ContinueDebugEvent(Id_Process, Id_Thread, win.DBG_CONTINUE);
    
    LOOP
      WaitForDebugEvent(di , win.INFINITE);

<* IF DEFINED(xd_debug) & xd_debug THEN *>
      PrintNotification;
<* END *>

      WITH di DO
        CASE dwDebugEventCode OF
        | win.CREATE_PROCESS_DEBUG_EVENT:
          WITH CreateProcessInfo DO
            H_Thread  := hThread;
            H_Process := hProcess;
            WITH event DO
              IP := GetIP ();
              Event := eve.ThreadCreated;
              Thread := AddThread (Id_Thread, hThread);
            END;
            ASSERT(eve.AddEvent(event));
            kmem.SynchronizeTraceForAllThreads ();
          END;
          EXIT;
        ELSE
        END;
      END;
      ASSERT(win.ContinueDebugEvent(di.dwProcessId, di.dwThreadId, win.DBG_CONTINUE));
    END;
    
    WITH event DO
      IP := GetIP();
      Event := eve.CompCreated;
      Stopable := TRUE;
      Component := info;
    END;
    ASSERT(eve.AddEvent(event));

    cont_flag := win.DBG_CONTINUE;

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
          ASSERT(eve.AddEvent(eve.LastEvent));
        END;
        EXIT;
      ELSIF eve.QueryEvent (eve.Exception) THEN
        EXIT;
      END;
    END;

    dbg.Find_Debuggee_HWND;

    RETURN TRUE;
  ELSE
    RETURN rmt.RestartProgram ();
  END;
END RestartProgram;


PROCEDURE GetProgramInfo(VAR inf: kt.EXEC_INFO): BOOLEAN;
BEGIN
  IF NOT opt.RemoteMode THEN
    IF NOT Loaded THEN
      RETURN FALSE;
    END;
    IF NOT FillDebugInfo(info) THEN
      RETURN FALSE;
    END;
    inf := info;
    RETURN TRUE;
  ELSE
    RETURN rmt.GetProgramInfo (inf);
  END;
END GetProgramInfo;

  TYPE
     MAPSTRUCT = RECORD
          lpFile : win.PVOID;
          hMapFile: win.HANDLE;
          raw: dt.RAW_DEBUG_INFO;
     END;

PROCEDURE GetDebugInfo(info: kt.EXEC_INFO; info_addr: sys.ADDRESS): BOOLEAN;
VAR
  file : rf.ChanId;
  res  : rf.OpenResults;
  fp   : xfp.FilePos;
  PMapStruct: POINTER TO MAPSTRUCT;
  len  : CARDINAL;
BEGIN
  IF NOT opt.RemoteMode THEN
    IF info.DebugInfoSize = 0 THEN
      RETURN FALSE;
    END;
    IF opt.KernelInRemoteMode THEN
      -- поскольку невозможно смапировать файл на удаленной машине
      wio.MakeChannel(file, win.HANDLE(info.hFile), info.full_name, rf.raw + rf.read, res);
      IF res # rf.opened THEN
        RETURN FALSE;
      END;
      xfp.CardToPos(fp, info.DebugInfoStart);
      rf.SetPos(file, fp);
      ioc.RawRead (file, info_addr, info.DebugInfoSize, len);
      RETURN len = info.DebugInfoSize;
    ELSE
      PMapStruct:= info_addr;
      PMapStruct^.hMapFile := win.CreateFileMapping (win.HANDLE(info.hFile), NIL, win.PAGE_READONLY, 0, 0, NIL);
      IF PMapStruct^.hMapFile = NIL THEN
         RETURN FALSE;
      END;
      (* map view of entire file *)
      PMapStruct^.lpFile := win.MapViewOfFile (PMapStruct^.hMapFile, win.FILE_MAP_READ, 0, 0, 0);
      IF PMapStruct^.lpFile = NIL THEN
         win.CloseHandle (PMapStruct^.hMapFile);
         RETURN FALSE;
      END;
      PMapStruct^.raw:= sys.ADDRESS(PMapStruct^.lpFile) + info.DebugInfoStart;
      RETURN TRUE;
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
  oldBreakHandler: brk.BreakHandler;

BEGIN
  ProgramExecuting := FALSE;
  Finished := TRUE;
  oldBreakHandler := brk.SetBreakHandler (CtrlBreakHandler);
END Krn_Prog.