<* Storage+ *>

MODULE XProfTC;

IMPORT sys := SYSTEM;
IMPORT arg := ProgEnv;
IMPORT str := Strings;
IMPORT ts  := Str;
IMPORT fs  := FileSys;

IMPORT xStr;
IMPORT fil := File;
IMPORT tls := DI_Tools;
IMPORT typ := DI_Types;
IMPORT bld := DI_Build;

FROM Printf IMPORT printf;

<* IF env_target='x86os2' THEN *>

IMPORT pla := OS2;
IMPORT dbg := Dbg_HLL;

<* ELSE *>

IMPORT pla := Windows;
IMPORT dbg := DbgCView;

<* END *>



VAR
  prog_name: xStr.String;
  prog_args: xStr.String;
  string   : xStr.String;

CONST
  PRODUCT   = "XDS Profiler, trace execution utility";
  VERSION   = "0.01";
  COPYRIGHT = "1997-2001 Excelsior";


PROCEDURE Copyright;
BEGIN
  printf("\n%s, Version %s\n(c) %s\n\n", PRODUCT, VERSION, COPYRIGHT);
END Copyright;


PROCEDURE Help;
BEGIN
  printf('Usage: xproftc prog_name prog_args');
  HALT (0);
END Help;


PROCEDURE Options (opt-: ARRAY OF CHAR);
BEGIN
  printf(opt);
  IF opt[1] # 0C THEN Help; END;
END Options;

PROCEDURE ParseCommandLine;
VAR
  k, i, p: CARDINAL;
  a: xStr.String;
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
    arg.GetArg(i, string);
    <* PUSH *>
    <* WOFF903+ *>
    p := xStr.CharPos (a, ' ', f);
    <* POP *>
    IF f THEN
      xStr.Insert ('"', 0, a);
      xStr.Append ('"', a);
    END;
    INC(i);
    str.Append(string, prog_args);
    str.Append(' ', prog_args);
  END;
END ParseCommandLine;


TYPE

  RPROCEDURE = RECORD
                 Procedure : tls.OBJECT;
                 Prolog    : CARDINAL;
                 CodeProlog: sys.CARD8;
                 Epilog    : CARDINAL;
                 CodeEpilog: sys.CARD8;
                 Hits      : CARDINAL;
               END;
  PAPROCS = POINTER TO ARRAY OF RPROCEDURE;
  PAMODULES = POINTER TO ARRAY OF PAPROCS;


VAR
  Modules : PAMODULES;
  CodeInt3: sys.CARD8;
  CodeBegin: CARDINAL;



<* IF env_target = 'x86nt' THEN *>

VAR
  H_Process, H_Thread : pla.HANDLE;
  di: pla.DEBUG_EVENT;
  cont_flag: CARDINAL;
  ProgramTerminated: BOOLEAN;


PROCEDURE Get(source: CARDINAL; dest: sys.ADDRESS; len: CARDINAL): BOOLEAN;
VAR
  read_len: CARDINAL;
BEGIN
  IF NOT pla.ReadProcessMemory(H_Process, pla.PCVOID(source), dest, len, sys.ADR(read_len)) THEN RETURN FALSE END;
<* PUSH *>
<* WOFF304+ *>
  RETURN read_len = len;
<* POP *>
END Get;


PROCEDURE Put(dest: CARDINAL; source: sys.ADDRESS; len: CARDINAL): BOOLEAN;
VAR
  written: CARDINAL;
BEGIN
  IF NOT pla.WriteProcessMemory(H_Process, pla.PCVOID(dest), source, len, sys.ADR(written)) THEN RETURN FALSE END;
<* PUSH *>
<* WOFF304+ *>
  RETURN written = len;
<* POP *>
END Put;


PROCEDURE GetReg(regno: CARDINAL; VAR value: sys.WORD): BOOLEAN;
VAR
  Context: pla.CONTEXT;
BEGIN
  Context.ContextFlags := pla.CONTEXT_CONTROL + pla.CONTEXT_INTEGER + pla.CONTEXT_SEGMENTS;
  IF NOT pla.GetThreadContext(H_Thread, Context) THEN RETURN FALSE; END;
  CASE regno OF
  |  1: value := Context.Eax MOD 100H;
  |  5: value := (Context.Eax MOD 10000H) DIV 100H;
  |  9: value := Context.Eax MOD 10000H;
  | 17: value := Context.Eax;

  |  4: value := Context.Ebx MOD 100H;
  |  8: value := (Context.Ebx MOD 10000H) DIV 100H;
  | 12: value := Context.Ebx MOD 10000H;
  | 20: value := Context.Ebx;

  |  2: value := Context.Ecx MOD 100H;
  |  6: value := (Context.Ecx MOD 10000H) DIV 100H;
  | 10: value := Context.Ecx MOD 10000H;
  | 18: value := Context.Ecx;

  |  3: value := Context.Edx MOD 100H;
  |  7: value := (Context.Edx MOD 10000H) DIV 100H;
  | 11: value := Context.Edx MOD 10000H;
  | 19: value := Context.Edx;

  | 15: value := Context.Esi MOD 10000H;
  | 23: value := Context.Esi;

  | 16: value := Context.Edi MOD 10000H;
  | 24: value := Context.Edi;

  | 14: value := Context.Ebp MOD 10000H;
  | 22: value := Context.Ebp;

  | 13: value := Context.Esp MOD 10000H;
  | 21: value := Context.Esp;

  | 26: value := Context.SegCs;
  | 28: value := Context.SegDs;
  | 27: value := Context.SegSs;
  | 25: value := Context.SegEs;
  | 29: value := Context.SegFs;
  | 30: value := Context.SegGs;

  | 33: value := Context.Eip;

  | 34, 1000: value := Context.EFlags;
  ELSE
    RETURN FALSE;
  END;
  RETURN TRUE;
END GetReg;


PROCEDURE SetReg(regno: CARDINAL; value: sys.WORD): BOOLEAN;
VAR
  Context: pla.CONTEXT;
BEGIN
  Context.ContextFlags := pla.CONTEXT_CONTROL + pla.CONTEXT_INTEGER + pla.CONTEXT_SEGMENTS;
  IF NOT pla.GetThreadContext(H_Thread, Context) THEN RETURN FALSE; END;
  CASE regno OF
  | 17: Context.Eax := CARDINAL(value);
  | 20: Context.Ebx := CARDINAL(value);
  | 18: Context.Ecx := CARDINAL(value);
  | 19: Context.Edx := CARDINAL(value);
  | 23: Context.Esi := CARDINAL(value);
  | 24: Context.Edi := CARDINAL(value);
  | 22: Context.Ebp := CARDINAL(value);
  | 21: Context.Esp := CARDINAL(value);
  | 26: Context.SegCs := CARDINAL(value);
  | 28: Context.SegDs := CARDINAL(value);
  | 27: Context.SegSs := CARDINAL(value);
  | 33: Context.Eip := CARDINAL(value);
  | 34, 1000: Context.EFlags := CARDINAL(value);
  ELSE
    RETURN FALSE;
  END;
  Context.ContextFlags := pla.CONTEXT_CONTROL + pla.CONTEXT_INTEGER + pla.CONTEXT_SEGMENTS;
  RETURN pla.SetThreadContext(H_Thread, Context);
END SetReg;


PROCEDURE GetIP (): CARDINAL;
VAR
  Context: pla.CONTEXT;
BEGIN
  Context.ContextFlags := pla.CONTEXT_CONTROL;
  IF NOT pla.GetThreadContext(H_Thread, Context) THEN RETURN 0; END;
  RETURN  Context.Eip;
END GetIP;



PROCEDURE LoadProgram (): BOOLEAN;
VAR
  Title, PATH, f_name: xStr.String;
  a   : pla.PSTR;
  Cmd_Line: xStr.String;
  startup_info: pla.STARTUPINFO;
  pi: pla.PROCESS_INFORMATION;
  header    : pla.IMAGE_NT_HEADERS;
  pos: CARDINAL;

BEGIN
  (* Найти полный путь по redirection'у *)

  IF NOT fs.Exists(prog_name) THEN
    arg.String("PATH", PATH);
    PATH := PSTRING(path);
    IF pla.SearchPath(sys.ADR(PATH), sys.ADR(prog_name), NIL, SIZE(fprog_name), sys.ADR(fprog_name), a) = 0 THEN RETURN FALSE; END;
  ELSE
    fil.ModifyFileName(prog_name, fprog_name);
  END;

  Title := 'The XP application: ';
  xStr.Append(f_name, Title);

  WITH startup_info DO
    cb              := SIZE(startup_info);
    lpReserved      := NIL;
    lpDesktop       := NIL;
    lpTitle         := sys.ADR(Title);
    dwFlags         := pla.STARTF_SET{};
    cbReserved2     := 0;
    lpReserved2     := NIL;
  END;

  COPY(f_name, prog_name);
  COPY(f_name, Cmd_Line);
  xStr.Append(' ', Cmd_Line);
  xStr.Append(prog_args, Cmd_Line);
  IF NOT pla.CreateProcess(NIL, sys.ADR(Cmd_Line), NIL, NIL, TRUE,
                    pla.DEBUG_ONLY_THIS_PROCESS+pla.CREATE_NEW_CONSOLE+
                    pla.NORMAL_PRIORITY_CLASS, NIL, NIL,
                    sys.REF(startup_info), sys.ADR(pi))  THEN RETURN FALSE END;

  LOOP
    pla.WaitForDebugEvent(di , pla.INFINITE);
    WITH di DO
      CASE dwDebugEventCode OF
      | pla.CREATE_PROCESS_DEBUG_EVENT:
          WITH CreateProcessInfo DO
            H_Thread  := hThread;
            H_Process := hProcess;
            CodeBegin := 0;
            pos := CARDINAL(lpBaseOfImage);
            ASSERT(Get(pos, sys.ADR(CodeBegin), 2));
            ASSERT(CodeBegin = pla.IMAGE_DOS_SIGNATURE);
            INC(pos, SIZE(pla.IMAGE_DOS_HEADER));
            ASSERT(Get(pos-4, sys.ADR(CodeBegin), 4));
            pos := CARDINAL(lpBaseOfImage)+CodeBegin;
            ASSERT(Get(pos, sys.ADR(header), SIZE(header)));
            CodeBegin := CARDINAL(lpBaseOfImage) + header.OptionalHeader.BaseOfCode;
          END;
          EXIT;
      ELSE
      END;
    END;
    ASSERT(pla.ContinueDebugEvent(di.dwProcessId, di.dwThreadId, pla.DBG_CONTINUE));
  END;

 RETURN TRUE;
END LoadProgram;

<* END *>


PROCEDURE SetBreakpoint (addr: CARDINAL; VAR code: sys.CARD8);
BEGIN
  ASSERT( Get(addr, sys.ADR(code), 1) );
  ASSERT( Put(addr, sys.ADR(CodeInt3), 1) );
END SetBreakpoint;


PROCEDURE DoBreakpoint (addr: CARDINAL): BOOLEAN;
VAR
  m, p: CARDINAL;
BEGIN
  IF addr < CodeBegin THEN RETURN FALSE; END;
  m := tls.FindModByAddr(addr-CodeBegin);
  IF m = tls.Invalid_Module THEN RETURN FALSE; END;
  FOR p := 0 TO HIGH(Modules^[m-1]^) DO
    WITH Modules^[m-1]^[p] DO
      IF (addr = Prolog) THEN
        INC(Hits);
        RETURN TRUE;
      ELSIF (addr = Epilog) THEN
        RETURN TRUE;
      END;
    END;
  END;
  RETURN FALSE;
END DoBreakpoint;


PROCEDURE CheckBreakpoint (addr: CARDINAL; VAR code: sys.CARD8): BOOLEAN;
VAR
  m, p: CARDINAL;
BEGIN
  IF addr < CodeBegin THEN RETURN FALSE; END;
  m := tls.FindModByAddr(addr-CodeBegin);
  IF m = tls.Invalid_Module THEN RETURN FALSE; END;
  FOR p := 0 TO HIGH(Modules^[m-1]^) DO
    WITH Modules^[m-1]^[p] DO
      IF (addr = Prolog) THEN
        code := CodeProlog;
        RETURN TRUE;
      ELSIF (addr = Epilog) THEN
        code := CodeEpilog;
        RETURN TRUE;
      END;
    END;
  END;
  RETURN FALSE;
END CheckBreakpoint;



<* IF env_target = 'x86nt' THEN *>

PROCEDURE ProcessEvents;
VAR
  IP: CARDINAL;
BEGIN
  cont_flag := pla.DBG_CONTINUE;
  LOOP
    ASSERT(pla.ContinueDebugEvent(di.dwProcessId, di.dwThreadId, cont_flag));
    cont_flag := pla.DBG_CONTINUE;
    pla.WaitForDebugEvent(di , pla.INFINITE);
    WITH di DO
      CASE dwDebugEventCode OF
      | pla.EXCEPTION_DEBUG_EVENT:
        WITH Exception DO
          WITH ExceptionRecord DO
            CASE ExceptionCode OF
            | pla.EXCEPTION_SINGLE_STEP:
                EXIT;
            | pla.EXCEPTION_BREAKPOINT:
                IP := CARDINAL(ExceptionAddress);
                IF DoBreakpoint(IP) THEN
                  ASSERT(SetReg(33, IP));
                  EXIT;
                END;
            ELSE
              cont_flag := pla.DBG_EXCEPTION_NOT_HANDLED;
            END;
          END;
        END;
      | pla.LOAD_DLL_DEBUG_EVENT:
      | pla.UNLOAD_DLL_DEBUG_EVENT:
      | pla.CREATE_THREAD_DEBUG_EVENT:
      | pla.EXIT_THREAD_DEBUG_EVENT:
      | pla.EXIT_PROCESS_DEBUG_EVENT:
         ProgramTerminated := TRUE;
         EXIT;
      END;
    END;
  END;
END ProcessEvents;


PROCEDURE ExecutionProgram;
VAR
  add_step: BOOLEAN;
  flgs    : BITSET;
  Code    : sys.CARD8;
  IP: CARDINAL;
BEGIN
  ProgramTerminated := FALSE;
  LOOP
    ASSERT(GetReg(1000, flgs));
    INCL(flgs, 16);
    ASSERT(SetReg(1000, flgs));
    IP := GetIP();
    add_step := CheckBreakpoint(IP, Code);
    IF add_step THEN
      ASSERT(GetReg(1000, flgs));
      INCL(flgs, 8);
      ASSERT(SetReg(1000, flgs));
      ASSERT( Put(IP, sys.ADR(Code), 1) );
    END;
    ProcessEvents;
    IF ProgramTerminated THEN EXIT; END;
    IF add_step THEN
      ASSERT(GetReg(1000, flgs));
      EXCL(flgs, 8);
      ASSERT(SetReg(1000, flgs));
      ASSERT( Put(IP, sys.ADR(CodeInt3), 1) );
      ProcessEvents;
      IF ProgramTerminated THEN EXIT; END;
    END;
  END;
END ExecutionProgram;

<* END *>


PROCEDURE InitModules (): BOOLEAN;
VAR
  N, m, p: CARDINAL;
BEGIN
  CodeInt3 := 0CCH;
  IF NOT dbg.OpenExe(prog_name) THEN RETURN FALSE; END;
  dbg.ReadModules;
  NEW(Modules, typ.LastModule);
  FOR m := 0 TO typ.LastModule-1 DO
    dbg.BuildForMod(m);
    N := tls.LabelsNo(m+1);
    IF N = 0 THEN
      Modules^[m] := NIL;
    ELSE
      NEW(Modules^[m], N);
      FOR p := 0 TO N-1 DO
        WITH Modules^[m]^[p] DO
          Procedure  := tls.GetLabel(m+1, p);
          tls.ProcAttr(Procedure, Prolog, Epilog);
          INC(Prolog, CodeBegin);
          SetBreakpoint(Prolog, CodeProlog);
          INC(Epilog, CodeBegin);
          SetBreakpoint(Epilog, CodeEpilog);
          Hits  := 0;
        END;
      END;
    END;
  END;
  RETURN TRUE;
END InitModules;


PROCEDURE PrintModules;
VAR
  m, p: CARDINAL;
  name_ptr: xStr.txt_ptr;
  name_str: xStr.String;
  header: BOOLEAN;
BEGIN
  FOR m := 0 TO HIGH(Modules^) DO
    header := FALSE;
    IF Modules^[m] # NIL THEN
      FOR p := 0 TO HIGH(Modules^[m]^)  DO
        WITH Modules^[m]^[p] DO
          IF Hits # 0 THEN
            IF NOT header THEN
              ASSERT(tls.ModName(m+1, name_ptr));
              COPY(name_ptr^, name_str);
              printf('----------------------------------------------------------------\n');
              printf('Module %s\n', name_str);
              header := TRUE;
            END;
            ASSERT(tls.ObjectName(Procedure, name_ptr));
            COPY(name_ptr^, name_str);
            printf('  %-30s %u\n', name_str, Hits);
          END;
        END;
      END;
    END;
  END;
END PrintModules;


BEGIN
  Copyright;
  ParseCommandLine;
  IF NOT LoadProgram() THEN printf('Error: program %s not loaded.\n', prog_name); HALT (1); END;
  IF NOT InitModules() THEN printf("Error: can't read debug info.%s.\n", prog_name); HALT (2); END;
  ExecutionProgram;
FINALLY
  PrintModules;
END XProfTC.
