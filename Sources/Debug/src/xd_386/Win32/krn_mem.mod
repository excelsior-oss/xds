IMPLEMENTATION MODULE Krn_Mem;

IMPORT sys := SYSTEM;
IMPORT win := Windows;

IMPORT kt  := KrnTypes;
IMPORT opt := Options;
IMPORT rmt := Remote;
IMPORT thr := Threads;

FROM Krn_Dbg IMPORT H_Thread, H_Process;

FROM Events IMPORT ACCESS_TYPE;

<* IF DEFINED (xd_debug) & xd_debug THEN *>

FROM Printf IMPORT printf;

<* END *>

---------------------- Доступ к памяти и регистрам -------------------------
                      -----------------------------

CONST
  CONTEXT_FULL = win.CONTEXT_CONTROL
               + win.CONTEXT_INTEGER
               + win.CONTEXT_SEGMENTS
               + win.CONTEXT_FLOATING_POINT;


PROCEDURE GetRegisterCache(VAR RegisterCache: kt.REGISTER_CACHE): BOOLEAN;


  PROCEDURE GetBaseAddr (Selector: win.DWORD): kt.ADDRESS;
  VAR
    SelectorEntry: win.LDT_ENTRY;
  BEGIN
    IF NOT win.GetThreadSelectorEntry (H_Thread, Selector, SelectorEntry) THEN
      RETURN 0;
    END;
    RETURN VAL (CARDINAL, SelectorEntry.BaseHi ) * 01000000H +
           VAL (CARDINAL, SelectorEntry.BaseMid) *   010000H +
           VAL (CARDINAL, SelectorEntry.BaseLow);
  END GetBaseAddr;


VAR
  Context: win.CONTEXT;
  
BEGIN
  IF NOT opt.RemoteMode THEN
    Context.ContextFlags := CONTEXT_FULL;
    IF win.GetThreadContext(H_Thread, Context) THEN
      WITH RegisterCache DO
        Eax := Context.Eax; Ebx := Context.Ebx; Ecx := Context.Ecx; Edx := Context.Edx;
        Esi := Context.Esi; Edi := Context.Edi; Ebp := Context.Ebp; Esp := Context.Esp;

        SegCs := Context.SegCs; BaseCs := GetBaseAddr (SegCs);
        SegSs := Context.SegSs; BaseSs := GetBaseAddr (SegSs);
        SegDs := Context.SegDs; BaseDs := GetBaseAddr (SegDs);
        SegEs := Context.SegEs; BaseEs := GetBaseAddr (SegEs);
        SegFs := Context.SegFs; BaseFs := GetBaseAddr (SegFs);
        SegGs := Context.SegGs; BaseGs := GetBaseAddr (SegGs);

        Eip := Context.Eip;
        EFlags := Context.EFlags;

        WITH FloatingRegisters DO
          ControlWord   := Context.FloatSave.ControlWord;
          StatusWord    := Context.FloatSave.StatusWord;
          TagWord       := Context.FloatSave.TagWord;
          ErrorOffset   := Context.FloatSave.ErrorOffset;
          ErrorSelector := Context.FloatSave.ErrorSelector;
          DataOffset    := Context.FloatSave.DataOffset;
          DataSelector  := Context.FloatSave.DataSelector;
          Cr0NpxState   := Context.FloatSave.Cr0NpxState;
          sys.MOVE (sys.ADR(Context.FloatSave.RegisterArea), sys.ADR(RegisterData), SIZE(RegisterData));
        END;
      END;
  
      RETURN TRUE;  
    ELSE
--      ASSERT (FALSE, win.GetLastError());
      RETURN FALSE;  
    END;
  ELSE
    RETURN rmt.GetRegisterCache (RegisterCache);
  END;
END GetRegisterCache;

(* Записать значение в регистр         *)
PROCEDURE PutRegisterCache(RegisterCache: kt.REGISTER_CACHE): BOOLEAN;
VAR
  Context: win.CONTEXT;

BEGIN
  IF NOT opt.RemoteMode THEN
    WITH Context DO
      Eax := RegisterCache.Eax; Ebx := RegisterCache.Ebx; Ecx := RegisterCache.Ecx; Edx := RegisterCache.Edx;
      Esi := RegisterCache.Esi; Edi := RegisterCache.Edi; Ebp := RegisterCache.Ebp; Esp := RegisterCache.Esp;
      SegCs := RegisterCache.SegCs;
      SegSs := RegisterCache.SegSs;
      SegDs := RegisterCache.SegDs;
      SegEs := RegisterCache.SegEs;
      SegFs := RegisterCache.SegFs;
      SegGs := RegisterCache.SegGs;
      Eip := RegisterCache.Eip; EFlags := RegisterCache.EFlags;
      WITH FloatSave DO
        ControlWord   := RegisterCache.FloatingRegisters.ControlWord;
        StatusWord    := RegisterCache.FloatingRegisters.StatusWord;
        TagWord       := RegisterCache.FloatingRegisters.TagWord;
        ErrorOffset   := RegisterCache.FloatingRegisters.ErrorOffset;
        ErrorSelector := RegisterCache.FloatingRegisters.ErrorSelector;
        DataOffset    := RegisterCache.FloatingRegisters.DataOffset;
        DataSelector  := RegisterCache.FloatingRegisters.DataSelector;
        Cr0NpxState   := RegisterCache.FloatingRegisters.Cr0NpxState;
        sys.MOVE (sys.ADR(RegisterCache.FloatingRegisters.RegisterData), sys.ADR(RegisterArea), SIZE(RegisterArea));
      END;
    END;
  
    Context.ContextFlags := CONTEXT_FULL;
    RETURN win.SetThreadContext(H_Thread, Context);
  ELSE
    RETURN rmt.PutRegisterCache (RegisterCache);
  END;
END PutRegisterCache;



PROCEDURE Get(source: kt.ADDRESS; dest: sys.ADDRESS; len: CARDINAL): BOOLEAN;
VAR
  read_len: CARDINAL;

BEGIN
  IF NOT opt.RemoteMode THEN
    IF NOT win.ReadProcessMemory(H_Process, win.LPCVOID(source), dest, len, read_len) THEN
      RETURN FALSE;
    END;
    <* PUSH *>
    <* WOFF304+ *>
    RETURN (len=read_len);
    <* POP *>
  ELSE
    RETURN rmt.Get (source, dest, len);
  END;
END Get;

(* Записать по адресу из буфера немного байтов *)
PROCEDURE Put(dest: kt.ADDRESS; source: sys.ADDRESS; len: CARDINAL): BOOLEAN;
VAR
  written: CARDINAL;
BEGIN
  IF NOT opt.RemoteMode THEN
    IF NOT win.WriteProcessMemory(H_Process, win.LPCVOID(dest), source, len, written) THEN RETURN FALSE END;
    <* PUSH *>
    <* WOFF304+ *>
    RETURN written = len;
    <* POP *>
  ELSE
    RETURN rmt.Put (dest, source, len);
  END;
END Put;


CONST
  MainThread = 0;
  -- главный поток всегда имеет нулевой индекс,
  -- кроме того, он всегда создается при создании программы


<* IF DEFINED (xd_debug) & xd_debug THEN *>

PROCEDURE PrintThreadInfo (header-: ARRAY OF CHAR);
VAR
  ThreadContext: win.CONTEXT;
  thread: CARDINAL;
BEGIN
  printf ("***** Threads info: %s *****\n", header);
  IF thr.Threads.Count > 0 THEN
    FOR thread := 0 TO thr.Threads.Count-1 DO
      WITH thr.Threads.Threads^[thread] DO
        printf ("  thread %d\n    TID=%d (0x%X)\n", thread, ID, ID);
        ThreadContext.ContextFlags := win.CONTEXT_DEBUG_REGISTERS;
        ASSERT (win.GetThreadContext (Handle, ThreadContext));
        printf ("    Dr0=0x%$8X\n    Dr1=0x%$8X\n    Dr2=0x%$8X\n    Dr3=0x%$8X\n    Dr6=0x%$8X\n    Dr7=0x%$8X\n"
               , ThreadContext.Dr0
               , ThreadContext.Dr1
               , ThreadContext.Dr2
               , ThreadContext.Dr3
               , ThreadContext.Dr6
               , ThreadContext.Dr7 );
      END;
    END;
  END;
END PrintThreadInfo;

<* END *>


PROCEDURE SynchronizeTraceForAllThreads ();
-- синхронизация трассировки происходит по главному потоку
VAR
  MainThreadContext: win.CONTEXT;
  ThreadContext: win.CONTEXT;
  thread, index: CARDINAL;
BEGIN
 <* IF DEFINED (xd_debug) & xd_debug THEN *>
  IF opt.Debug (opt.Another) THEN
   PrintThreadInfo ("before SynchronizeTraceForAllThreads()");
  END;
 <* END *>
  IF thr.Threads.Count > 1 THEN
    MainThreadContext.ContextFlags := win.CONTEXT_DEBUG_REGISTERS;
    ASSERT (win.GetThreadContext (thr.Threads.Threads^[MainThread].Handle, MainThreadContext));
    FOR thread := 1 TO thr.Threads.Count-1 DO
      WITH thr.Threads.Threads^[thread] DO
        FOR index := 1 TO 4 DO
          IF thr.Threads.Threads^[MainThread].AccessBreaks[index] AND NOT AccessBreaks[index] THEN
            ThreadContext.ContextFlags := win.CONTEXT_DEBUG_REGISTERS;
            ASSERT (win.GetThreadContext (Handle, ThreadContext));
            ThreadContext.Dr0 := MainThreadContext.Dr0;
            ThreadContext.Dr1 := MainThreadContext.Dr1;
            ThreadContext.Dr2 := MainThreadContext.Dr2;
            ThreadContext.Dr3 := MainThreadContext.Dr3;
            ThreadContext.Dr6 := MainThreadContext.Dr6;
            ThreadContext.Dr7 := MainThreadContext.Dr7;
            ThreadContext.ContextFlags := win.CONTEXT_DEBUG_REGISTERS;
            ASSERT (win.SetThreadContext (Handle, ThreadContext));
            AccessBreaks[index] := TRUE;
          END;
        END;
      END;
    END;
  END;
 <* IF DEFINED (xd_debug) & xd_debug THEN *>
  IF opt.Debug (opt.Another) THEN
    PrintThreadInfo ("after SynchronizeTraceForAllThreads()");
  END;
 <* END *>
END SynchronizeTraceForAllThreads;



(* Установить трассировку записи в поле по адресу с заданой длиной *)
PROCEDURE SetTrace (Access: ACCESS_TYPE; addr: kt.ADDRESS; len: CARDINAL; VAR Index: CARDINAL): BOOLEAN;
VAR
  Context: win.CONTEXT;
  i, t: CARDINAL;
  DR7 : BITSET;
  pos : CARDINAL;
BEGIN
  IF NOT opt.RemoteMode THEN
    IF (len # 1) AND (len # 2) AND (len # 4) OR (addr MOD len # 0) THEN RETURN FALSE; END;
    Context.ContextFlags := win.CONTEXT_DEBUG_REGISTERS;
    Index := 0;
    ASSERT (thr.Threads.Count > 0);
    FOR t := 0 TO thr.Threads.Count-1 DO
      WITH thr.Threads.Threads^[t] DO
        ASSERT (win.GetThreadContext (Handle, Context));
        DR7 := BITSET(Context.Dr7);
        i := 1;
        LOOP
          IF i > 4 THEN
            EXIT;
          END;
          IF ((Index=0) AND NOT AccessBreaks[i]) OR (i=Index) THEN
            CASE i OF
            | 1: Context.Dr0 := addr;
            | 2: Context.Dr1 := addr;
            | 3: Context.Dr2 := addr;
            | 4: Context.Dr3 := addr;
            END;
            pos := 12 + i*4;
            INCL(DR7, pos);
            CASE Access OF
            | Write : EXCL(DR7, pos+1);
            | Read  : INCL(DR7, pos+1);
            END;
            CASE len OF
            | 1: EXCL(DR7, pos+2); EXCL(DR7, pos+3);
            | 2: INCL(DR7, pos+2); EXCL(DR7, pos+3);
            | 4: INCL(DR7, pos+2); INCL(DR7, pos+3);
            END;
            INCL(DR7, (i-1)*2);
            EXCL(DR7, (i-1)*2+1);
            Context.Dr7 := CARDINAL(DR7);
            ASSERT (win.SetThreadContext (Handle, Context));
            AccessBreaks[i] := TRUE;
            Index := i;
            EXIT;
          END;
          INC(i);
        END;
      END;
    END;
    SynchronizeTraceForAllThreads ();
    RETURN Index # 0;
  ELSE
    RETURN rmt.SetTrace (Access, addr, len, Index);
  END;
END SetTrace;


(* Снять трассировку записи в поле по адресу с заданой длиной *)
PROCEDURE RemoveTrace (VAR index: CARDINAL): BOOLEAN;
VAR
  Context: win.CONTEXT;
  DR7: BITSET;
  t  : CARDINAL;
BEGIN
  IF NOT opt.RemoteMode THEN
    IF thr.Threads.Count > 0 THEN
      FOR t := 0 TO thr.Threads.Count-1 DO
        WITH thr.Threads.Threads^[t] DO
          IF AccessBreaks[index] THEN
            Context.ContextFlags := win.CONTEXT_DEBUG_REGISTERS;
            ASSERT(win.GetThreadContext(Handle, Context));
            DR7 := BITSET(Context.Dr7);
            EXCL(DR7, (index-1)*2);
            Context.Dr7 := CARDINAL(DR7);
            Context.ContextFlags := win.CONTEXT_DEBUG_REGISTERS;
            ASSERT(win.SetThreadContext(Handle, Context));
            AccessBreaks[index] := FALSE;
          END;
        END;
      END;
    END;
    index := 0;
    RETURN TRUE;
  ELSE
    RETURN rmt.RemoveTrace (index);
  END;
END RemoveTrace;


(* Получить информацию о сегменте *)
PROCEDURE GetSegmentInfo (      addr: kt.ADDRESS;
                          VAR  begin: kt.ADDRESS;
                          VAR    len: CARDINAL;
                          VAR access: kt.ATTRIBS): BOOLEAN;

VAR
  mem_info: win.MEMORY_BASIC_INFORMATION;
BEGIN
  IF NOT opt.RemoteMode THEN
    win.VirtualQueryEx(H_Process, sys.ADDRESS(addr), mem_info, SIZE(mem_info));
    begin        := kt.ADDRESS(mem_info.BaseAddress);
    len          := mem_info.RegionSize;
    access       := kt.ATTRIBS{kt.execute, kt.read, kt.write, kt.bit_32}; --!!!!!!!!!!!!!!!11
    RETURN (win.PROTECT_SET(mem_info.State)   = win.MEM_COMMIT)    AND
           (win.PROTECT_SET(mem_info.Protect) # win.PAGE_NOACCESS) AND
           (win.PROTECT_SET(mem_info.Protect) # win.PROTECT_SET{});
  ELSE
    RETURN rmt.GetSegmentInfo (addr, begin, len, access);
  END;
END GetSegmentInfo;

PROCEDURE IsAddrFromExecutableSeg (addr: kt.ADDRESS): BOOLEAN;
VAR
  mem_info: win.MEMORY_BASIC_INFORMATION;

BEGIN
  IF NOT opt.RemoteMode THEN
    win.VirtualQueryEx(H_Process, sys.ADDRESS(addr), mem_info, SIZE(mem_info));
    CASE opt.TargetSystem OF
    | opt.win_nt:
      RETURN ((win.PAGE_EXECUTE+win.PAGE_EXECUTE_READ+ win.PAGE_EXECUTE_READWRITE+ win.PAGE_EXECUTE_WRITECOPY)
             * win.PROTECT_SET(mem_info.Protect)) # win.PROTECT_SET{};
    | opt.win95:
      RETURN ((win.PAGE_READWRITE) * win.PROTECT_SET(mem_info.Protect)) # win.PROTECT_SET{};
    END;
  ELSE
    RETURN rmt.IsAddrFromExecutableSeg (addr);
  END;
END IsAddrFromExecutableSeg;


BEGIN
 <* IF DEFINED (xd_debug) & xd_debug THEN *>
  ASSERT(SIZE(kt.REGISTER_DATA)=win.SIZE_OF_80387_REGISTERS);
 <* END *>
END Krn_Mem.
