IMPLEMENTATION MODULE Krn_Mem;

IMPORT sys := SYSTEM;

IMPORT kt  := KrnTypes;
IMPORT opt := Options;
IMPORT rmt := Remote;
IMPORT thr := Threads;

IMPORT lnx := Linux;

FROM SYSTEM IMPORT CARD16;

FROM Krn_Dbg IMPORT H_Thread;--, H_Process;

FROM Events IMPORT ACCESS_TYPE;

<* IF DEFINED (xd_debug) & xd_debug THEN *>

FROM Printf IMPORT printf;

<* END *>

---------------------- Доступ к памяти и регистрам -------------------------
                      -----------------------------


PROCEDURE GetRegisterCache(VAR RegisterCache: kt.REGISTER_CACHE): BOOLEAN;
VAR
  regs: ARRAY [0..lnx.REGS_NO-1] OF CARDINAL;
BEGIN
  IF lnx.proc_get_regs(H_Thread, regs) THEN
    WITH RegisterCache DO
      Eax := regs[lnx.EAX];
      Ebx := regs[lnx.EBX];
      Ecx := regs[lnx.ECX];
      Edx := regs[lnx.EDX];
      Esi := regs[lnx.ESI];
      Edi := regs[lnx.EDI];
      Ebp := regs[lnx.EBP];
      Esp := regs[lnx.ESP];

--      printf ("**** GetRegisterCache: H_Thread=0x%x\n\t\tXCS=0x%$8x, XSS=0x%$8x, XDS=0x%$8x, XES=0x%$8x, XFS=0x%$8x, XGS=0x%$8x\n",
--              H_Thread, regs[lnx.XCS], regs[lnx.XSS], regs[lnx.XDS], regs[lnx.XES], regs[lnx.XFS], regs[lnx.XGS]);
(*      
      SegCs := CARD16(regs[lnx.XCS]); BaseCs := 0;
      SegSs := CARD16(regs[lnx.XSS]); BaseSs := 0;
      SegDs := CARD16(regs[lnx.XDS]); BaseDs := 0;
      SegEs := CARD16(regs[lnx.XES]); BaseEs := 0;
      SegFs := CARD16(regs[lnx.XFS]); BaseFs := 0;
      SegGs := CARD16(regs[lnx.XGS]); BaseGs := 0;
*)
(*
      SegCs := CARD16(regs[lnx.XCS]); BaseCs := lnx.get_base_addr (regs[lnx.XCS]);
      SegSs := CARD16(regs[lnx.XSS]); BaseSs := lnx.get_base_addr (regs[lnx.XSS]);
      SegDs := CARD16(regs[lnx.XDS]); BaseDs := lnx.get_base_addr (regs[lnx.XDS]);
      SegEs := CARD16(regs[lnx.XES]); BaseEs := lnx.get_base_addr (regs[lnx.XES]);
      SegFs := CARD16(regs[lnx.XFS]); BaseFs := lnx.get_base_addr (regs[lnx.XFS]);
      SegGs := CARD16(regs[lnx.XGS]); BaseGs := lnx.get_base_addr (regs[lnx.XGS]);
*)
      SegCs := CARD16(regs[lnx.XCS]); BaseCs := 0;
      SegSs := CARD16(regs[lnx.XSS]); BaseSs := 0;
      SegDs := CARD16(regs[lnx.XDS]); BaseDs := 0;
      SegEs := CARD16(regs[lnx.XES]); BaseEs := SegEs;
      SegFs := CARD16(regs[lnx.XFS]); BaseFs := SegFs;
      SegGs := CARD16(regs[lnx.XGS]); BaseGs := SegGs;

      Eip := regs[lnx.EIP];
      EFlags := regs[lnx.EFLAGS];
(*
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
*)
    END;

    RETURN TRUE;  
  ELSE
    RETURN FALSE;
  END;
END GetRegisterCache;


(* Записать значение в регистр         *)
PROCEDURE PutRegisterCache(RegisterCache: kt.REGISTER_CACHE): BOOLEAN;
VAR
  regs: ARRAY [0..lnx.REGS_NO-1] OF CARDINAL;
BEGIN
  WITH RegisterCache DO
    regs[lnx.EAX] := Eax;
    regs[lnx.EBX] := Ebx;
    regs[lnx.ECX] := Ecx;
    regs[lnx.EDX] := Edx;
    regs[lnx.ESI] := Esi;
    regs[lnx.EDI] := Edi;
    regs[lnx.EBP] := Ebp;
    regs[lnx.ESP] := Esp;

    regs[lnx.XCS] := CARDINAL(SegCs);
    regs[lnx.XSS] := CARDINAL(SegSs);
    regs[lnx.XDS] := CARDINAL(SegDs);
    regs[lnx.XES] := CARDINAL(SegEs);
    regs[lnx.XFS] := CARDINAL(SegFs);
    regs[lnx.XGS] := CARDINAL(SegGs);

--    printf ("**** PutRegisterCache: XCS=0x%$8x, XSS=0x%$8x, XDS=0x%$8x, XES=0x%$8x, XFS=0x%$8x, XGS=0x%$8x\n",
--            regs[lnx.XCS], regs[lnx.XSS], regs[lnx.XDS], regs[lnx.XES], regs[lnx.XFS], regs[lnx.XGS]);
      

    regs[lnx.EIP] := Eip;
    regs[lnx.EFLAGS] := EFlags;
(*
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
*)
  END;

  RETURN lnx.proc_set_regs(H_Thread, regs);
END PutRegisterCache;


PROCEDURE Get(source: kt.ADDRESS; dest: sys.ADDRESS; len: CARDINAL): BOOLEAN;
BEGIN
  RETURN lnx.proc_get_memory (H_Thread, CARDINAL(source), len, dest);
END Get;

(* Записать по адресу из буфера немного байтов *)
PROCEDURE Put(dest: kt.ADDRESS; source: sys.ADDRESS; len: CARDINAL): BOOLEAN;
BEGIN
  RETURN lnx.proc_set_memory (H_Thread, source, len, CARDINAL(dest));
END Put;


CONST
  MainThread = 0;
  -- главный поток всегда имеет нулевой индекс,
  -- кроме того, он всегда создается при создании программы


<* IF DEFINED (xd_debug) & xd_debug THEN *>

PROCEDURE PrintThreadInfo (header-: ARRAY OF CHAR);
BEGIN
  printf ("***** Threads info: %s *****\n", header);
(*
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
*)
END PrintThreadInfo;

<* END *>


PROCEDURE SynchronizeTraceForAllThreads ();
-- синхронизация трассировки происходит по главному потоку
BEGIN
 <* IF DEFINED (xd_debug) & xd_debug THEN *>
  IF opt.Debug (opt.Another) THEN
   PrintThreadInfo ("before SynchronizeTraceForAllThreads()");
  END;
 <* END *>
 <* IF DEFINED (xd_debug) & xd_debug THEN *>
  IF opt.Debug (opt.Another) THEN
    PrintThreadInfo ("after SynchronizeTraceForAllThreads()");
  END;
 <* END *>
END SynchronizeTraceForAllThreads;



(* Установить трассировку записи в поле по адресу с заданой длиной *)
PROCEDURE SetTrace (Access: ACCESS_TYPE; addr: kt.ADDRESS; len: CARDINAL; VAR Index: CARDINAL): BOOLEAN;
VAR
  dbg_regs: ARRAY [0..lnx.DBGREGS_NO-1] OF CARDINAL;
  i, t: CARDINAL;
  DR7 : BITSET;
  pos : CARDINAL;
BEGIN
  IF NOT opt.RemoteMode THEN
    IF (len # 1) AND (len # 2) AND (len # 4) OR (addr MOD len # 0) THEN RETURN FALSE; END;
    Index := 0;
    ASSERT (thr.Threads.Count > 0);
    FOR t := 0 TO thr.Threads.Count-1 DO
      WITH thr.Threads.Threads^[t] DO
        ASSERT(lnx.proc_get_dbgregs(Handle, dbg_regs));
        DR7 := BITSET(dbg_regs[7]);
        i := 1;
        LOOP
          IF i > 4 THEN
            EXIT;
          END;
          IF ((Index=0) AND NOT AccessBreaks[i]) OR (i=Index) THEN
            CASE i OF
            | 1: dbg_regs[0] := addr;
            | 2: dbg_regs[1] := addr;
            | 3: dbg_regs[2] := addr;
            | 4: dbg_regs[3] := addr;
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
            dbg_regs[7] := CARDINAL(DR7);
            ASSERT(lnx.proc_set_dbgregs(Handle, dbg_regs));
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
  dbg_regs: ARRAY [0..lnx.DBGREGS_NO-1] OF CARDINAL;
  DR7: BITSET;
  t  : CARDINAL;
BEGIN
  IF NOT opt.RemoteMode THEN
    IF thr.Threads.Count > 0 THEN
      FOR t := 0 TO thr.Threads.Count-1 DO
        WITH thr.Threads.Threads^[t] DO
          IF AccessBreaks[index] THEN
            ASSERT(lnx.proc_get_dbgregs(Handle, dbg_regs));
            DR7 := BITSET(dbg_regs[7]);
            EXCL(DR7, (index-1)*2);
            dbg_regs[7] := CARDINAL(DR7);
            ASSERT(lnx.proc_set_dbgregs(Handle, dbg_regs));
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
  beg: CARDINAL;
  l, acc: INTEGER;
BEGIN
  IF NOT lnx.get_segment_info (CARDINAL(addr), beg, l, acc)
  THEN
    RETURN FALSE;
  END;
  begin := kt.ADDRESS(beg);
  len := kt.ADDRESS(l);

  access := kt.ATTRIBS{kt.bit_32};
  IF (acc & lnx.VM_EXEC) # 0 THEN
    INCL (access, kt.execute);
  END;
  IF (acc & lnx.VM_READ) # 0 THEN
    INCL (access, kt.read);
  END;
  IF (acc & lnx.VM_WRITE) # 0 THEN
    INCL (access, kt.write);
  END;
  RETURN TRUE;
END GetSegmentInfo;

PROCEDURE IsAddrFromExecutableSeg (addr: kt.ADDRESS): BOOLEAN;
VAR
  beg: CARDINAL;
  l, acc: INTEGER;
BEGIN
  IF NOT lnx.get_segment_info (CARDINAL(addr), beg, l, acc)
  THEN
    RETURN FALSE;
  END;
  RETURN (acc & lnx.VM_EXEC) # 0;
END IsAddrFromExecutableSeg;


BEGIN
END Krn_Mem.
