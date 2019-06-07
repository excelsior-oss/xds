IMPLEMENTATION MODULE Krn_Mem;

IMPORT sys := SYSTEM;
IMPORT        OS2;

IMPORT kt  := KrnTypes;
IMPORT eve := Events;
IMPORT rmt := Remote;
IMPORT opt := Options;

FROM Krn_Dbg IMPORT PID, TID;

---------------------- Доступ к памяти и регистрам -------------------------
                      -----------------------------


PROCEDURE GetRegisterCache(VAR Cache: kt.REGISTER_CACHE): BOOLEAN;
VAR
  DbgBuffer: OS2.uDB_t;
BEGIN
  IF opt.RemoteMode THEN
    RETURN rmt.GetRegisterCache(Cache);
  END;

  WITH DbgBuffer DO
    Cmd   := OS2.DBG_C_ReadReg;
    Pid   := PID;
    Tid   := TID;

    IF (OS2.DosDebug(sys.ADR(DbgBuffer)) = 0) AND (DbgBuffer.Cmd = OS2.DBG_N_Success) THEN
      WITH Cache DO
        Eax := EAX; Ebx := EBX; Ecx := ECX; Edx := EDX;
        Esi := ESI; Edi := EDI; Ebp := EBP; Esp := ESP;
        SegCs := CS; BaseCs := CSBase;
        SegSs := SS; BaseSs := SSBase;
        SegDs := DS; BaseDs := DSBase;
        SegEs := ES; BaseEs := ESBase;
        SegFs := FS; BaseFs := FSBase;
        SegGs := GS; BaseGs := GSBase;
        Eip := EIP; Cache.EFlags := DbgBuffer.EFlags;
      END;
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END;
END GetRegisterCache;

PROCEDURE PutRegisterCache(Cache: kt.REGISTER_CACHE): BOOLEAN;
VAR
  DbgBuffer: OS2.uDB_t;
BEGIN
  IF opt.RemoteMode THEN
    RETURN rmt.PutRegisterCache (Cache);
  END;

  WITH DbgBuffer DO
    Cmd   := OS2.DBG_C_WriteReg;
    Pid   := PID;
    Tid   := TID;
    WITH Cache DO
      EAX := Eax; EBX := Ebx; ECX := Ecx; EDX := Edx;
      ESI := Esi; EDI := Edi; EBP := Ebp; ESP := Esp;
      CS := SegCs; CSBase := BaseCs;
      SS := SegSs; SSBase := BaseSs;
      DS := SegDs; DSBase := BaseDs;
      ES := SegEs; ESBase := BaseEs;
      FS := SegFs; FSBase := BaseFs;
      GS := SegGs; GSBase := BaseGs;
      EIP   := Eip; DbgBuffer.EFlags := Cache.EFlags;
    END;
  END;
  IF OS2.DosDebug(sys.ADR(DbgBuffer)) # 0 THEN RETURN FALSE END;
  IF DbgBuffer.Cmd # OS2.DBG_N_Success THEN RETURN FALSE END;
  RETURN TRUE;
END PutRegisterCache;

(*
(* Получить значение из регистра       *)
PROCEDURE GetReg(regno: CARDINAL; VAR value: kt.REG_VALUE): BOOLEAN;
BEGIN
  IF opt.RemoteMode THEN
    RETURN rmt.GetReg (regno, value);
  END;

  IF NOT CacheOK THEN RETURN FALSE; END;
  CASE regno OF
  | 00H: value := RegisterCache.EAX MOD 100H;
  | 04H: value := (RegisterCache.EAX MOD 10000H) DIV 100H;
  | 08H: value := RegisterCache.EAX MOD 10000H;
  | 10H: value := RegisterCache.EAX;

  | 03H: value := RegisterCache.EBX MOD 100H;
  | 07H: value := (RegisterCache.EBX MOD 10000H) DIV 100H;
  | 0BH: value := RegisterCache.EBX MOD 10000H;
  | 13H: value := RegisterCache.EBX;

  | 01H: value := RegisterCache.ECX MOD 100H;
  | 05H: value := (RegisterCache.ECX MOD 10000H) DIV 100H;
  | 09H: value := RegisterCache.ECX MOD 10000H;
  | 11H: value := RegisterCache.ECX;

  | 02H: value := RegisterCache.EDX MOD 100H;
  | 06H: value := (RegisterCache.EDX MOD 10000H) DIV 100H;
  | 0AH: value := RegisterCache.EDX MOD 10000H;
  | 12H: value := RegisterCache.EDX;

  | 0EH: value := RegisterCache.ESI MOD 10000H;
  | 16H: value := RegisterCache.ESI;

  | 0FH: value := RegisterCache.EDI MOD 10000H;
  | 17H: value := RegisterCache.EDI;

  | 0DH: value := RegisterCache.EBP MOD 10000H;
  | 15H: value := RegisterCache.EBP;

  | 0CH: value := RegisterCache.ESP MOD 10000H;
  | 14H: value := RegisterCache.ESP;

  | 19H: value := VAL(CARDINAL, RegisterCache.CS);
  | 1BH: value := VAL(CARDINAL, RegisterCache.DS);
  | 1AH: value := VAL(CARDINAL, RegisterCache.SS);
  | 18H: value := VAL(CARDINAL, RegisterCache.ES);
  | 1DH: value := VAL(CARDINAL, RegisterCache.GS);
  | 1CH: value := VAL(CARDINAL, RegisterCache.FS);
  | 22H: value := RegisterCache.EIP;
  | 24H: value := RegisterCache.EFlags;
  ELSE
    RETURN FALSE;
  END;
  RETURN TRUE;
END GetReg;

(* Записать значение в регистр         *)
PROCEDURE SetReg(regno: CARDINAL; value: kt.REG_VALUE): BOOLEAN;
BEGIN
  IF opt.RemoteMode THEN
    RETURN rmt.SetReg (regno, value);
  END;

  WITH RegisterCache DO
    Cmd   := OS2.DBG_C_ReadReg;
    Pid   := PID;
    Tid   := TID;
  END;
  IF OS2.DosDebug(sys.ADR(RegisterCache)) # 0 THEN RETURN FALSE END;
  IF RegisterCache.Cmd # OS2.DBG_N_Success THEN RETURN FALSE END;
  CASE regno OF
  | 10H: RegisterCache.EAX := CARDINAL(value);
  | 13H: RegisterCache.EBX := CARDINAL(value);
  | 11H: RegisterCache.ECX := CARDINAL(value);
  | 12H: RegisterCache.EDX := CARDINAL(value);
  | 16H: RegisterCache.ESI := CARDINAL(value);
  | 17H: RegisterCache.EDI := CARDINAL(value);
  | 15H: RegisterCache.EBP := CARDINAL(value);
  | 14H: RegisterCache.ESP := CARDINAL(value);
  | 22H: RegisterCache.EIP := CARDINAL(value);
  | 24H: RegisterCache.EFlags := CARDINAL(value);
  ELSE
    RETURN FALSE;
  END;
  WITH RegisterCache DO
    Cmd   := OS2.DBG_C_WriteReg;
    Pid   := PID;
    Tid   := TID;
  END;
  IF OS2.DosDebug(sys.ADR(RegisterCache)) # 0 THEN RETURN FALSE END;
  IF RegisterCache.Cmd # OS2.DBG_N_Success THEN RETURN FALSE END;
  RETURN TRUE;
END SetReg;
*)

(* Прочитать с адреса в буфер немного байтов   *)
PROCEDURE Get(source: kt.ADDRESS; dest: sys.ADDRESS; len: CARDINAL): BOOLEAN;
VAR
  DbgBuffer: OS2.uDB_t;
BEGIN
  IF opt.RemoteMode THEN
    RETURN rmt.Get (source, dest, len);
  END;


  WITH DbgBuffer DO
    Cmd    := OS2.DBG_C_ReadMemBuf;
    Pid    := PID;
    Addr   := source;
    Buffer := CARDINAL(dest);
    Len    := len;
  END;
  IF OS2.DosDebug(sys.ADR(DbgBuffer)) # 0 THEN RETURN FALSE END;
  RETURN DbgBuffer.Cmd = OS2.DBG_N_Success;
END Get;

(* Записать по адресу из буфера немного байтов *)
PROCEDURE Put(dest: kt.ADDRESS; source: sys.ADDRESS; len: CARDINAL): BOOLEAN;
VAR
  DbgBuffer: OS2.uDB_t;
BEGIN
  IF opt.RemoteMode THEN
    RETURN rmt.Put (dest, source, len);
  END;

  WITH DbgBuffer DO
    Cmd    := OS2.DBG_C_WriteMemBuf;
    Pid    := PID;
    Addr   := dest;
    Buffer := CARDINAL(source);
    Len    := len;
  END;
  IF OS2.DosDebug(sys.ADR(DbgBuffer)) # 0 THEN RETURN FALSE END;
  RETURN DbgBuffer.Cmd = OS2.DBG_N_Success;
END Put;



PROCEDURE SetTrace (Access: eve.ACCESS_TYPE; addr: kt.ADDRESS; len: CARDINAL; VAR Index: CARDINAL): BOOLEAN;
VAR
  DbgBuffer: OS2.uDB_t;
BEGIN
  IF opt.RemoteMode THEN
    RETURN rmt.SetTrace (Access, addr, len, Index);
  END;

  WITH DbgBuffer DO
    Cmd   := OS2.DBG_C_SetWatch;
    Pid   := PID;
    Addr  := addr;
    Len   := len;
    Index := 0;
    CASE Access OF
    | eve.Read:
      Value := OS2.DBG_W_Local + OS2.DBG_W_ReadWrite;
    | eve.Write:
      Value := OS2.DBG_W_Local + OS2.DBG_W_Write;
    END;
  END;
  IF OS2.DosDebug(sys.ADR(DbgBuffer)) # 0 THEN RETURN FALSE END;
  CASE DbgBuffer.Cmd OF
  | OS2.DBG_N_Success: Index := DbgBuffer.Index; RETURN TRUE;
  | OS2.DBG_N_Error  : RETURN FALSE;
  ELSE
    ASSERT(FALSE);
  END;
END SetTrace;


PROCEDURE RemoveTrace (VAR index: CARDINAL): BOOLEAN;

VAR
  DbgBuffer: OS2.uDB_t;
BEGIN
  IF opt.RemoteMode THEN
    RETURN rmt.RemoveTrace (index);
  END;

  WITH DbgBuffer DO
    Cmd   := OS2.DBG_C_ClearWatch;
    Pid   := PID;
    Index := index;
  END;
  IF OS2.DosDebug(sys.ADR(DbgBuffer)) # 0 THEN RETURN FALSE END;
  CASE DbgBuffer.Cmd OF
  | OS2.DBG_N_Success:
    index := 0;
    RETURN TRUE;
  | OS2.DBG_N_Error  :
    RETURN FALSE;
  ELSE
    ASSERT(FALSE);
  END;
END RemoveTrace;

(* Получить информацию о сегменте *)
PROCEDURE GetSegmentInfo (      addr: kt.ADDRESS;
                          VAR  begin: kt.ADDRESS;
                          VAR    len: CARDINAL;
                          VAR access: kt.ATTRIBS): BOOLEAN;
VAR
  DbgBuffer: OS2.uDB_t;
BEGIN
  IF opt.RemoteMode THEN
    RETURN rmt.GetSegmentInfo (addr, begin, len, access);
  END;

  WITH DbgBuffer DO
    Cmd   := OS2.DBG_C_AddrToObject;
    Pid   := PID;
    Addr  := addr;
  END;
  ASSERT(OS2.DosDebug(sys.ADR(DbgBuffer)) = 0);
  IF DbgBuffer.Cmd = OS2.DBG_N_Success THEN
    begin   := DbgBuffer.Buffer;
    len     := DbgBuffer.Len;
    access  := kt.ATTRIBS{kt.read, kt.write, kt.execute, kt.bit_32};
    ASSERT(DbgBuffer.Value = OS2.DBG_O_OBJMTE);
    RETURN TRUE;
  ELSE
    ASSERT(DbgBuffer.Cmd = OS2.DBG_N_Error);
    RETURN FALSE;
  END;
END GetSegmentInfo;

PROCEDURE IsAddrFromExecutableSeg (addr: kt.ADDRESS): BOOLEAN;
BEGIN
  IF opt.RemoteMode THEN
    RETURN rmt.IsAddrFromExecutableSeg (addr);
  END;

  RETURN TRUE;
END IsAddrFromExecutableSeg;

BEGIN
END Krn_Mem.
