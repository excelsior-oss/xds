IMPLEMENTATION MODULE Exe_Mem;

IMPORT sys  := SYSTEM;

IMPORT kt   := KrnTypes;
IMPORT kmem := Krn_Mem;
IMPORT ind  := KrnIndex;

IMPORT dt  := DI_Types;
IMPORT tls := DI_Tools;

<* IF DEFINED (xd_debug) & xd_debug THEN *>

IMPORT io  := InOut;
IMPORT opt := Options;

<* END *>

---------------------- Доступ к памяти и регистрам -------------------------
                      -----------------------------
CONST
  MaxPages     = 32;   (* Максимальное число страниц в кэше *)
  CacheSize    = 4096;

TYPE
  TCacheMem    = ARRAY [0..CacheSize-1] OF sys.CARD8;
  TPage        = RECORD
                   CacheMem     : TCacheMem; (* Память страницы кэша        *)
                   CacheMemOk   : BOOLEAN;
                   CacheAddr    : CARDINAL;
                   Calls        : CARDINAL;  (* Кол-во обращений к странице *)
                   LastTime     : CARDINAL;  (* Время последнего обращения  *)
                   Scanned      : BOOLEAN;   (* Страница отсканирована *)
                 END;

VAR
  RegisterCache: kt.REGISTER_CACHE;
  CacheOk      : BOOLEAN;

  Pages        : ARRAY[1..MaxPages] OF TPage;
  UsedPages    : CARDINAL;

  FlashedPages : CARDINAL;
  MaxUsedPages : CARDINAL;

  TimeCounter  : sys.CARD32;  (* Счетчик времени *)

(* Сбрасывает кэш памяти *)
PROCEDURE ClearCaches();
VAR
  i            : CARDINAL;
BEGIN
  FOR i:=1 TO MaxPages DO Pages[i].CacheMemOk := FALSE; END;
  FlashedPages:= 0;
  TimeCounter:=  0;
  MaxUsedPages:= 0;
  UsedPages:=    0;
END ClearCaches;

PROCEDURE GetCaches();
BEGIN
  CacheOk:= kmem.GetRegisterCache(RegisterCache);
  ClearCaches;
END GetCaches;


(* Получить значение из регистра       *)
PROCEDURE GetReg(regno: CARDINAL; VAR value: kt.REG_VALUE): BOOLEAN;
BEGIN
  IF NOT CacheOk THEN RETURN FALSE; END;
  CASE regno OF
  | 00H..03H: value := RegisterCache.Common[regno].l;                (* AL..DL   *)
  | 04H..07H: value := RegisterCache.Common[regno-04H].h;            (* AH..DH   *)
  | 08H..0FH: value := RegisterCache.Common[regno-08H].w;            (* AX..DI   *)
  | 10H..17H: value := RegisterCache.Common[regno-10H].dw;           (* EAX..EDI *)

  | 18H..1DH: value := VAL(CARDINAL, RegisterCache.Bases[regno-18H]);(* ES..GS   *)
  | 22H     : value := RegisterCache.Eip;
  | 24H     : value := RegisterCache.EFlags;
  ELSE
    RETURN FALSE;
  END;
  RETURN TRUE;
END GetReg;


(* Записать значение в регистр         *)
PROCEDURE SetReg(regno: CARDINAL; value: kt.REG_VALUE): BOOLEAN;
BEGIN
  CASE regno OF
  | 10H..17H: RegisterCache.Common[regno-10H].dw := CARDINAL(value);                  (* EAX..EDI *)
  | 08H..0FH: RegisterCache.Common[regno-08H].w  := VAL(sys.CARD16, CARDINAL(value)); (* AX..DI   *)
  | 00H..03H: RegisterCache.Common[regno].l      := VAL(sys.CARD8, CARDINAL(value));  (* AL..DL   *)
  | 04H..07H: RegisterCache.Common[regno-04H].h  := VAL(sys.CARD8, CARDINAL(value));  (* AH..DH   *)

  | 18H..1DH: RegisterCache.Segs[regno-18H] := VAL(sys.CARD16, CARDINAL(value));      (* ES..GS   *)

  | 22H: RegisterCache.Eip    := CARDINAL(value);
  | 24H: RegisterCache.EFlags := CARDINAL(value);
  ELSE
    RETURN FALSE;
  END;
  RETURN kmem.PutRegisterCache(RegisterCache);
EXCEPT
  RETURN FALSE;
END SetReg;


<* IF DEST_XDS AND (TARGET_OS = "WINNT") THEN *>

(* Получить данные о плавающих регитстрах *)
PROCEDURE GetFloatRegs (VAR FloatRegs: kt.FLOATING_REGISTER_CACHE): BOOLEAN;
BEGIN
  IF NOT CacheOk THEN RETURN FALSE; END;
  FloatRegs := RegisterCache.FloatingRegisters;
  RETURN TRUE;
END GetFloatRegs;

(* Установить значения плавающих регитстров *)
PROCEDURE SetFloatRegs (FloatRegs: kt.FLOATING_REGISTER_CACHE): BOOLEAN;
BEGIN
  RegisterCache.FloatingRegisters := FloatRegs;
  RETURN kmem.PutRegisterCache(RegisterCache);
END SetFloatRegs;

<* END *>


PROCEDURE ScanForBreaks (addr: sys.ADDRESS; begin, end: CARDINAL);
VAR
  i: CARDINAL;
BEGIN
--  IF IsAddrFromExecutableSeg(begin) THEN
    WITH ind.Index DO
      IF Free > 0 THEN
        FOR i := 0 TO Free-1 DO
          WITH Index^[i] DO
            IF Busy AND (Access = ind.EXECUTE) THEN
              IF (Addr >= begin) AND (Addr <= end) THEN
                sys.MOVE(sys.ADR(OpCode), sys.ADDADR(addr, Addr-begin), kt.BreakLen);
              END;
            END;
          END;
        END;
      END;
    END;
--  END;
END ScanForBreaks;

(*
PROCEDURE UpdateBreaks (addr: sys.ADDRESS; begin, end: kt.ADDRESS);
VAR
  i: CARDINAL;
  End: kt.ADDRESS;
  ln, pos: CARDINAL;
BEGIN
  WITH ind.Index DO
    IF Free > 0 THEN
      FOR i := 0 TO Free-1 DO
        WITH Index^[i] DO
          IF Busy AND (Access = ind.EXECUTE) THEN
            End := Addr+kt.BreakLen-1;
            IF Addr <= begin THEN
              IF begin <= End THEN
                pos := begin-Addr;
                IF End <= end THEN
                  ln := End-begin+1;
                  sys.MOVE(addr, sys.ADR(OpCode[pos]), ln);
                ELSE
                  ln := end-begin+1;
                  sys.MOVE(addr, sys.ADR(OpCode[pos]), ln);
                END;
              END;
            ELSIF Addr <= end THEN
              IF End <= end THEN
                sys.MOVE(sys.ADDADR(addr, Addr-begin), sys.ADR(OpCode), kt.BreakLen);
              ELSE
                ln := end-Addr+1;
                sys.MOVE(sys.ADDADR(addr, Addr-begin), sys.ADR(OpCode), ln);
              END;
            END;
          END;
        END;
      END;
    END;
  END;
END UpdateBreaks;
*)


PROCEDURE GetCacheMem(VAR CacheMem : TCacheMem; addr: kt.ADDRESS): BOOLEAN;
BEGIN
  RETURN kmem.Get(addr, sys.ADR(CacheMem), CacheSize);
END GetCacheMem;

VAR
  Scan: BOOLEAN;

PROCEDURE Get_Special (source: kt.ADDRESS; dest: sys.ADDRESS; len: CARDINAL;
                       cache: BOOLEAN; scan: BOOLEAN): BOOLEAN;
VAR
  rc: BOOLEAN;
BEGIN
  IF len = 0 THEN RETURN FALSE; END;
  IF cache THEN
    Scan := scan;
    rc := Get (source, dest, len);
    Scan := TRUE;
    RETURN rc;
  ELSIF kmem.Get(source, dest, len) THEN
    IF scan THEN
      ScanForBreaks (dest, source,  source+len-1);
    END;
    RETURN TRUE;
  END;
  RETURN FALSE;
END Get_Special;

(*Возращает номер выталкиваемой страницы*)
PROCEDURE GetBadPage() : CARDINAL;
VAR
  i, Page, Min, Time : CARDINAL;
BEGIN
  Time:= 0;
  Page:= 0;
  Min:=  0;
  FOR i:=1 TO MaxPages DO
    IF (Pages[i].CacheMemOk) AND ( (Pages[i].LastTime < Time) OR (Time = 0) )
    THEN Time:= Pages[i].LastTime END;
  END;
  FOR i:=1 TO MaxPages DO
    IF (Pages[i].CacheMemOk) AND
    ( (Pages[i].Calls < Min) OR (Min = 0) ) AND (Pages[i].LastTime = Time)
    THEN Min:= Pages[i].Calls; Page:= i; END;
  END;
  IF Page > 0 THEN INC(FlashedPages); END;
  RETURN Page;
END GetBadPage;

(* Прочитать с адреса в буфер немного байтов, меньше CacheSize*)
PROCEDURE GetFromPage(source: kt.ADDRESS; dest: sys.ADDRESS; len: CARDINAL) : BOOLEAN;
VAR
  FreePage,                (* Номер новой страницы, если она будет создаваться *)
  i, offs                  : CARDINAL;
  addr                     : kt.ADDRESS;
BEGIN
  IF len = 0 THEN RETURN FALSE; END;
  INC(TimeCounter);
  FreePage:= 0;
  offs:= source MOD CacheSize;
  addr:= source - offs;
  FOR i := MaxPages TO 1 BY -1 DO
    WITH Pages[i] DO
      IF CacheMemOk THEN
        IF (source >= CacheAddr) AND (source<=(CacheAddr+(CacheSize-1))) THEN
           IF Scan AND NOT Scanned THEN
             ScanForBreaks(sys.ADR(CacheMem), CacheAddr, CacheAddr+(CacheSize-1));
             Scanned := TRUE;
           END;
           INC(Calls);
           LastTime:= TimeCounter;
           sys.MOVE(sys.ADR(CacheMem[offs]), dest, len);
           RETURN TRUE;
         END;
      ELSE
        FreePage:=i;
      END;
    END;
  END;

  IF FreePage = 0 THEN FreePage:= GetBadPage() END;

  WITH Pages[FreePage] DO
    IF NOT GetCacheMem(CacheMem, addr) THEN RETURN FALSE END;
    CacheAddr:= addr;                             (* Создать новую страницу *)
    CacheMemOk:= TRUE;
    Calls:= 1;                                    (*Кол-во вызовов равно 1*)
    LastTime:= TimeCounter;
    IF Scan THEN
      ScanForBreaks(sys.ADR(CacheMem), CacheAddr, CacheAddr+(CacheSize-1));
    END;
    Scanned := Scan;
    sys.MOVE(sys.ADR(CacheMem[offs]), dest, len);
  END;
  RETURN TRUE;
END GetFromPage;

(* Прочитать с адреса в буфер байты   *)
PROCEDURE Get(source: kt.ADDRESS; dest: sys.ADDRESS; len: CARDINAL): BOOLEAN;
VAR
  count, offs: CARDINAL;
  addr : kt.ADDRESS;
BEGIN
  addr := source - (source MOD CacheSize);
  offs:=source - addr;
  IF len + offs <= CacheSize THEN
    IF NOT GetFromPage(source, dest, len) THEN RETURN FALSE END;
  ELSE
    count:= CacheSize - offs;
    len:= len - count;
    IF NOT GetFromPage(source, dest, count) THEN RETURN FALSE END;
    LOOP
      IF len=0 THEN EXIT END;
      IF source > MAX(CARDINAL)-count THEN RETURN FALSE END;
      IF len > CacheSize THEN
        IF NOT GetFromPage(source + count,sys.ADDADR(dest,count), CacheSize) THEN RETURN FALSE END;
        DEC(len,   CacheSize);
        INC(count, CacheSize);
      ELSE
        IF NOT GetFromPage(source + count,sys.ADDADR(dest,count), len)  THEN RETURN FALSE END;
        EXIT;
      END;
    END;
  END;
  RETURN TRUE;
END Get;


(* Писать на страницу кусочек меньше CacheSize *)
PROCEDURE WriteToPage(dest: kt.ADDRESS; source: sys.ADDRESS; len: CARDINAL);
VAR
  i     :  CARDINAL;
  offs  :  CARDINAL;
  flag  :  BOOLEAN;    (* найдена или нет нужная страница *)
BEGIN
  offs:= dest MOD CacheSize;
  flag:= FALSE;
  i:=0;
  WHILE (i < MaxPages) AND NOT flag DO
    INC(i);
    IF (dest >= Pages[i].CacheAddr) AND ((dest-CacheSize) < Pages[i].CacheAddr ) THEN
      sys.MOVE(source, sys.ADR(Pages[i].CacheMem[offs]), len);
--      UpdateBreaks (sys.ADR(Pages[i].CacheMem[offs]), dest, dest+len-1);
      Pages[i].Scanned := FALSE;
      flag:= TRUE;
    END;
  END;
END WriteToPage;

(* Записать в кэш *)
PROCEDURE WriteToCache(dest: kt.ADDRESS; source: sys.ADDRESS; len: CARDINAL);
VAR
  count, offs: CARDINAL;
BEGIN
  offs:= dest MOD CacheSize;
  IF len + offs <= 4096 THEN
    WriteToPage(dest, source, len);
  ELSE
    count:= CacheSize - offs;
    len:= len - count;
    WriteToPage(dest, source, count);
    LOOP
      IF len=0 THEN EXIT END;
      IF len > CacheSize THEN
        WriteToPage(dest + count, source + count, CacheSize);
        DEC(len,   CacheSize);
        INC(count, CacheSize);
      ELSE
        WriteToPage(dest + count, source + count, len);
        EXIT;
      END;
    END;
  END;
END WriteToCache;


(* Записать по адресу из буфера немного байтов *)
PROCEDURE Put(dest: kt.ADDRESS; source: sys.ADDRESS; len: CARDINAL): BOOLEAN;
VAR
  rc: BOOLEAN;
BEGIN
  rc := kmem.Put(dest, source, len);
  IF rc THEN
    WriteToCache(dest, source, len);
  END;
  RETURN rc;
END Put;

(* Получить текущее значение IP *)
PROCEDURE GetIP (): kt.ADDRESS;
BEGIN
  IF CacheOk THEN
    RETURN RegisterCache.Eip + RegisterCache.BaseCs;
  ELSE
    RETURN 0;
  END;
END GetIP;

PROCEDURE GetFrame (): kt.ADDRESS;
BEGIN
  IF CacheOk THEN
    RETURN RegisterCache.Ebp + RegisterCache.BaseSs;
  ELSE
    RETURN 0;
  END;
END GetFrame;

(* Получить текущее значение SP *)
PROCEDURE GetSP (): kt.ADDRESS;
BEGIN
  IF CacheOk THEN
    RETURN RegisterCache.Esp + RegisterCache.BaseSs;
  ELSE
    RETURN 0;
  END;
END GetSP;

PROCEDURE GetSegmentInfo(    addr  : kt.ADDRESS;
                         VAR begin : kt.ADDRESS;
                         VAR len   : CARDINAL;
                         VAR access: kt.ATTRIBS): BOOLEAN;

VAR
  c, obj: CARDINAL;
BEGIN
  IF tls.FindComObjByAddr(addr, c, obj) THEN
    WITH dt.Components.Components^[c].EI.Objects^[obj] DO
      begin  := Begin;
      len    := End - Begin + 1;
      access := Attributes;
      RETURN TRUE;
    END;
  ELSE
    RETURN kmem.GetSegmentInfo(addr, begin, len, access);
  END;
END GetSegmentInfo;

PROCEDURE IsAddrFromExecutableSeg(addr: kt.ADDRESS): BOOLEAN;

VAR
  c, obj: CARDINAL;
BEGIN
  IF tls.FindComObjByAddr(addr, c, obj) THEN
    WITH dt.Components.Components^[c].EI.Objects^[obj] DO
      RETURN kt.execute IN Attributes;
    END;
  ELSE
    RETURN kmem.IsAddrFromExecutableSeg(addr);
  END;
END IsAddrFromExecutableSeg;

PROCEDURE GetFlags(VAR flags: kt.FLAGS): BOOLEAN;
BEGIN
  IF CacheOk THEN
    WITH flags DO
      C :=  0 IN BITSET(RegisterCache.EFlags);
      P :=  2 IN BITSET(RegisterCache.EFlags);
      A :=  4 IN BITSET(RegisterCache.EFlags);
      Z :=  6 IN BITSET(RegisterCache.EFlags);
      S :=  7 IN BITSET(RegisterCache.EFlags);
      D := 10 IN BITSET(RegisterCache.EFlags);
      O := 11 IN BITSET(RegisterCache.EFlags);
    END;
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END GetFlags;


<* IF DEFINED (xd_debug) & xd_debug THEN *>
VAR
  i: CARDINAL;
<* END *>

BEGIN
  Scan := TRUE;

  CacheOk := FALSE;
  ClearCaches;

  (* Статистика *)
<* IF DEFINED (xd_debug) & xd_debug THEN *>
FINALLY
  IF opt.Debug(opt.InfoCache) THEN
    io.WriteLn;
    io.WriteString("EXE_MEM report :");                                 io.WriteLn;
    io.WriteString("Status of cache pages");                            io.WriteLn;
    UsedPages:= 0;
    FOR i:=1 TO MaxPages DO IF Pages[i].CacheMemOk THEN INC(UsedPages) END; END;
    IF UsedPages > MaxUsedPages THEN MaxUsedPages:= UsedPages END;
    FOR i:=1 TO MaxPages DO
      IF Pages[i].CacheMemOk THEN
        io.WriteString("Num=");       io.WriteCard(i, 5);
        io.WriteString("   Calls=");  io.WriteCard(Pages[i].Calls, 5);
        io.WriteString("   Address=");io.WriteCard(Pages[i].CacheAddr, 12);
        io.WriteString("   Time=");   io.WriteCard(TimeCounter - Pages[i].LastTime, 8);
        io.WriteLn;
      END;
    END;
    io.WriteString("MaxPages    =");     io.WriteCard(MaxPages,     5); io.WriteLn;
    io.WriteString("FlashedPages=");     io.WriteCard(FlashedPages, 5); io.WriteLn;
    io.WriteString("MaxUsedPages=");     io.WriteCard(MaxUsedPages, 5); io.WriteLn;
    io.WriteLn;
  END;
<* END *>

END Exe_Mem.
