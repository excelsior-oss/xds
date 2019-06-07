<* NEW debug- *>


-- Главный модуль компоненты xpdump
-- Визуализатор профилирования программы

MODULE xpDump;

<* Storage+ *>

IMPORT sys := SYSTEM;
IMPORT arg := ProgEnv;
IMPORT fmt := FormStr;
IMPORT ioc := IOChan;
IMPORT seq := SeqFile;
IMPORT prn := Printf;
IMPORT tio := TextIO;

IMPORT xs  := xStr;
IMPORT txt := Texts;
IMPORT fil := File;
IMPORT sor := Sort;
IMPORT i2s := Int2Str;
IMPORT i64 := Int64;
IMPORT opt := Options;

IMPORT pt  := PrfTypes;
IMPORT prf := Profile;

IMPORT tls := DI_Tools;


VAR
  LogFile    : BOOLEAN;       -- Write log file
  LogFileName: xs.String;     -- Log file name
  log_open   : BOOLEAN;       -- Write log file
  log_file   : ioc.ChanId;


PROCEDURE printf (s-: ARRAY OF CHAR; SEQ arg: sys.BYTE);
VAR
  t: xs.String;
BEGIN
  IF log_open THEN
    fmt.print (t, s, arg);
    tio.WriteString (log_file, t);
  ELSE
    prn.printf (s, arg);
  END;
END printf;


CONST
  PRODUCT   = "visualization utility";
  COPYRIGHT = "1997-2001 Excelsior";


PROCEDURE Copyright;
BEGIN
  printf("\n%s, %s, Version %s\n(c) %s\n\n", pt.IDENTKEY, PRODUCT, pt.VERSION, COPYRIGHT);
END Copyright;



TYPE
  BACK_BAR = ( empty, dot, double );
  BACK_BAR_SYMBOL = ARRAY BACK_BAR OF CHAR;


CONST
  BackBarSymbols = BACK_BAR_SYMBOL { CHR(ORD(' '))
                                   , CHR(ORD('.'))
                                   , CHR(ORD(':'))
                                   };

  profiler_data_extension = 'xpt';
  profiler_log_extension  = 'log';


TYPE
  -- Sort order: by percentages or by names
  SORT_ORDER  = ( SortByPercent, SortByNames );

  -- Sort direction: up or down
  SORT_DIRECT = ( SortDown, SortUp );


VAR
  prof_data   : xs.String;       -- Profile data name
  SortDirect  : SORT_DIRECT;     -- Sort direct;
  SortOrder   : SORT_ORDER;      -- Sort order;
  GraphBar    : BOOLEAN;         -- Graph bar indicator
  BackBar     : BACK_BAR;        -- Graph bar background
  OnlyMod     : BOOLEAN;         -- Display modules only
  OnlyCom     : BOOLEAN;         -- Display modules
  ShowSource  : BOOLEAN;         -- Display source
  FullPath    : BOOLEAN;         -- Show full path for module
  PerLevel    : CARDINAL;        -- Percentage level sensitive
  Precision   : xs.String;       -- Precision
  Utility     : pt.UTILITY;      -- utility (execution or memory)
  Quantity    : CARDINAL;        -- Quantity
  Info        : BOOLEAN;         -- Info about current options
  Time        : pt.TIME;         -- Program execution time
  Delimiter   : BOOLEAN;         -- Use delimiter


PROCEDURE Init;
BEGIN
  SortDirect  := SortDown;
  SortOrder   := SortByPercent;
  PerLevel    := 5;
  BackBar     := dot;
  GraphBar    := FALSE;
  OnlyMod     := FALSE;
  OnlyCom     := FALSE;
  ShowSource  := FALSE;
  FullPath    := FALSE;
  Precision   := "%5.1f";
  Info        := FALSE;
  LogFile     := FALSE;
  LogFileName := '';
  log_open    := FALSE;
  Delimiter   := FALSE;
END Init;


PROCEDURE Help;
BEGIN
  printf("Usage:   xpdump [ ('-'|'/') options ] <trace file>[.xpt]\n");
  printf('\nOptions: C               display components only\n');
  printf('\t M               display modules only\n');
  printf('\t S               display module with source\n');
  printf('\t F               display full path\n');
  printf('\t G               display graph bar\n');
  printf('\t I               display information about current settings\n');
  printf('\t R               reverse sort\n');
  printf("\t D               use decimal delimiter\n");
  printf('\t O=<order>       sort order\n');
  printf('\t L=<level>       percent level sensitive\n');
  printf('\t P=<precision>   set precision\n');
  printf('\t W[=<filename>]  write log to file\n');
  printf("\n\t <order> is a sort order: by 'N'ame or 'P'ercent\n");
  printf('\t <level> is percent level\n');
  printf('\t <precision> is precision, 0..3\n\n');
  printf("Default: sort by percentage, %u, precision 1\n", PerLevel);
  printf('\t other options - off\n');
  HALT (0);
END Help;


PROCEDURE Options (opt-: ARRAY OF CHAR);
VAR
  s : xs.String;
  ok: BOOLEAN;
  p : CARDINAL;
BEGIN
  CASE CAP(opt[1]) OF
  | 'R' :
    IF opt[2] # 0C THEN Help; END;
    CASE SortDirect OF
    | SortDown : SortDirect := SortUp;
    | SortUp   : SortDirect := SortDown;
    END;
    IF SortDirect = SortUp THEN
      PerLevel := 100-PerLevel;
    END;

  | 'I' :
    IF opt[2] # 0C THEN Help; END;
    Info := TRUE;

  | 'O' :
    IF opt[2] # '=' THEN Help; END;
    CASE CAP(opt[3]) OF
    | 'N' : SortOrder := SortByNames;
    | 'P' : SortOrder := SortByPercent;
    ELSE
      Help;
    END;
    IF opt[4] # 0C THEN Help; END;

  | 'L' :
    IF opt[2] # '=' THEN Help; END;
    xs.Extract(opt, 3, LENGTH(opt), s);
    PerLevel := xs.StrToCard(s, 10, ok);
    IF NOT ok OR (PerLevel > 100) THEN Help; END;

  | 'G' :
    IF opt[2] # 0C THEN Help; END;
    GraphBar := TRUE;

  | 'D' :
    IF opt[2] # 0C THEN Help; END;
    Delimiter := TRUE;

  | 'C' :
    IF opt[2] # 0C THEN Help; END;
    OnlyCom := TRUE;
    OnlyMod := FALSE;
    ShowSource := FALSE;
    IF SortDirect = SortUp THEN
      PerLevel := 100;
    ELSE
      PerLevel := 0;
    END;

  | 'M' :
    IF opt[2] # 0C THEN Help; END;
    OnlyCom := FALSE;
    OnlyMod := TRUE;
    ShowSource := FALSE;

  | 'S' :
    IF opt[2] # 0C THEN Help; END;
    ShowSource := TRUE;
    OnlyCom := FALSE;
    OnlyMod := FALSE;

  | 'F' :
    IF opt[2] # 0C THEN Help; END;
    FullPath := TRUE;

  | 'P' :
    IF opt[2] # '=' THEN Help; END;
    xs.Extract(opt, 3, LENGTH(opt), s);
    p := xs.StrToCard(s, 10, ok);
    IF NOT ok OR (p >= 4) THEN Help; END;
    fmt.print (Precision, "%%%u.%uf", 4+p, p);

  | 'W' :
    LogFile := TRUE;
    IF opt[2] = '=' THEN
      xs.Extract(opt, 3, LENGTH(opt), LogFileName);
    ELSIF opt[2] # 0C THEN
      Help;
    END;

  ELSE
    Help;
  END;
END Options;


PROCEDURE ParseCommandLine;
VAR
  k, i: CARDINAL;
  a: xs.String;
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
  arg.GetArg(i, prof_data);
END ParseCommandLine;


PROCEDURE PrepareData;
VAR
  res: seq.OpenResults;
BEGIN
  fil.AddExtension (prof_data, profiler_data_extension);
  IF LogFile THEN
    IF LogFileName = '' THEN
      IF prof_data = '' THEN
        COPY ('xpdump', LogFileName);
      ELSE
        COPY (prof_data, LogFileName);
      END;
      fil.ChangeExtension (LogFileName, profiler_log_extension);
    ELSE
      fil.AddExtension (LogFileName, profiler_log_extension);
    END;
    seq.OpenWrite (log_file, LogFileName, seq.write+seq.text+seq.old, res);
    log_open := res = seq.opened;
    IF log_open THEN
      Copyright;
    ELSE
      printf('Can not open log file %s.\n', LogFileName);
      LogFile := FALSE;
   END;
  END;
END PrepareData;



PROCEDURE Separator;
BEGIN
  printf("-------------------------------------------------------------------------------\n");
END Separator;


TYPE
  KEY = POINTER TO ARRAY OF CARDINAL;

  RMODULE = RECORD
              KProcs: KEY;
              -- FOR TRACE CALLS PROFILE DATA ONLY
              MData: pt.PROFDATA;
            END;

  PAMODULES = POINTER TO ARRAY OF RMODULE;

  RCOMPONENT = RECORD
                 KModules: KEY;
                 Modules : PAMODULES;
                 CASE :pt.UTILITY OF
                 | pt.TRACE_EXECUTION, pt.TRACE_MEMORY:
                   KPublics: KEY;
                 | pt.TRACE_CALLS_PROFILE:
                   CData: pt.PROFDATA;
                 END;
               END;

  PACOMPONENTS = POINTER TO ARRAY OF RCOMPONENT;


VAR
  Components : PACOMPONENTS;
  KComponents: KEY;


  PROCEDURE init_key (N: CARDINAL; VAR key: KEY);
  VAR
    i: CARDINAL;
  BEGIN
    IF N = 0 THEN
      key := NIL;
    ELSE
      NEW (key, N);
      FOR i := 0 TO N-1 DO
        key^[i] := i;
      END;
    END;
  END init_key;


  PROCEDURE init_modules * (c-: CARDINAL; N: CARDINAL);
  VAR
    m: CARDINAL;
   <* IF debug THEN *>
    p, n, c1, m1, l1: INTEGER;
    s: xs.txt_ptr;
   <* END *>
  BEGIN
    WITH Components^[c] DO
      init_key (N, KModules);
      IF N = 0 THEN
        Modules := NIL;
      ELSE
        NEW (Modules, N);
        FOR m := 0 TO N-1 DO
          init_key (prf.N_Proc (c, m), Modules^[m].KProcs);
         <* IF debug THEN *>
          s := xs.txt_ptr(prf.ModuleName (c, m));
          printf ("%d.%s\n", c, s^);
          FOR p := 0 TO prf.N_Proc (c, m)-1 DO
            s := xs.txt_ptr(prf.ProcName (c, m, p));
            printf ("   %d.%d.%s\n", c, m, s^);
            FOR n := 0 TO prf.N_Call (c, m, p)-1 DO
              printf ("\t%d: ", prf.CallCount (c, m, p, n));
              IF prf.CallPlace (c, m, p, n, c1, m1, l1) THEN
                s := tls.GetSourceLine (c1, m1+1, l1);
                printf ("%d.%d.%d.%s\n", c1, m1, l1,s^);
              ELSE
                printf ("call place not detected\n");
              END;
            END;
          END;
         <* END *>
        END;
      END;
    END;
  END init_modules;



PROCEDURE ReadProfilerData;


VAR
  LoadResult: INTEGER;
  c: CARDINAL;
  m: INTEGER;
BEGIN
  LoadResult := prf.LoadDebugInfo(prof_data);
  CASE LoadResult OF
  | prf.Error                       : printf("Error detected!\n");                             HALT (1);
  | prf.OpenErrorProfilerData       : printf("Error: can't open profiler data!\n");            HALT (2);
  | prf.ReadErrorProfilerData       : printf("Error: can't read profiler data!\n");            HALT (3);
  | prf.ReadDebugInfo               : printf("Error: can't read debug information!\n");        HALT (4);
  | prf.WrongFormatProfilerData     : printf("Error: wrong format profiler data!\n");          HALT (5);
  | prf.IsNot_XDS_ProfilerTraceFile : printf("Error: File is not XDS profiler trace file!\n"); HALT (6);
  ELSE
    CASE prf.Utility() OF
    | prf.TRACE_EXECUTION:
      Utility := pt.TRACE_EXECUTION;
      Quantity := prf.GetSnapshots();
      IF Quantity = 0 THEN
        printf("Warning: snapshots is zero. Nothing to do!\n");
        HALT (7);
      END;
    | prf.TRACE_MEMORY:
      Utility := pt.TRACE_MEMORY;
      Quantity := prf.GetMemUsed();
      IF Quantity = 0 THEN
        printf("Warning: memory is never used. Nothing to do!\n");
        HALT (8);
      END;
    | prf.TRACE_CALLS_PROFILE:
      Utility := pt.TRACE_CALLS_PROFILE;
      prf.GetExecutionTime (Time);
      IF i64.lss (Time, i64.int (1, 0)) THEN
        printf("Warning: program execution time is zero. Nothing to do!\n");
        HALT (8);
      END;
    ELSE
      printf("Error: unknown file type!\n");
      HALT (9);
    END;
    NEW (Components, LoadResult);
    init_key (LoadResult, KComponents);
    FOR c := 0 TO HIGH(Components^) DO
      WITH Components^[c] DO
        KModules := NIL;
        Modules := NIL;
        m := prf.N_Parts (c);
        CASE Utility OF
        | pt.TRACE_EXECUTION
        , pt.TRACE_MEMORY:
          KPublics := NIL;
          IF m > 0 THEN
            init_modules (c, m);
          ELSIF m < 0 THEN
            init_key (-m, KPublics);
          END;
        | pt.TRACE_CALLS_PROFILE:
          IF m > 0 THEN
            init_modules (c, m);
          END;
        END;
      END;
    END;
  END;
END ReadProfilerData;



PROCEDURE FillString (k: CARDINAL; ch: CHAR; VAR string: ARRAY OF CHAR);
VAR
  i: CARDINAL;
BEGIN
  i := 0;
  WHILE (i < k) AND (i < HIGH(string)) DO
    string[i] := ch;
    INC(i);
  END;
  string[i] := 0C;
END FillString;


PROCEDURE ProgressIndicator (len, max, i: CARDINAL; VAR ind: ARRAY OF CHAR);
VAR
  string: xs.String;
  j, k, n: CARDINAL;
BEGIN
  COPY('', ind);
  IF HIGH(ind)+1 < len THEN RETURN; END;
  IF max < len THEN
    j := VAL(CARDINAL, VAL(REAL, len) / VAL(REAL, max) + 0.5);
    max := max * j;
    i   := i * j;
  END;
  j := (2*len*i) DIV max;
  k := TRUNC(VAL(REAL,j)) DIV 2;
  n := TRUNC(VAL(REAL,j)) MOD 2;
  FillString(k,'#', ind);
  FillString(n,'#', string);
  xs.Append(string, ind);
  FillString(len-k-n,BackBarSymbols[BackBar], string);
  xs.Append(string, ind);
END ProgressIndicator;



PROCEDURE ComName (com: CARDINAL; fullpath: BOOLEAN; VAR name: ARRAY OF CHAR);
VAR
  tmp: xs.txt_ptr;
BEGIN
  tmp := xs.txt_ptr (prf.ComponentName (com));
  IF fullpath THEN
    COPY (tmp^, name);
  ELSE
    fil.ExtractFileName (tmp^, name);
  END;
END ComName;


VAR
  CurrCom: CARDINAL;
  CurrMod: INTEGER;


PROCEDURE CompareComponentsByPercents (i,j: CARDINAL): BOOLEAN;
VAR
  ik, jk: CARDINAL;
  ip, jp: CARDINAL;
  i64p, j64p: i64.INT64;
  data: pt.PROFDATA;
BEGIN
  ik := KComponents^[i];
  jk := KComponents^[j];
  IF Utility = pt.TRACE_CALLS_PROFILE THEN
    data := Components^[ik].CData;
    i64p := i64.int (data.pure_dur_lo, data.pure_dur_hi);
    data := Components^[jk].CData;
    j64p := i64.int (data.pure_dur_lo, data.pure_dur_hi);
    CASE SortDirect OF
    | SortUp   : RETURN i64.gtr (i64p, j64p);
    | SortDown : RETURN i64.lss (i64p, j64p);
    END;
  ELSE
    ip := prf.ComponentSnapshots (ik);
    jp := prf.ComponentSnapshots (jk);
    CASE SortDirect OF
    | SortUp   : RETURN ip > jp;
    | SortDown : RETURN ip < jp;
    END;
  END;
END CompareComponentsByPercents;

PROCEDURE CompareComponentsByNames (i,j: CARDINAL): BOOLEAN;
VAR
  ik, jk: CARDINAL;
  in, jn: xs.String;
BEGIN
  ik := KComponents^[i];
  jk := KComponents^[j];
  ComName (ik, FullPath, in);
  ComName (jk, FullPath, jn);
  CASE SortDirect OF
  | SortUp   : RETURN in < jn;
  | SortDown : RETURN in > jn;
  END;
END CompareComponentsByNames;

PROCEDURE ShakeComponents (i,j: CARDINAL);
VAR
  tmp: CARDINAL;
BEGIN
  tmp := KComponents^[i];
  KComponents^[i] := KComponents^[j];
  KComponents^[j] := tmp;
END ShakeComponents;

PROCEDURE SortComponents;
VAR
  N: CARDINAL;
BEGIN
  N := HIGH (KComponents^)+1;
  CASE SortOrder OF
  | SortByPercent:
    sor.Shell(N, CompareComponentsByPercents, ShakeComponents);
  | SortByNames:
    sor.Shell(N, CompareComponentsByNames,    ShakeComponents);
  END;
END SortComponents;


PROCEDURE CompareModulesByPercents (i,j: CARDINAL): BOOLEAN;
VAR
  ik, jk: CARDINAL;
  ip, jp: CARDINAL;
  i64p, j64p: i64.INT64;
  data: pt.PROFDATA;
BEGIN
  WITH Components^[CurrCom] DO
    IF Utility = pt.TRACE_CALLS_PROFILE THEN
      ik := KModules^[i];
      data := Modules^[ik].MData;
      i64p := i64.int (data.pure_dur_lo, data.pure_dur_hi);
      jk := KModules^[j];
      data := Modules^[jk].MData;
      j64p := i64.int (data.pure_dur_lo, data.pure_dur_hi);
      CASE SortDirect OF
      | SortUp   : RETURN i64.gtr (i64p, j64p);
      | SortDown : RETURN i64.lss (i64p, j64p);
      END;
    ELSE
      ip := prf.ModuleSnapshots (CurrCom, i);
      jp := prf.ModuleSnapshots (CurrCom, j);
      CASE SortDirect OF
      | SortUp   : RETURN ip > jp;
      | SortDown : RETURN ip < jp;
      END;
    END;
  END;
END CompareModulesByPercents;

PROCEDURE CompareModulesByNames (i, j: CARDINAL): BOOLEAN;
VAR
  in, jn: xs.txt_ptr;
BEGIN
  in := xs.txt_ptr (prf.SourceName (CurrCom, i));
  jn := xs.txt_ptr (prf.SourceName (CurrCom, j));
  CASE SortDirect OF
  | SortUp   : RETURN in^ < jn^;
  | SortDown : RETURN in^ > jn^;
  END;
END CompareModulesByNames;

PROCEDURE ShakeModules (i,j: CARDINAL);
VAR
  tmp: CARDINAL;
BEGIN
  WITH Components^[CurrCom] DO
    tmp := KModules^[i];
    KModules^[i] := KModules^[j];
    KModules^[j] := tmp;
  END;
END ShakeModules;

PROCEDURE SortModules;
VAR
  N: CARDINAL;
BEGIN
  N := HIGH(Components^[CurrCom].KModules^)+1;
  CASE SortOrder OF
  | SortByPercent : sor.Shell(N, CompareModulesByPercents, ShakeModules);
  | SortByNames   : sor.Shell(N, CompareModulesByNames, ShakeModules);
  END;
END SortModules;


PROCEDURE ComparePublicsByPercents (i,j: CARDINAL): BOOLEAN;
VAR
  ik, jk: CARDINAL;
  ip, jp: CARDINAL;
BEGIN
  ik := Components^[CurrCom].KPublics^[i];
  ip := prf.PublicSnapshots (CurrCom, ik);
  jk := Components^[CurrCom].KPublics^[j];
  jp := prf.PublicSnapshots (CurrCom, jk);
  CASE SortDirect OF
  | SortUp   : RETURN ip > jp;
  | SortDown : RETURN ip < jp;
  END;
END ComparePublicsByPercents;

PROCEDURE ComparePublicsByNames (i,j: CARDINAL): BOOLEAN;
VAR
  ik, jk: CARDINAL;
  in, jn: xs.txt_ptr;
BEGIN
  ik := Components^[CurrCom].KPublics^[i];
  in := xs.txt_ptr (prf.PublicName (CurrCom, ik));
  jk := Components^[CurrCom].KPublics^[j];
  jn := xs.txt_ptr (prf.PublicName (CurrCom, jk));
  CASE SortDirect OF
  | SortUp   : RETURN in^ < jn^;
  | SortDown : RETURN in^ > jn^;
  END;
END ComparePublicsByNames;

PROCEDURE ShakePublics (i,j: CARDINAL);
VAR
  tmp: CARDINAL;
BEGIN
  WITH Components^[CurrCom] DO
    tmp := KPublics^[i];
    KPublics^[i] := KPublics^[j];
    KPublics^[j] := tmp;
  END;
END ShakePublics;

PROCEDURE SortPublics;
VAR
  N: CARDINAL;
BEGIN
  N := HIGH(Components^[CurrCom].KPublics^)+1;
  CASE SortOrder OF
  | SortByPercent : sor.Shell(N, ComparePublicsByPercents, ShakePublics);
  | SortByNames   : sor.Shell(N, ComparePublicsByNames, ShakePublics);
  END;
END SortPublics;


PROCEDURE CompareProceduresByPercents (i,j: CARDINAL): BOOLEAN;
VAR
  ik, jk: CARDINAL;
  ip, jp: CARDINAL;
  i64p, j64p: i64.INT64;
  idata, jdata: pt.PROFDATA;
BEGIN
  IF Utility = pt.TRACE_CALLS_PROFILE THEN
    ik := Components^[CurrCom].Modules^[CurrMod].KProcs^[i];
    prf.ProcInfo (CurrCom, CurrMod, ik, idata);
    i64p := i64.int (idata.pure_dur_lo, idata.pure_dur_hi);
    jk := Components^[CurrCom].Modules^[CurrMod].KProcs^[j];
    prf.ProcInfo (CurrCom, CurrMod, jk, jdata);
    j64p := i64.int (jdata.pure_dur_lo, jdata.pure_dur_hi);
    CASE SortDirect OF
    | SortUp   : RETURN i64.gtr (i64p, j64p);
    | SortDown : RETURN i64.lss (i64p, j64p);
    END;
  ELSE
    ip := prf.ProcSnapshots (CurrCom, CurrMod, i);
    jp := prf.ProcSnapshots (CurrCom, CurrMod, j);
    CASE SortDirect OF
    | SortUp   : RETURN ip > jp;
    | SortDown : RETURN ip < jp;
    END;
  END;
END CompareProceduresByPercents;


PROCEDURE CompareProceduresByNames (i,j: CARDINAL): BOOLEAN;
VAR
  ik, jk: CARDINAL;
  in, jn: xs.txt_ptr;
BEGIN
  ik := Components^[CurrCom].Modules^[CurrMod].KProcs^[i];
  in := xs.txt_ptr (prf.ProcName (CurrCom, CurrMod, ik));
  jk := Components^[CurrCom].Modules^[CurrMod].KProcs^[j];
  jn := xs.txt_ptr (prf.ProcName (CurrCom, CurrMod, jk));
  CASE SortDirect OF
  | SortUp   : RETURN (in = NIL) OR ((jn # NIL) AND (in^ < jn^));
  | SortDown : RETURN (jn = NIL) OR ((in # NIL) AND (in^ > jn^));
  END;
END CompareProceduresByNames;


PROCEDURE ShakeProcedures (i,j: CARDINAL);
VAR
  tmp: CARDINAL;
BEGIN
  WITH Components^[CurrCom].Modules^[CurrMod] DO
    tmp := KProcs^[i];
    KProcs^[i] := KProcs^[j];
    KProcs^[j] := tmp;
  END;
END ShakeProcedures;


PROCEDURE SortProcedures;
VAR
  N: CARDINAL;
BEGIN
  N := HIGH(Components^[CurrCom].Modules^[CurrMod].KProcs^)+1;
  CASE SortOrder OF
  | SortByPercent : sor.Shell(N, CompareProceduresByPercents, ShakeProcedures);
  | SortByNames   : sor.Shell(N, CompareProceduresByNames, ShakeProcedures);
  END;
END SortProcedures;



CONST
  IND_LEN = 40;


PROCEDURE ProcessProfileData;


  PROCEDURE CalculateTimes (VAR total, overhead: pt.TIME);

    PROCEDURE inc (VAR a: pt.PROFDATA; b: pt.PROFDATA);
    VAR
      tmp1: pt.TIME;
      tmp2: pt.TIME;
    BEGIN
      ---------------------------
      tmp1.high := a.pure_dur_hi;
      tmp1.low  := a.pure_dur_lo;
      tmp2.high := b.pure_dur_hi;
      tmp2.low  := b.pure_dur_lo;
      tmp1 := i64.add (tmp1, tmp2);
      a.pure_dur_hi := tmp1.high;
      a.pure_dur_lo := tmp1.low;
      ---------------------------
(*
      tmp1.high := a.dirty_dur_hi;
      tmp1.low  := a.dirty_dur_lo;
      tmp2.high := b.dirty_dur_hi;
      tmp2.low  := b.dirty_dur_lo;
      tmp1 := i64.add (tmp1, tmp2);
      a.dirty_dur_hi := tmp1.high;
      a.dirty_dur_lo := tmp1.low;
*)
      ---------------------------
      INC (a.total_entry_count, b.total_entry_count);
      INC (a.norec_entry_count, b.norec_entry_count);
    END inc;

  VAR
    tmp  : pt.TIME;
    com  : CARDINAL;
    mod  : CARDINAL;
    proc : CARDINAL;
    data : pt.PROFDATA;
  BEGIN
    total := i64.int (0, 0);
    FOR com := 0 TO HIGH(Components^) DO
      WITH Components^[com] DO
        CData := pt.EMPTY_PROFDATA;
        IF Modules # NIL THEN
          FOR mod := 0 TO HIGH(Modules^) DO
            WITH Modules^[mod] DO
              MData := pt.EMPTY_PROFDATA;
              proc := prf.N_Proc (com, mod);
              FOR proc := 1 TO proc DO
                prf.ProcInfo (com, mod, proc-1, data);
                inc (MData, data);
                tmp := i64.int (data.pure_dur_lo, data.pure_dur_hi);
                total := i64.add (total, tmp);
              END;
              inc (CData, MData);
            END;
          END;
        END;
      END;
    END;
    overhead := i64.sub (Time, total);
  END CalculateTimes;


  PROCEDURE Int64ToStr (i: i64.INT64; VAR s: ARRAY OF CHAR);
  VAR
    p: INTEGER;
  BEGIN
    i2s.IntToStr (FALSE, i.high, i.low, s);
    IF Delimiter THEN
      p := LENGTH(s);
      LOOP
        DEC (p, 3);
        IF p <= 0 THEN EXIT; END;
        xs.Insert ('`', p, s);
      END;
    END;
  END Int64ToStr;


  PROCEDURE tpercent (p, t: pt.TIME): REAL;
  VAR
    tr: REAL;
  BEGIN
    tr := i64.int2real (t);
    IF tr > 0. THEN
      RETURN i64.int2real (p) / tr * 100.0;
    ELSE
      RETURN 0.;
    END;
  END tpercent;


  PROCEDURE data2time (data: pt.PROFDATA): pt.TIME;
  BEGIN
    RETURN i64.int (data.pure_dur_lo, data.pure_dur_hi);
  END data2time;


  PROCEDURE Header;
  VAR
    overhead, total: pt.TIME;
    tmp, string: xs.String;
  BEGIN
    GraphBar := GraphBar AND (Utility # pt.TRACE_CALLS_PROFILE);
    IF Info THEN
      CASE Utility OF
      | pt.TRACE_EXECUTION:
        printf('Samples');
      | pt.TRACE_MEMORY:
        printf('Memory');
      | pt.TRACE_CALLS_PROFILE:
        printf('Ticks');
      END;
      IF SortDirect # SortDown THEN
        printf(', reverse sort by ');
      ELSE
        printf(', sort by ');
      END;
      IF SortOrder = SortByPercent THEN
        printf('percentage');
      ELSE
        printf('name');
      END;
      printf(', %%%u', PerLevel);
      IF OnlyCom THEN printf(', components only'); END;
      IF OnlyMod THEN printf(', modules only'); END;
      IF ShowSource THEN printf(', with source'); END;
      IF FullPath THEN printf(', show full path'); END;
      IF GraphBar THEN printf(', graph bar'); END;
      printf('\n');
      Separator;
    END;
    CASE Utility OF
    | pt.TRACE_EXECUTION:
      printf('Snapshots: %u\n', Quantity);
      Separator;
      IF GraphBar THEN
        printf('Sample Abs%  Rel%  %*c Name\n', IND_LEN, CHR(ORD(' ')));
      ELSE
        printf('Sample Abs%  Rel%  Name\n');
      END;
    | pt.TRACE_MEMORY:
      printf('Memory was allocated %u times\n', Quantity);
      Separator;
      IF GraphBar THEN
        printf('Alloc. Abs%  Rel%  %*c Name\n', IND_LEN, CHR(ORD(' ')));
      ELSE
        printf('Alloc. Abs%  Rel%  Name\n');
      END;
    | pt.TRACE_CALLS_PROFILE:
      Int64ToStr (Time, string);
      printf('Execution time: %s ticks\n', string);
      CalculateTimes (total, overhead);
      Int64ToStr (overhead, string);
      fmt.print (tmp, 'Profiler and library overhead: %%s ticks, %s%%\n', Precision);
      printf (tmp, string, tpercent (overhead, Time));
      -- а теперь нужно в качестве времени работы программы взять время
      -- чистое время работы процедур, без времени работы профилировщика
      Time := total;
      Separator;
      printf('     Own ticks    Total ticks Abs% Rel%     Calls Recursions  Name\n');
    END;
    Separator;
  END Header;



  PROCEDURE print1 (tab: CARDINAL; samples: CARDINAL; pnAbs, pnRel: REAL; string-: ARRAY OF CHAR);
  VAR
    fmt1, fmt2, ind: xs.String;
  BEGIN
    fmt.print (fmt2, "%*c%%s", tab, CHR(ORD(' ')));
    IF GraphBar THEN
      ProgressIndicator (IND_LEN, 100, VAL(CARDINAL, pnAbs), ind);
      fmt.print (fmt1, '%4u  %s %s %%s %s\n', samples, Precision, Precision, fmt2);
      printf (fmt1, pnRel, pnAbs, ind, string);
    ELSE
      fmt.print (fmt1, '%4u  %-3s %-3s %s\n', samples, Precision, Precision, fmt2);
      printf(fmt1, pnAbs, pnRel, string);
    END;
  END print1;


  PROCEDURE print2 (tab: CARDINAL; data: pt.PROFDATA; pnAbs, pnRel: REAL; string-: ARRAY OF CHAR);
  VAR
    fmt1, fmt2: xs.String;
    pure, dirty: xs.String;
  BEGIN
    fmt.print (fmt2, "    %*c%%s", tab, CHR(32));
    WITH data DO
      Int64ToStr (i64.int (pure_dur_lo, pure_dur_hi), pure);
      Int64ToStr (i64.int (dirty_dur_lo, dirty_dur_hi), dirty);
      fmt.print (fmt1, '%%14s %%14s%-3s%-3s %%9d %%5d %s\n', Precision, Precision, fmt2);
      printf (fmt1, pure, dirty, pnAbs, pnRel, total_entry_count, total_entry_count-norec_entry_count, string);
    END;
  END print2;


  PROCEDURE ShowIt (percent: REAL): BOOLEAN;
  VAR
    tmp : CARDINAL;
    up  : BOOLEAN;
    down: BOOLEAN;
  BEGIN
    tmp := VAL(CARDINAL, percent);
    up := (SortDirect = SortUp) AND (tmp <= PerLevel);
    down := (SortDirect = SortDown) AND (tmp >= PerLevel);
    RETURN up OR down;
  END ShowIt;


  PROCEDURE percent (part, total: CARDINAL): REAL;
  BEGIN
    RETURN VAL (REAL, part) / VAL (REAL, total) * 100.;
  END percent;


VAR
  icom, com   : CARDINAL;
  imod, mod   : CARDINAL;
  iproc, proc : CARDINAL;
  ipub, pub   : CARDINAL;
  com_samples : CARDINAL;
  mod_samples : CARDINAL;
  proc_samples: CARDINAL;
  line_samples: CARDINAL;
  pub_samples : CARDINAL;
  unk_samples : CARDINAL;
  pnRel       : REAL;
  pnAbs       : REAL;
  ready       : BOOLEAN;
  str1        : xs.String;
  str2        : xs.txt_ptr;
  mod_source  : xs.txt_ptr;
  bline       : INTEGER;
  eline       : INTEGER;
  line        : INTEGER;
  text        : txt.TEXT;
  pub_addr    : CARDINAL;
  pub_len     : CARDINAL;
  res         : pt.MEM_TRACE_RESULT;
  com_data    : pt.PROFDATA;
  mod_data    : pt.PROFDATA;
  proc_data   : pt.PROFDATA;
  line_data   : pt.PROFDATA;


BEGIN
  com_samples  := 0;
  mod_samples  := 0;
  proc_samples := 0;
  line_samples := 0;
  Header;
  SortComponents;
  FOR icom := 0 TO HIGH(KComponents^) DO
    com := KComponents^[icom];
    CurrCom := com;
    WITH Components^[com] DO
      IF Utility = pt.TRACE_CALLS_PROFILE THEN
        com_data := CData;
        pnAbs := tpercent (data2time(com_data), Time);
        pnRel := pnAbs;
        ready := i64.gtr (i64.int (com_data.pure_dur_lo, com_data.pure_dur_hi), i64.int (0, 0));
      ELSE
        com_samples := prf.ComponentSnapshots (com);
        ready := com_samples # 0;
        pnAbs := percent (com_samples, Quantity);
        pnRel := pnAbs;
      END;
      IF ShowIt (pnAbs) THEN
        ComName (com, FullPath, str1);
        IF Utility = pt.TRACE_CALLS_PROFILE THEN
          print2 (1, com_data, pnAbs, pnRel, str1);
        ELSE
          print1 (1, com_samples, pnAbs, pnRel, str1);
        END;
        IF ready AND NOT OnlyCom THEN
          IF KModules # NIL THEN
            SortModules;
            FOR imod := 0 TO HIGH(KModules^) DO
              mod := KModules^[imod];
              CurrMod := mod;
              WITH Modules^[mod] DO
                IF Utility = pt.TRACE_CALLS_PROFILE THEN
                  mod_data := MData;
                  pnAbs := tpercent (data2time(mod_data), Time);
                  pnRel := tpercent (data2time(mod_data), data2time(com_data));
                  ready := TRUE; -- FIX ME
                ELSE
                  mod_samples := prf.ModuleSnapshots (com, mod);
                  ready := mod_samples # 0;
                  pnAbs := percent (mod_samples, Quantity);
                  pnRel := percent (mod_samples, com_samples);
                END;
                IF ready AND ShowIt (pnAbs) THEN
                  mod_source := xs.txt_ptr (prf.SourceName (com, mod));
                  IF FullPath THEN
                    str2 := mod_source;
                  ELSE
                    str2 := xs.txt_ptr (prf.ModuleName (com, mod));
                  END;
                  IF Utility = pt.TRACE_CALLS_PROFILE THEN
                    print2 (3, mod_data, pnAbs, pnRel, str2^);
                  ELSE
                    print1 (3, mod_samples, pnAbs, pnRel, str2^);
                  END;
                  IF NOT OnlyMod AND (KProcs # NIL) THEN
                    SortProcedures;
                    FOR iproc := 0 TO HIGH(KProcs^) DO
                      proc := KProcs^[iproc];
                      IF Utility = pt.TRACE_CALLS_PROFILE THEN
                        prf.ProcInfo (com, mod, proc, proc_data); -- FIX ME
                        pnAbs := tpercent (data2time(proc_data), Time);
                        pnRel := tpercent (data2time(proc_data), data2time(mod_data));
                        ready := TRUE; -- FIX ME
                      ELSE
                        proc_samples := prf.ProcSnapshots (com, mod, proc);
                        ready := proc_samples # 0;
                        pnAbs := percent (proc_samples, Quantity);
                        pnRel := percent (proc_samples, mod_samples);
                      END;
                      IF ready AND ShowIt (pnAbs) THEN
                        str2 := xs.txt_ptr (prf.ProcName (com, mod, proc));
                        IF Utility = pt.TRACE_CALLS_PROFILE THEN
                          print2 (5, proc_data, pnAbs, pnRel, str2^);
                        ELSE
                          print1 (5, proc_samples, pnAbs, pnRel, str2^);
                        END;
                        IF ShowSource THEN
                          txt.Open (text, mod_source^);
                          IF text # txt.nil THEN
                            IF prf.ProcBounds (com, mod, proc, bline, eline) THEN
                              FOR line := bline TO eline DO
                                IF Utility = pt.TRACE_CALLS_PROFILE THEN
                                  line_data := pt.EMPTY_PROFDATA;; -- MData; -- FIX ME
                                  pnAbs := 0.0;                    -- tpercent (data2time(line_data), Time);
                                  pnRel := 0.0;                    -- tpercent (data2time(line_data), data2time(mod_data));
                                ELSE
                                  line_samples := prf.LineSnapshots (com, mod, line);
                                  pnAbs := percent (line_samples, Quantity);
                                  pnRel := percent (line_samples, mod_samples);
                                END;
                                txt.GetLine (text, line, str2);
                                IF Utility = pt.TRACE_CALLS_PROFILE THEN
                                  printf ("%s\n", str2^);
                                ELSE
                                  print1 (7, line_samples, pnAbs, pnRel, str2^);
                                END;
                              END;
                              Separator;
                            END;
                            txt.Close (text);
                          END;
                        END;
                      END;
                    END;
                  END;
                END;
              END;
            END;
          ELSIF KPublics # NIL THEN
            SortPublics;
            FOR ipub := 0 TO HIGH(KPublics^) DO
              pub := KPublics^[ipub];
              pub_samples := prf.PublicSnapshots (com, pub);
              ready := pub_samples # 0;
              pnAbs := percent (pub_samples, Quantity);
              pnRel := percent (pub_samples, com_samples);
              IF ready AND ShowIt (pnAbs) THEN
                str2 := xs.txt_ptr (prf.PublicName (com, pub));
                prf.PublicAttr (com, pub, pub_addr, pub_len);
                fmt.print (str1, '%-30s %$8X-%$8X', str2^, pub_addr, pub_addr+pub_len-1);
                print1 (3, pub_samples, pnAbs, pnRel, str1);
              END;
            END;
          END;
          unk_samples := prf.GetUnknownParts (com);
          ready := unk_samples # 0;
          IF ready THEN
            pnAbs := percent (unk_samples, Quantity);
            pnRel := percent (unk_samples, com_samples);
            IF ShowIt (pnAbs) THEN
              print1 (3, unk_samples, pnAbs, pnRel, 'Unknown');
            END;
          END;
        END;
      END;
    END;
  END;
  Separator;
  IF Utility = pt.TRACE_MEMORY THEN
    IF prf.GetMemTraceResults (res) THEN
      WITH res DO
        printf ("Results: memory allocate %u bytes %u times\n", alloc_mem, alloc_count);
        printf ("         total memory allocate max. %u bytes\n", alloc_max_total);
        printf ("         one time memory allocate max. %u bytes\n", alloc_max_size);
        printf ("         memory deallocate %u bytes %u times\n", dealloc_mem, dealloc_count);
      END;
    ELSE
      printf ("Error: can not make results\n");
      HALT (10);
    END;
    Separator;
  END;
END ProcessProfileData;


BEGIN
  Copyright;
  Init;
  ParseCommandLine;
  PrepareData;
  ReadProfilerData;
  ProcessProfileData;
  HALT (0);
END xpDump.
