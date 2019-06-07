-- Главный модуль компоненты xpd
-- Визуализатор профилирования программы
<* NEW genprof-*>
MODULE xpD;

<* Storage+ *>

IMPORT SeqFile;
IMPORT DStrings;
IMPORT xmRTS;
IMPORT arg := ProgEnv;
IMPORT WholeIO;
IMPORT TextIO;
IMPORT Str;
IMPORT Printf;
IMPORT SYSTEM;
IMPORT Sort;


IMPORT fmt := FormStr;
IMPORT ioc := IOChan;
IMPORT tio := TextIO;

IMPORT xStr;
IMPORT txt := Texts;
IMPORT fil := File;

CONST START_END_PROC_OVERHEAD = 55;
 
TYPE
    PROCINFO = RECORD
      name: DStrings.String;
      str: xmRTS.X2C_Profile_STR;
    END;

    ModuleInfo = RECORD
        name: DStrings.String;
        proc: POINTER TO ARRAY OF PROCINFO;
        modinfo : PROCINFO;
    END;

    int64 = xmRTS.X2C_int64;
    DataType = POINTER TO ARRAY OF POINTER TO PROCINFO;

VAR
    prof_file: xStr.String;
    info: POINTER TO ARRAY OF ModuleInfo;
    procs, mods: DataType;
    modNum, procNum : LONGINT;
    total, procTime, profTime : int64;
    write_modules, write_header, write_pure, write_dirty:BOOLEAN;
    comma, delimiter : CHAR;

-------------------------------------------------------------------------
VAR
    sortdata: DataType;
    sortNum: LONGINT;

PROCEDURE CMPDirty(a,b:SYSTEM.CARD32) : BOOLEAN;
BEGIN
    IF   sortdata[a].str.dirty_dur.high = sortdata[b].str.dirty_dur.high THEN
         RETURN sortdata[a].str.dirty_dur.low < sortdata[b].str.dirty_dur.low;
    ELSE RETURN sortdata[a].str.dirty_dur.high < sortdata[b].str.dirty_dur.high

    END;
END CMPDirty;

PROCEDURE CMPPure(a,b:SYSTEM.CARD32) : BOOLEAN;
BEGIN
    IF   sortdata[a].str.pure_dur.high = sortdata[b].str.pure_dur.high THEN
         RETURN sortdata[a].str.pure_dur.low < sortdata[b].str.pure_dur.low;
    ELSE RETURN sortdata[a].str.pure_dur.high < sortdata[b].str.pure_dur.high

    END;
END CMPPure;

PROCEDURE Shake(a,b:SYSTEM.CARD32);
VAR foo : POINTER TO PROCINFO;
BEGIN
    foo := sortdata[a];
    sortdata[a] := sortdata[b];
    sortdata[b] := foo;
END Shake;

PROCEDURE ParseProfilerData;
VAR i,j : INTEGER;
BEGIN
-- overhead
  FOR i := 0 TO procNum-1 DO
    FOR j := 0 TO START_END_PROC_OVERHEAD-1 DO
      procs[i].str.pure_dur := SYSTEM.VAL( int64, X2J_SUB64(
                                    procs[i].str.pure_dur.low,
                                    procs[i].str.pure_dur.high,
                                    procs[i].str.total_entry_count,
                                    0 ));
    END;
  END;

  FOR i := 0 TO modNum-1 DO
    FOR j := 0 TO LEN(info[i].proc^)-1 DO
      mods[i].str.pure_dur := SYSTEM.VAL( int64, X2J_ADD64( mods[i].str.pure_dur.low, mods[i].str.pure_dur.high,
                               info[i].proc[j].str.pure_dur.low, info[i].proc[j].str.pure_dur.high ));

    END;
  END;

  IF ~write_modules THEN
      sortdata := procs;
      sortNum := procNum;
  ELSE
      sortdata := mods;
      sortNum := modNum;
  END;

  IF write_pure THEN
      Sort.Shell (sortNum, CMPPure, Shake);
  ELSE
      Sort.Shell (sortNum, CMPDirty, Shake);
  END;
  procTime.low :=0 ;
  procTime.high :=0 ;
  FOR i := 0 TO procNum-1 DO
      procTime := SYSTEM.VAL( int64, X2J_ADD64( procTime.low, procTime.high,
                                   procs[i].str.pure_dur.low, procs[i].str.pure_dur.high ));
  END;
  profTime := SYSTEM.VAL( int64, X2J_SUB64( total.low, total.high,
                                            procTime.low, procTime.high ));

END ParseProfilerData;
-----------------------------------------------------------------

PROCEDURE PrintInt64 ( foo: int64 );
VAR buf: ARRAY 15 OF CHAR;
    inx: INTEGER;
    res: int64;
BEGIN
    FOR inx := 0 TO LEN(buf)-3 DO buf[inx] := " "; END;
    buf[LEN(buf)-1] := 0X;
    buf[LEN(buf)-2] := "0";
    IF ( SYSTEM.VAL(LONGINT,foo.high) < 0 ) THEN
        buf[LEN(buf)-2] := "?";
        Printf.printf("%s %c", buf, delimiter);
        RETURN;
    END;

    inx := LEN(buf)-2;
    WHILE (foo.low # 0) OR ( foo.high # 0) DO
        res := SYSTEM.VAL( int64, X2J_REM64( foo.low, foo.high, 10, 0 ));
        foo := SYSTEM.VAL( int64, X2J_DIV64( foo.low, foo.high, 10, 0 ));
        buf[inx] := CHR (res.low MOD 10 + 48);
        DEC(inx);
    END;
    Printf.printf("%s %c", buf, delimiter);
END PrintInt64;

PROCEDURE PrintPerCent ( foo, total: int64 );
VAR buf: ARRAY 7 OF CHAR;
    inx: INTEGER;
    res: int64;
    tmp: SYSTEM.CARD32;
BEGIN
    FOR inx := 0 TO LEN(buf)-1 DO buf[inx] := " "; END;
    buf[6] := 0X;
    buf[2] := comma;
    IF ( SYSTEM.VAL(LONGINT,foo.high) < 0 ) THEN
        buf[5] := "?";
        Printf.printf("%s %c", buf, delimiter);
        RETURN;
    END;

    res := SYSTEM.VAL(int64, X2J_MUL64(
                    foo.low, foo.high,
                    100000, 0 ));
    res := SYSTEM.VAL(int64, X2J_DIV64(
                    res.low, res.high,
                    total.low, total.high ));

--    FOR inx := 0 TO LEN(buf)-3 DO buf[inx] := " "; END;
--    inx := LEN(buf)-3;

    tmp := res.low;

    IF tmp = 100000 THEN
        Printf.printf("100%c00 %c", comma, delimiter );
        RETURN;
    END;
    IF tmp MOD 10 # 0 THEN
       buf[5] := CHR (tmp MOD 10 + 48);
    ELSE
       buf[5] := "0";
    END;
    tmp := tmp DIV 10;

    IF tmp MOD 10 # 0 THEN
       buf[4] := CHR (tmp MOD 10 + 48);
    ELSE
       buf[4] := "0";
    END;
    tmp := tmp DIV 10;

    IF tmp MOD 10 # 0 THEN
       buf[3] := CHR (tmp MOD 10 + 48);
    ELSE
       buf[3] := "0";
    END;
    tmp := tmp DIV 10;

    IF tmp MOD 10 # 0 THEN
       buf[1] := CHR (tmp MOD 10 + 48);
    ELSE
--       IF tmp DIV 10 # 0 THEN
           buf[1] := "0";
--        ELSE
--           buf[1] := " ";
--        END;
    END;
    tmp := tmp DIV 10;

    IF tmp # 0 THEN
       buf[0] := CHR (tmp + 48);
    ELSE
       buf[0] := " ";
    END;

    Printf.printf("%s %c", buf, delimiter);
END PrintPerCent;

PROCEDURE WriteProfilerData;
VAR i : INTEGER;
    foo: int64;
    data: DataType;
BEGIN
    IF ~write_modules THEN
        data := procs;
    ELSE
        data := mods;
    END;
    IF write_header THEN
        Printf.printf("MODULES %d\n", modNum);
        Printf.printf("PROCS   %d\n", procNum);

        Printf.printf("TOTAL TIME");
        PrintInt64( total );
        Printf.printf("\n");

        Printf.printf("REAL  TIME");
        PrintInt64( procTime );
        Printf.printf("\n");

        Printf.printf("PROFILER AND LIBRARY OVERHEAD ");
        PrintPerCent( profTime, total );
        Printf.printf("\n");
    END;

    FOR i := 0 TO LEN(data^)-1 DO
        IF write_pure THEN
            PrintInt64( data[i].str.pure_dur );
            PrintPerCent( data[i].str.pure_dur, procTime );

            IF data[i].str.total_entry_count # 0 THEN
                foo := SYSTEM.VAL( int64, X2J_DIV64( data[i].str.pure_dur.low,
                                                     data[i].str.pure_dur.high,
                                                     data[i].str.total_entry_count,
                                                     0 ));
                PrintInt64( foo );
            ELSE
                Printf.printf("             0 %c", delimiter);
            END;
            Printf.printf(" %9d %c", data[i].str.total_entry_count, delimiter);
        END;

        IF write_dirty & ~write_modules THEN
            PrintInt64( data[i].str.dirty_dur );
            PrintPerCent( data[i].str.dirty_dur, total );

            IF data[i].str.norec_entry_count # 0 THEN
                foo := SYSTEM.VAL( int64, X2J_DIV64( data[i].str.dirty_dur.low,
                                                     data[i].str.dirty_dur.high,
                                                     data[i].str.norec_entry_count,
                                                     0 ));
                PrintInt64( foo );
            ELSE
                Printf.printf("             0 %c", delimiter);
            END;
            Printf.printf(" %9d %c", data[i].str.norec_entry_count, delimiter);
        END;

        Printf.printf('"%s"\n',data[i].name^);

    END;
END WriteProfilerData;

PROCEDURE Help;
BEGIN
  Printf.printf("Usage:   xpd [ ('-'|'/') options ] <trace file.prf>\n");
  Printf.printf('\nOptions: ,               use comma as decimal delimiter\n');
  Printf.printf('\t h               display header\n');
  Printf.printf('\t p               display only pure  figures\n');
  Printf.printf('\t d               display only dirty figures\n');
  Printf.printf('\t m               display module figures\n');
--  Printf.printf('\t I               display information about current settings\n');
--  Printf.printf('\t R               reverse sort\n');
--  Printf.printf('\t O=<order>       sort order\n');
--  Printf.printf('\t L=<level>       percent level sensitive\n');
--  Printf.printf('\t P=<precision>   set precision\n');
--  Printf.printf('\t W[=<filename>]  write log to file\n');
--  Printf.printf("\n\t <order> is a sort order: by 'N'ame or 'P'ercent\n");
--  Printf.printf('\t <level> is percent level\n');
--  Printf.printf('\t <precision> is precision, 0..3\n\n');
--  Printf.printf('\t other options - off\n');
  HALT (0);
END Help;

PROCEDURE Option (opt-: ARRAY OF CHAR; number: INTEGER );
BEGIN
  CASE CAP(opt[1]) OF
  | 'H' :
    write_header := TRUE;
  | 'P' :
    write_dirty := FALSE;
  | 'D' :
    write_pure := FALSE;
  | 'M' :
    write_modules := TRUE;
--    delimiter := ';';
(*    IF opt[2] # 0C THEN Help; END;
    CASE SortDirect OF
    | SortDown : SortDirect := SortUp;
    | SortUp   : SortDirect := SortDown;
    END;
    IF SortDirect = SortUp THEN
      PerLevel := 100-PerLevel;
    END;
*)
  | ',' :
    comma := ",";
  ELSE
    Help;
  END;
--  Printf.printf("option %d %s\n", number, opt);

END Option;


PROCEDURE ParseCommandLine;
VAR
  k, i: SYSTEM.CARD32;
  a: xStr.String;
  count: INTEGER;
CONST MAXOPTIONSCOUNT = 4;
BEGIN
  write_header := FALSE;
  write_modules := FALSE;
  write_dirty := TRUE;
  write_pure := TRUE;
  comma := ".";
  delimiter := ";";
  k := arg.ArgNumber();
  IF k = 0 THEN Help; END;
  i := 0;
  count := 1;
  LOOP
    IF (i = k) THEN EXIT; END;
    arg.GetArg(i, a);
    IF (a[0] = '/') OR (a[0] = '-') THEN
      IF count = MAXOPTIONSCOUNT THEN Help; END;
      Option( a, count );
      INC(count);
    ELSE
      arg.GetArg(i, prof_file);
    END;
    i := i+1;
  END;
  IF prof_file = "" THEN Help; END;
END ParseCommandLine;

BEGIN
  ParseCommandLine;
  ReadProfilerData;
  ParseProfilerData;
  WriteProfilerData;

END xpD.

