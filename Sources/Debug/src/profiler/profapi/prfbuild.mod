<* +storage *>

IMPLEMENTATION MODULE PrfBuild;

IMPORT sys := SYSTEM;
IMPORT ioc := IOChan;
IMPORT rf  := RndFile;
IMPORT rio := RawIO;
IMPORT xfp := xFilePos;
IMPORT fmt := FormStr;
IMPORT prn := Printf;
IMPORT str := Strings;

IMPORT kt  := KrnTypes;
IMPORT dt  := DI_Types;
IMPORT pt  := PrfTypes;

IMPORT xs  := xStr;
IMPORT fil := File;
IMPORT txt := Texts;
IMPORT i64 := Int64;




PROCEDURE AddTableComp (begin, end: kt.ADDRESS; name-: ARRAY OF CHAR; index: CARDINAL);
VAR
  tmp: pt.PACOMP;
BEGIN
  WITH TableComps DO
    IF Comps = NIL THEN
      NEW(Comps, 8);
      Count := 0;
    ELSIF Count = HIGH(Comps^) THEN
      NEW(tmp, 2*(HIGH(Comps^)+1));
      sys.MOVE(sys.ADR(Comps^), sys.ADR(tmp^), SIZE(Comps^));
      DISPOSE(Comps);
      Comps := tmp;
    END;
    WITH Comps^[Count] DO
      COPY (name, Name);
      Index := index;
      Begin := begin;
      End := end;
    END;
    INC(Count);
  END;
END AddTableComp;



PROCEDURE AddTableSnapshot (snapshot: pt.SNAPSHOT);
VAR
  tmp: pt.PASNAPSHOT;
BEGIN
  WITH TableSnapshots DO
    IF Snapshots = NIL THEN
      NEW(Snapshots, 1024);
      Count := 0;
    ELSIF Count = HIGH(Snapshots^) THEN
      NEW(tmp, 2*(HIGH(Snapshots^)+1));
      sys.MOVE(sys.ADR(Snapshots^), sys.ADR(tmp^), SIZE(Snapshots^));
      DISPOSE(Snapshots);
      Snapshots := tmp;
    END;
    Snapshots^[Count] := snapshot;
    INC(Count);
  END;
END AddTableSnapshot;


PROCEDURE AddTableMemUsed (mem_used: pt.MEM_USED);
VAR
  tmp: pt.PAMEM_USED;
BEGIN
  WITH TableMemUsed DO
    IF MemUsed = NIL THEN
      NEW(MemUsed, 1024);
      Count := 0;
    ELSIF Count = HIGH(MemUsed^) THEN
      NEW(tmp, 2*(HIGH(MemUsed^)+1));
      sys.MOVE(sys.ADR(MemUsed^), sys.ADR(tmp^), SIZE(MemUsed^));
      DISPOSE(MemUsed);
      MemUsed := tmp;
    END;
    MemUsed^[Count] := mem_used;
    INC(Count);
  END;
END AddTableMemUsed;



PROCEDURE AddTableCProfile (module: pt.TMODULE);
VAR
  tmp: pt.PTMODULE;
BEGIN
  WITH TableCProfile DO
    IF Modules = NIL THEN
      NEW (Modules, 8);
      Count := 0;
    ELSIF Count = HIGH(Modules^) THEN
      NEW(tmp, 2*(HIGH(Modules^)+1));
      sys.MOVE (sys.ADR(Modules^), sys.ADR(tmp^), SIZE(Modules^));
      DISPOSE(Modules);
      Modules := tmp;
    END;
    Modules^[Count] := module;
    INC(Count);
  END;
END AddTableCProfile;


PROCEDURE AddTableCProfileProc (mod: CARDINAL; proc: pt.TPROC);
VAR
  tmp: pt.PTPROC;
BEGIN
  WITH TableCProfile.Modules^[mod].Procs DO
    IF Procs = NIL THEN
      NEW (Procs, 8);
      Count := 0;
    ELSIF Count = HIGH(Procs^) THEN
      NEW(tmp, 2*(HIGH(Procs^)+1));
      sys.MOVE (sys.ADR(Procs^), sys.ADR(tmp^), SIZE(Procs^));
      DISPOSE(Procs);
      Procs := tmp;
    END;
    Procs^[Count] := proc;
    INC(Count);
  END;
END AddTableCProfileProc;


PROCEDURE AddCProfileCall (mod, proc: CARDINAL; call: pt.TCALL);
VAR
  tmp: pt.PTCALL;
  i: CARDINAL;
BEGIN
  WITH TableCProfile.Modules^[mod].Procs.Procs^[proc].Calls DO
    IF Calls = NIL THEN
      NEW (Calls, 2);
      Count := 0;
    ELSIF Count = HIGH(Calls^) THEN
      NEW(tmp, 2*(HIGH(Calls^)+1));
      sys.MOVE (sys.ADR(Calls^), sys.ADR(tmp^), SIZE(Calls^));
      DISPOSE(Calls);
      Calls := tmp;
    END;
    i := 0;
    WHILE i < Count DO
      IF Calls^[i].Addr = call.Addr THEN
        INC (Calls^[i].Num, call.Num);
        RETURN;
      END;
      INC (i);
    END;
    Calls^[Count] := call;
    INC(Count);
  END;
END AddCProfileCall;


PROCEDURE GetInternalIndex (inx: dt.INDEX): dt.INDEX;
VAR
  i: CARDINAL;
BEGIN
  WITH TableComps DO
    IF Count > 0 THEN
      FOR i := 0 TO Count-1 DO
        IF inx = Comps^[i].Index THEN
          RETURN i;
        END;
      END;
    END;
  END;
  RETURN MAX(pt.INDEX);
END GetInternalIndex;



PROCEDURE WriteProtocol (VAR name: ARRAY OF CHAR; util: pt.UTILITY): INTEGER;
VAR
  pro: ioc.ChanId;
  res: rf.OpenResults;
  i  : CARDINAL;
  key: xs.String;
  len: sys.CARD16;
BEGIN
  fil.ChangeExtension (name, pt.TRACE_FILE_EXT);
  rf.OpenClean (pro, name, rf.write+rf.raw+rf.old, res);
  IF res # rf.opened THEN
    RETURN pt.ErrorWriteProtocol;
  END;
  fmt.print (key, "%s, %s", pt.IDENTKEY, pt.VERSION);
  ioc.RawWrite (pro, sys.ADR(key), LENGTH(key)+1);

  WITH TableComps DO
    rio.Write (pro, Count);
    FOR i := 0 TO Count-1 DO
      rio.Write (pro, Comps^[i].Index);
    END;
    len := 0;
    FOR i := 0 TO Count-1 DO
      INC(len, LENGTH(Comps^[i].Name)+1);
    END;
    rio.Write (pro, len);
    FOR i := 0 TO Count-1 DO
      ioc.RawWrite (pro, sys.ADR(Comps^[i].Name), LENGTH(Comps^[i].Name)+1);
    END;
  END;

  rio.Write (pro, util);
  CASE util OF
  | pt.TRACE_EXECUTION:
    WITH TableSnapshots DO
      rio.Write (pro, Count);
      IF Count > 0 THEN
        FOR i := 0 TO Count-1 DO
          rio.Write (pro, Snapshots^[i]);
        END;
      END;
    END;
  | pt.TRACE_MEMORY:
    WITH TableMemUsed DO
      rio.Write (pro, Count);
      IF Count > 0 THEN
        FOR i := 0 TO Count-1 DO
          rio.Write (pro, MemUsed^[i]);
        END;
      END;
    END;
    rio.Write (pro, MemTraceResults);
  END;

  rf.Close(pro);
  RETURN 0;
END WriteProtocol;



PROCEDURE ReadProtocol (name-: ARRAY OF CHAR; VAR util: pt.UTILITY): INTEGER;

  CONST
    tCOMPONENT = "COM";
    tMODULE    = "M";
    tPROCEDURE = "P";
    tCALL      = "C";
    tTIME      = "PROGRAM EXECUTION TIME";
    tMODE      = "MODE";
    tMODE_MIN  = "MIN";
    tMODE_STD  = "STANDARD";
    tMODE_FULL = "FULL";


  PROCEDURE ReadProtocolCallsProfiling (name-: ARRAY OF CHAR; VAR util: pt.UTILITY): INTEGER;
  VAR
    text: txt.TEXT;

    MODULE OpenClose;

    IMPORT txt, text, name;

    BEGIN
      txt.Open (text, name);
    FINALLY
      IF text # txt.nil THEN
        txt.Close (text);
      END;
    END OpenClose;

  VAR
    line: xs.txt_ptr;
    ln  : CARDINAL;
    tmp : pt.NAME;


    PROCEDURE get_param (n: CARDINAL; VAR tmp: ARRAY OF CHAR);
    BEGIN
      xs.ItemS (tmp, line^, " ", n-1);
      ASSERT(tmp # "");
    END get_param;

    VAR
      startIndex: CARDINAL;
      patternFound: BOOLEAN;
      posOfPattern: CARDINAL;

    PROCEDURE get_rest_of_params (n: CARDINAL; VAR tmp: ARRAY OF CHAR);
    BEGIN
      posOfPattern := 0;
      REPEAT
        startIndex := posOfPattern;
        str.FindNext (" ", line^, startIndex, patternFound, posOfPattern);
        IF patternFound THEN
          INC (posOfPattern);
        END;
        DEC (n);
      UNTIL (n = 0) OR NOT patternFound;
      ASSERT (n = 0);
      xs.Extract (line^, startIndex, LENGTH (line^), tmp);
      ASSERT(tmp # "");
    END get_rest_of_params;

  (* Looks forward for next occurrence of pattern in stringToSearch, starting the search at
     position startIndex. If startIndex < LENGTH(stringToSearch) and pattern is found,
     patternFound is returned as TRUE, and posOfPattern contains the start position in
     stringToSearch of pattern. Otherwise patternFound is returned as FALSE, and posOfPattern
     is unchanged.
  *)


    PROCEDURE get_last_com (): pt.INDEX;
    BEGIN
      RETURN TableComps.Count;
    END get_last_com;

    PROCEDURE get_last_mod (): pt.INDEX;
    BEGIN
      RETURN TableCProfile.Count;
    END get_last_mod;

    PROCEDURE get_last_proc (): CARDINAL;
    BEGIN
      RETURN TableCProfile.Modules^[get_last_mod()-1].Procs.Count;
    END get_last_proc;

(*
    PROCEDURE print;
    VAR
      i, j: CARDINAL;
    BEGIN
      FOR i := 0 TO TableCProfile.Count-1 DO
        prn.printf ("Module %s from %s\n", TableCProfile.Modules^[i].Name, TableComps.Comps^[TableCProfile.Modules^[i].Index].Name);
        FOR j := 0 TO TableCProfile.Modules^[i].Procs.Count-1 DO
          prn.printf ("  Procedure %s\n", TableCProfile.Modules^[i].Procs.Procs^[j].Name);
        END;
      END;
    END print;
*)

  VAR
    i, p: CARDINAL;
    m, c: CARDINAL;
    ok  : BOOLEAN;
    info: pt.PROFDATA;
    addr: CARDINAL;
    end : CARDINAL;

  BEGIN
    IF text = txt.nil THEN
      RETURN pt.OpenErrorProfilerData;
    END;
    ProfMode := pt.mode_none;
    FOR ln := 1 TO txt.LastLine (text)-1 DO
      txt.GetLine (text, ln, line);
      IF line^ # "" THEN
        get_param (1, tmp);
        IF tmp = tMODE THEN
          get_param (2, tmp);
          IF tmp = tMODE_MIN THEN
            ProfMode := pt.mode_min;
          ELSIF tmp = tMODE_STD THEN
            ProfMode := pt.mode_std;
          ELSE
            ASSERT (tmp = tMODE_FULL);
            ProfMode := pt.mode_full;
          END;
        ELSIF tmp = tCOMPONENT THEN
          get_param (2, tmp);
          addr := xs.StrToCard (tmp, 16, ok);
          ASSERT (ok);
          get_param (3, tmp);
          end := xs.StrToCard (tmp, 16, ok);
          ASSERT (ok);
          get_rest_of_params (4, tmp);
          AddTableComp (addr, end, tmp, get_last_com());
        ELSIF tmp = tMODULE THEN
          get_param (2, tmp);
          AddTableCProfile (pt.TMODULE {get_last_com()-1, tmp, pt.TPROCS {NIL, 0}});
        ELSIF tmp = tPROCEDURE THEN
          get_param (2, tmp);
          info.pure_dur_hi       := xs.StrToCard (tmp, 16, ok);
          ASSERT (ok);
          get_param (3, tmp);
          info.pure_dur_lo       := xs.StrToCard (tmp, 16, ok);
          ASSERT (ok);
          get_param (4, tmp);
          info.dirty_dur_hi      := xs.StrToCard (tmp, 16, ok);
          ASSERT (ok);
          get_param (5, tmp);
          info.dirty_dur_lo      := xs.StrToCard (tmp, 16, ok);
          ASSERT (ok);
          get_param (6, tmp);
          info.total_entry_count := xs.StrToCard (tmp, 10, ok);
          ASSERT (ok);
          get_param (7, tmp);
          info.norec_entry_count := xs.StrToCard (tmp, 10, ok);
          ASSERT (ok);
          get_param (8, tmp);
          -- and add procedure to list
          IF (info.dirty_dur_hi > MAX(INTEGER) )OR (info.pure_dur_hi > MAX(INTEGER)) THEN
            info.pure_dur_hi := 0;
            info.pure_dur_lo := 0;
            info.dirty_dur_hi := 0;
            info.dirty_dur_lo := 0;
          END;
          AddTableCProfileProc (get_last_mod()-1, pt.TPROC {tmp, info, pt.TCALLS {NIL, 0}});
        ELSIF tmp = tCALL THEN
          get_param (2, tmp);
          addr := xs.StrToCard (tmp, 16, ok);
          ASSERT (ok);
          get_param (3, tmp);
          i := xs.StrToCard (tmp, 10, ok);
          ASSERT (ok);
          AddCProfileCall (get_last_mod()-1, get_last_proc()-1, pt.TCALL {0, addr, i});
        ELSE
          xs.Extract (line^, 0, LENGTH(tTIME), tmp);
          ASSERT (tmp = tTIME);
          get_param (4, tmp);
          CProfileTime.high := xs.StrToCard (tmp, 16, ok);
          ASSERT (ok);
          get_param (5, tmp);
          CProfileTime.low := xs.StrToCard (tmp, 16, ok);
          ASSERT (ok);
        END;
      END;
    END;
    util := pt.TRACE_CALLS_PROFILE;

    WITH TableCProfile DO
      IF Count > 0 THEN
        FOR m := 0 TO Count-1 DO
          WITH Modules^[m].Procs DO
            IF Count > 0 THEN
              FOR p := 0 TO Count-1 DO
                WITH Procs^[p].Calls DO
                  IF Count > 0 THEN
                    FOR c := 0 TO Count-1 DO
                      WITH Calls^[c] DO
                        ComNo := dt.Invalid_Component;
                        i := 0;
                        LOOP
                          addr := TableComps.Comps^[i].Begin;
                          end := TableComps.Comps^[i].End;
                          IF (addr <= Addr) AND (Addr <= end) THEN
                            ComNo := TableComps.Comps^[i].Index;
                            EXIT;
                          END;
                          INC (i);
                          IF i = TableComps.Count THEN
                            EXIT;
                          END;
                        END;
                      END;
                    END;
                  END;
                END;
              END;
            END;
          END;
        END;
      END;
    END;
(*
    WITH TableCProfile DO
      IF Count > 0 THEN
        FOR m := 0 TO Count-1 DO
          WITH Modules^[m] DO
            WITH Procs DO
              IF Count > 0 THEN
                FOR p := 0 TO Count-1 DO
                  WITH Procs^[p].Calls DO
                    IF Count > 0 THEN
                      FOR c := 0 TO Count-1 DO
                        WITH Calls^[c] DO
                          prn.printf ("mod=%d inx=%d proc=%d call=%d com=%d adr=0x%$8X\n", m, Index, p, c, ComNo, Addr);
                        END;
                      END;
                    END;
                  END;
                END;
              END;
            END;
          END;
        END;
      END;
    END;
  *)
--    ASSERT (ProfMode # pt.mode_none);
    RETURN 0;
  END ReadProtocolCallsProfiling;


VAR
  pro: ioc.ChanId;
  res: rf.OpenResults;
  i  : CARDINAL;
  N  : CARDINAL;
  tmp: xs.String;
  key: xs.String;
  len: sys.CARD16;
  n1 : CARDINAL;
  n2 : CARDINAL;
  buf: POINTER TO ARRAY OF CHAR;


  MODULE OpenClose;

  IMPORT rf, pro, name, res;

  BEGIN
    rf.OpenOld (pro, name, rf.read+rf.raw+rf.old, res);
  FINALLY
    IF res = rf.opened THEN
      rf.Close (pro);
    END;
  END OpenClose;


BEGIN
  IF res # rf.opened THEN
    RETURN pt.OpenErrorProfilerData;
  END;

  fmt.print (tmp, "%s, %s", pt.IDENTKEY, pt.VERSION);
  n1 := LENGTH(tmp)+1;
  ioc.RawRead (pro, sys.ADR(key), n1, n2 );
  IF n1 # n2 THEN
    RETURN pt.ReadErrorProfilerData;
  END;
  IF tmp # key THEN
    RETURN pt.IsNot_XDS_ProfilerTraceFile;
  END;

  rio.Read (pro, N);
  IF N = MAX(CARDINAL) THEN
--    res := rf.alreadyOpen;
--    rf.Close(pro);
    RETURN ReadProtocolCallsProfiling (name, util);
  ELSE
    WITH TableComps DO
      Count := N;
      NEW(Comps, Count);
      FOR i := 0 TO Count-1 DO
        rio.Read (pro, Comps^[i].Index);
      END;
      rio.Read (pro, len);
      NEW(buf, len);
      ioc.RawRead (pro, sys.ADR(buf^), len, n2);
      IF len # n2 THEN
        RETURN pt.WrongFormatProfilerData;
      END;
      n1 := 0;
      FOR i := 0 TO Count-1 DO
        n2 := 0;
        REPEAT
          Comps^[i].Name[n2] := buf^[n1];
          INC(n1);
          INC(n2);
        UNTIL buf^[n1] = 0C;
        Comps^[i].Name[n2] := 0C;
        INC(n1);
      END;
      DISPOSE(buf);
    END;

    rio.Read (pro, util);
    CASE util OF
    | pt.TRACE_EXECUTION:
      WITH TableSnapshots DO
        rio.Read (pro, Count);
        IF Count > 0 THEN
          NEW (Snapshots, Count);
          FOR i := 0 TO Count-1 DO
            rio.Read (pro, Snapshots^[i]);
          END;
        END;
      END;
    | pt.TRACE_MEMORY:
      WITH TableMemUsed DO
        rio.Read (pro, Count);
        IF Count > 0 THEN
          NEW (MemUsed, Count);
          FOR i := 0 TO Count-1 DO
            rio.Read (pro, MemUsed^[i]);
          END;
        END;
      END;
      rio.Read (pro, MemTraceResults);
    END;
  END;
  RETURN 0;
EXCEPT
  RETURN pt.WrongFormatProfilerData;
END ReadProtocol;



PROCEDURE ClearDebugInfo;
VAR
  j, p: CARDINAL;
BEGIN
  WITH TableComps DO
    IF Comps # NIL THEN
      DISPOSE (Comps);
    END;
    Count := 0;
  END;
  WITH TableSnapshots DO;
    IF Snapshots # NIL THEN
      DISPOSE (Snapshots);
    END;
    Count := 0;
  END;
  WITH TableMemUsed DO;
    IF MemUsed # NIL THEN
      DISPOSE (MemUsed);
    END;
    Count := 0;
  END;
  WITH TableCProfile DO
    IF Modules # NIL THEN
      FOR j := 0 TO Count-1 DO
        WITH Modules^[j] DO
          WITH Procs DO
            IF Procs # NIL THEN
              FOR p := 0 TO Count-1 DO
                WITH Procs^[p].Calls DO
                  IF Calls # NIL THEN
                    DISPOSE (Calls);
                  END;
                  Count := 0;
                END;
              END;
              DISPOSE(Procs);
            END;
            Count := 0;
          END;
        END;
      END;
      DISPOSE(Modules);
    END;
    Count := 0;
  END;
  MemTraceResults := pt.MEM_TRACE_RESULT {0, 0, 0, 0, 0, 0};
  CProfileTime := pt.TIME {0, 0};
END ClearDebugInfo;



BEGIN
  TableComps      := pt.COMPS {NIL, 0};
  TableSnapshots  := pt.SNAPSHOTS {NIL, 0};
  TableMemUsed    := pt.MEM_USED_STR {NIL, 0};
  TableCProfile   := pt.TMODULES {NIL, 0};
  MemTraceResults := pt.MEM_TRACE_RESULT {0, 0, 0, 0, 0, 0};
  CProfileTime    := pt.TIME {0, 0};
END PrfBuild.
