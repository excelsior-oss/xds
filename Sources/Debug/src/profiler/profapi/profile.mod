<* Storage+ *>
<* ALIGNMENT = "1" *>

IMPLEMENTATION MODULE Profile;

IMPORT sys := SYSTEM;
IMPORT ioc := IOChan;
IMPORT rf  := RndFile;
IMPORT fn  := FileName;
IMPORT fmt := FormStr;

IMPORT kt  := KrnTypes;

IMPORT dt  := DI_Types;
IMPORT tls := DI_Tools;
IMPORT bld := DI_Build;
IMPORT dri := DI_Read;

IMPORT xs  := xStr;
IMPORT fil := File;
IMPORT sor := Sort;

IMPORT sex := ScanExe;

IMPORT pt  := PrfTypes;
IMPORT pb  := PrfBuild;

FROM Printf IMPORT printf;


VAR
  UtilityType: pt.UTILITY;
  Components : pt.COMPONENTS;


PROCEDURE AddComponent (name-: ARRAY OF CHAR);
BEGIN
  WITH Components DO
    IF Components = NIL THEN
      NEW(Components, HIGH(pb.TableComps.Comps^)+1);
      ASSERT(Components # NIL);
    END;
    WITH Components^[Count] DO
      COPY(name, ComName);
      CCount := 0;
      Unknown := 0;
      Modules := NIL;
      Publics := NIL;
    END;
    INC(Count);
  END;
END AddComponent;


CONST
  together = TRUE;


PROCEDURE AddProcedure (comno, modno: CARDINAL; proc: pt.RPROCEDURE; together: BOOLEAN);
CONST
  HN_BLOCK = 16;

VAR
  tmp: pt.PAPROCEDURES;
  i: CARDINAL;
BEGIN
  WITH Components.Components^[comno].Modules^[modno].Procedures DO
    IF Proc = NIL THEN
      NEW(Proc, HN_BLOCK);
      ASSERT(Proc # NIL);
    ELSIF Count > HIGH(Proc^) THEN
      NEW(tmp, HIGH(Proc^)+1 + HN_BLOCK);
      ASSERT(tmp # NIL);
      sys.MOVE(sys.ADR(Proc^), sys.ADR(tmp^), SIZE(Proc^));
      DISPOSE(Proc);
      Proc := tmp;
    END;
    IF together THEN
      i := 0;
      WHILE i < Count DO
        WITH Proc^[i] DO
          IF tls.EqualObjects (Proc, proc.Proc) THEN
            INC (PCount);
            RETURN;
          END;
        END;
        INC(i);
      END;
    END;
    Proc^[Count] := proc;
    INC(Count);
  END;
END AddProcedure;



PROCEDURE ReadProfilerData (util: pt.UTILITY): BOOLEAN;
TYPE
  CHAR_SET = SET OF CHAR;

VAR
  comp : dt.COMPONENT;
  fname: xs.String;
  cname: xs.String;
  pstr : xs.txt_ptr;
  pname: xs.txt_ptr;
  i, n : CARDINAL;
  p    : CARDINAL;
  obj  : CARDINAL;
  inx  : CARDINAL;
  offs : CARDINAL;
  pos  : CARDINAL;
  comno: CARDINAL;
  modno: CARDINAL;
  line : CARDINAL;
  proc : dt.OBJECT;
  calls: pt.TCALLS;

  PROCEDURE MakeComName (name-: ARRAY OF CHAR);
  BEGIN
    COPY(name, fname);
    fn.GetName (fname, cname);
    xs.Append('$', cname);
  END MakeComName;


BEGIN
  IF pb.TableComps.Count = 0 THEN
    RETURN FALSE;
  END;
  FOR i := 0 TO pb.TableComps.Count-1 DO
    MakeComName (pb.TableComps.Comps^[i].Name);
    IF NOT tls.FindComponentByName(cname, comno) THEN
      comp := dt.EmptyComponent;
      WITH comp DO
       (* Name := bld.AddName( cname);   *)
        DI := dt.EmptyDebugInfo;
        raw := NIL;
      END;
      bld.AddComponent(comp);
      comno:= dt.Components.Count-1;
      dt.Components.Components^[comno].Name:= bld.AddName( cname);
      AddComponent (fname);
      IF sex.OpenExe (fname, comno, dt.Components.Components^[comno]) THEN
        IF dri.ReadModules (comno, dt.Components.Components^[comno]) = 0 THEN
          modno := dt.Components.Components^[comno].DI.LastModule;
          IF modno # 0 THEN
            WITH Components.Components^[comno] DO
              NEW (Modules, modno);
              FOR modno := 0 TO HIGH(Modules^) DO
                WITH Modules^[modno] DO
                  MCount := 0;
                  Procedures := pt.PROCEDURES{ NIL, 0 };
                  Lines := NIL;
                END;
              END;
            END;
          END;
        END;
      END;
    END;
  END;

  CASE util OF
  | pt.TRACE_EXECUTION:
    n := pb.TableSnapshots.Count;
  | pt.TRACE_MEMORY:
    n := pb.TableMemUsed.Count;
  | pt.TRACE_CALLS_PROFILE:
    n := pb.TableCProfile.Count;
  END;

  IF n > 0 THEN
    FOR i := 0 TO n-1 DO
      IF (util = pt.TRACE_EXECUTION) OR (util = pt.TRACE_MEMORY) THEN
        CASE util OF
        | pt.TRACE_EXECUTION:
          inx  := pb.TableSnapshots.Snapshots^[i].Index;
          obj  := pb.TableSnapshots.Snapshots^[i].Object;
          offs := pb.TableSnapshots.Snapshots^[i].Offset;
        | pt.TRACE_MEMORY:
          inx  := pb.TableMemUsed.MemUsed^[i].Index;
          obj  := pb.TableMemUsed.MemUsed^[i].Object;
          offs := pb.TableMemUsed.MemUsed^[i].Offset;
        END;
        MakeComName (pb.TableComps.Comps^[pb.GetInternalIndex(inx)].Name);
        ASSERT(tls.FindComponentByName(cname, comno));
        INC(Components.Components^[comno].CCount);
        IF obj < dt.Components.Components^[comno].EI.N_Objects THEN
          INC(offs, dt.Components.Components^[comno].EI.Objects^[obj].Begin);
          IF dri.CheckDebugInfoVersion (dt.Components.Components^[comno].EI) THEN
            IF tls.FindModInCompByAddr (comno, offs, modno) THEN
              dri.BuildForMod (modno-1, comno, dt.Components.Components^[comno]);
              WITH Components.Components^[comno].Modules^[modno-1] DO
                IF MCount = 0 THEN
                  line := tls.LastLineHasCode (comno, modno);
                  IF line # 0 THEN
                    NEW(Lines, line);
                    sys.FILL(sys.ADR(Lines^[0]), 0, SIZE(Lines^));
                  END;
                END;
                INC(MCount);
                proc := tls.FindProcByAddr (comno, modno, offs);
                AddProcedure (comno, modno-1, pt.RPROCEDURE {proc, pt.TRACE_EXECUTION, 1}, together);
                IF tls.SourceByAddrInMod (comno, modno, offs, line) THEN
                  INC(Lines^[line-1]);
                END;
              END;
            ELSE
              INC(Components.Components^[comno].Unknown);
            END;
          ELSIF tls.FindPublicByAddrInCom (comno, offs, FALSE, pname) THEN
            WITH Components.Components^[comno] DO
              IF Publics = NIL THEN
                pos := tls.PublicsNo (comno);
                ASSERT(pos # 0);
                NEW(Publics, pos);
                sys.FILL(sys.ADR(Publics^[0]), 0, SIZE(Publics^));
              END;
              pos := 0;
              LOOP
                pstr := tls.GetPublicName (comno, pos);
                IF pstr^ = pname^ THEN
                  INC(Publics^[pos]);
                  EXIT;
                END;
                IF pos = HIGH(Publics^) THEN ASSERT(FALSE); END;
                INC(pos);
              END;
            END;
          ELSE
            INC(Components.Components^[comno].Unknown);
          END;
        ELSE
          INC(Components.Components^[comno].Unknown);
        END;
      ELSIF util = pt.TRACE_CALLS_PROFILE THEN
        inx := pb.TableCProfile.Modules^[i].Index;
        inx := pb.GetInternalIndex (inx);
        MakeComName (pb.TableComps.Comps^[inx].Name);
        ASSERT(tls.FindComponentByName(cname, comno));
        Components.Components^[comno].Base := pb.TableComps.Comps^[inx].Begin;
        IF dri.CheckDebugInfoVersion (dt.Components.Components^[comno].EI) THEN
          bld.CorrectModuleName (pb.TableCProfile.Modules^[i].Name);
          IF tls.FindModInComp (comno, pb.TableCProfile.Modules^[i].Name, modno) THEN
            dri.BuildForMod (modno-1, comno, dt.Components.Components^[comno]);
            FOR p := 1 TO pb.TableCProfile.Modules^[i].Procs.Count DO
              proc := tls.FindObjectByName (comno, modno, pb.TableCProfile.Modules^[i].Procs.Procs^[p-1].Name);
              WITH pb.TableCProfile.Modules^[i].Procs.Procs^[p-1] DO
                IF Calls.Calls = NIL THEN
                  calls := pt.TCALLS {NIL, 0};
                ELSE
                  NEW (calls.Calls, HIGH(Calls.Calls^)+1);
                  sys.MOVE (sys.ADR(Calls.Calls^), sys.ADR(calls.Calls^), SIZE(Calls.Calls^));
                  calls.Count := Calls.Count;
                END;
                AddProcedure (comno, modno-1, pt.RPROCEDURE {proc, pt.TRACE_CALLS_PROFILE, Data, calls}, NOT together);
              END;
            END;
          ELSE
            -- module not found
            printf("== module %s not found\n", pb.TableCProfile.Modules^[i].Name);
            RETURN FALSE;
          END;
        END;
      END;
    END;
  END;

  RETURN TRUE;
END ReadProfilerData;


-- Load profiler trace file and debugging information.
-- "name" is name of profiler trace file
-- Result:
--   <= 0 in case of error (see above)
--   >  0 if success (= number of components)
PROCEDURE ["C"] LoadDebugInfo (name-: ARRAY OF CHAR): INTEGER;
VAR
  name_ptr: xs.txt_ptr;
  name_str: xs.String;
  i, rc: INTEGER;
BEGIN
  ClearDebugInfo;
  name_ptr := sys.ADR(name);
  i := 0;
  LOOP
    name_str[i] := name_ptr^[i];
    IF name_ptr^[i] = 0C THEN EXIT; END;
    INC(i);
  END;
  rc := pb.ReadProtocol (name_str, UtilityType);
  IF rc # 0 THEN
    RETURN rc;
  END;
  IF ReadProfilerData (UtilityType) THEN
    RETURN Components.Count;
  ELSE
    RETURN WrongFormatProfilerData;
  END;
END LoadDebugInfo;


PROCEDURE ["C"] Utility (): CARDINAL;
BEGIN
  CASE UtilityType OF
  | pt.TRACE_EXECUTION:
    RETURN TRACE_EXECUTION;
  | pt.TRACE_MEMORY:
    RETURN TRACE_MEMORY;
  | pt.TRACE_CALLS_PROFILE:
    RETURN TRACE_CALLS_PROFILE;
  ELSE
    RETURN UNKNOWN;
  END;
END Utility;


PROCEDURE ["C"] ClearDebugInfo;
VAR
  i, j, p: CARDINAL;
BEGIN
  tls.ClearComponents (TRUE);
  pb.ClearDebugInfo;
  WITH Components DO
    IF Components # NIL THEN
      FOR i := 0 TO Count-1 DO
        WITH Components^[i] DO
          IF Modules # NIL THEN
            FOR j := 0 TO HIGH(Modules^) DO
              WITH Modules^[j] DO
                WITH Procedures DO
                  IF Proc # NIL THEN
                    IF UtilityType = pt.TRACE_CALLS_PROFILE THEN
                      FOR p := 0 TO Count-1 DO
                        WITH Proc^[p].Calls DO
                          Count := 0;
                          IF Calls # NIL THEN
                            DISPOSE (Calls);
                          END;
                        END;
                      END;
                    END;
                    DISPOSE (Proc);
                  END;
                  Count := 0;
                END;
                IF Lines # NIL THEN
                  DISPOSE (Lines);
                END;
              END;
            END;
            DISPOSE (Modules);
          END;
          IF UtilityType # pt.TRACE_CALLS_PROFILE THEN
            IF Publics # NIL THEN
              DISPOSE(Publics);
            END;
          END;
        END;
      END;
      DISPOSE(Components);
    END;
    Count := 0;
  END;
END ClearDebugInfo;


PROCEDURE ["C"] GetSnapshots (): INTEGER;
BEGIN
  RETURN VAL(INTEGER, pb.TableSnapshots.Count);
END GetSnapshots;

-- Number of times of memory used
PROCEDURE ["C"] GetMemUsed (): INTEGER;
BEGIN
  RETURN VAL(INTEGER, pb.TableMemUsed.Count);
END GetMemUsed;


-- Component name
-- nCom from [0..Quantity_Components-1]
PROCEDURE ["C"] ComponentName (nCom: INTEGER): NAME;
BEGIN
  RETURN NAME(sys.ADR(Components.Components^[nCom].ComName));
END ComponentName;


-- Number of modules or publics in component
-- nCom from [0..Quantity_Components-1]
-- result = 0 - no parts in components
-- result > 0 - quantity modules
-- result < 0 - quantity publics
PROCEDURE ["C"] N_Parts (nCom: INTEGER): INTEGER;
BEGIN
  IF Components.Components^[nCom].Modules # NIL THEN
    RETURN HIGH(Components.Components^[nCom].Modules^)+1;
  ELSIF Components.Components^[nCom].Publics # NIL THEN
    RETURN -VAL(INTEGER, HIGH(Components.Components^[nCom].Publics^))-1;
  ELSE
    RETURN 0;
  END;
END N_Parts;


-- Component shapshots
-- nCom from [0..Quantity_Components-1]
PROCEDURE ["C"] ComponentSnapshots (nCom: INTEGER): INTEGER;
BEGIN
  RETURN Components.Components^[nCom].CCount;
END ComponentSnapshots;


-- Quantity of snapshots in unknown parts
PROCEDURE ["C"] GetUnknownParts (nCom: INTEGER): INTEGER;
BEGIN
  RETURN Components.Components^[nCom].Unknown;
END GetUnknownParts;


-- Public name
-- nPublic from [0..Quantity_Public-1]
PROCEDURE ["C"] PublicName (nCom, nPublic: INTEGER): NAME;
VAR
  name_ptr: xs.txt_ptr;
BEGIN
  name_ptr := tls.GetPublicName (nCom, nPublic);
  RETURN NAME(name_ptr);
END PublicName;


-- Public shapshots
-- nPublic from [0..Quantity_Publics-1]
PROCEDURE ["C"] PublicSnapshots (nCom, nPublic: INTEGER): INTEGER;
BEGIN
  RETURN Components.Components^[nCom].Publics^[nPublic];
END PublicSnapshots;


-- Public attributes: address and lenght
-- nPublic from [0..Quantity_Publics-1]
PROCEDURE ["C"] PublicAttr (nCom, nPublic: INTEGER; VAR addr, length: CARDINAL);
BEGIN
  addr := tls.GetPublicAddr (nCom, nPublic);
  length := tls.GetPublicLen (nCom, nPublic);
END PublicAttr;



PROCEDURE ["C"] ModuleName (nCom, nModule: INTEGER): NAME;
VAR
  name_ptr: xs.txt_ptr;
BEGIN
  ASSERT(tls.ModName (nCom, nModule+1, name_ptr));
  RETURN NAME(name_ptr);
END ModuleName;


PROCEDURE ["C"] SourceName (nCom, nModule: INTEGER): NAME;
VAR
  name_ptr: xs.txt_ptr;
BEGIN
  name_ptr := tls.GetName ( dt.Components.Components^[nCom].DI.Modules^[nModule].SourceName);
  IF name_ptr^ = "" THEN
    ASSERT(tls.ModName (nCom, nModule+1, name_ptr));
  END;
  RETURN NAME(name_ptr);
END SourceName;


PROCEDURE ["C"] ModuleSnapshots (nCom, nModule: INTEGER): INTEGER;
BEGIN
  RETURN Components.Components^[nCom].Modules^[nModule].MCount;
END ModuleSnapshots;


PROCEDURE ["C"] N_Proc (nCom, nModule: INTEGER): INTEGER;
BEGIN
  RETURN Components.Components^[nCom].Modules^[nModule].Procedures.Count;
END N_Proc;


CONST
  UnknownProcedure = 'Unknown';


PROCEDURE ["C"] ProcName (nCom, nModule: INTEGER; nProc: INTEGER): NAME;
VAR
  name_ptr: xs.txt_ptr;
BEGIN
  WITH Components.Components^[nCom].Modules^[nModule].Procedures.Proc^[nProc] DO
    IF tls.EqualObjects(dt.Invalid_Object, Proc) THEN
      name_ptr := sys.ADR(UnknownProcedure);
    ELSE
      tls.Object_pName (Proc, name_ptr);
    END;
  END;
  RETURN NAME(name_ptr);
END ProcName;


PROCEDURE ["C"] ProcSnapshots  (nCom, nModule: INTEGER; nProc: INTEGER): INTEGER;
BEGIN
  RETURN Components.Components^[nCom].Modules^[nModule].Procedures.Proc^[nProc].PCount;
END ProcSnapshots;


PROCEDURE ["C"] ProcBounds (nCom, nModule, nProc: INTEGER; VAR begin, end: INTEGER): sys.BOOL32;
VAR
  prolog, prolog_line: CARDINAL;
  epilog, epilog_line: CARDINAL;
BEGIN
  WITH Components.Components^[nCom].Modules^[nModule] DO
    WITH Procedures.Proc^[nProc] DO
      IF tls.EqualObjects(dt.Invalid_Object, Proc) THEN
        RETURN FALSE;
      ELSE
        ASSERT(tls.ObjectAddr(Proc, prolog));
        IF NOT tls.SourceByAddrInMod (nCom, nModule+1, prolog, prolog_line) THEN RETURN FALSE; END;
        begin := VAL(INTEGER, prolog_line) - 1;
        tls.ProcAttr(Proc, prolog, epilog);
        IF NOT tls.SourceByAddrInMod (nCom, nModule+1, epilog, epilog_line) THEN RETURN FALSE; END;
        end := VAL(INTEGER, epilog_line) - 1;
      END;
    END;
  END;
  RETURN TRUE;
END ProcBounds;



PROCEDURE ["C"] LineSnapshots (nCom, nModule, nLine: INTEGER): INTEGER;
BEGIN
  WITH Components.Components^[nCom].Modules^[nModule] DO
    IF (Lines # NIL) AND (nLine <= VAL(INTEGER, HIGH(Lines^))) THEN
      RETURN Lines^[nLine];
    ELSE
      RETURN 0;
    END;
  END;
END LineSnapshots;


PROCEDURE ["C"] GetMemTraceResults (VAR res: pt.MEM_TRACE_RESULT): sys.BOOL32;
BEGIN
  IF UtilityType = pt.TRACE_MEMORY THEN
    res := pb.MemTraceResults;
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END GetMemTraceResults;

-- Procedure info (about time)
-- nModule from [0..Quantity_Modules-1]
-- nProc from [0..Quantity_Procedures-1]
PROCEDURE ["C"] ProcInfo (nCom, nModule, nProc: INTEGER; VAR info: pt.PROFDATA): sys.BOOL32;
BEGIN
  IF UtilityType = pt.TRACE_CALLS_PROFILE THEN
    info := Components.Components^[nCom].Modules^[nModule].Procedures.Proc^[nProc].Data;
    RETURN TRUE;
  ELSE
    info := pt.EMPTY_PROFDATA;
    RETURN FALSE;
  END;
END ProcInfo;


-- Procedure: calls info
-- nModule from [0..Quantity_Modules-1]
-- nProc from [0..Quantity_Procedures-1]
PROCEDURE ["C"] N_Call (nCom, nModule, nProc: INTEGER): INTEGER;
BEGIN
  IF UtilityType = pt.TRACE_CALLS_PROFILE THEN
    RETURN Components.Components^[nCom].Modules^[nModule].Procedures.Proc^[nProc].Calls.Count;
  ELSE
    RETURN 0;
  END;
END N_Call;


-- Procedure: calls info
-- nModule from [0..Quantity_Modules-1]
-- nProc from [0..Quantity_Procedures-1]
PROCEDURE ["C"] CallCount (nCom, nModule, nProc, nCall: INTEGER): INTEGER;
BEGIN
  IF UtilityType = pt.TRACE_CALLS_PROFILE THEN
    RETURN Components.Components^[nCom].Modules^[nModule].Procedures.Proc^[nProc].Calls.Calls^[nCall].Num;
  ELSE
    RETURN -1;
  END;
END CallCount;


VAR
  addr  : kt.ADDRESS;
  Com   : dt.ComNo;
  Mod   : dt.ModNo;
  Line  : CARDINAL;
  result: BOOLEAN;
  base  : kt.ADDRESS;

-- nModule from [0..Quantity_Modules-1]
-- nProc from [0..Quantity_Procedures-1]
-- nCall from [0..N_Call-1]
PROCEDURE ["C"] CallPlace (nCom, nModule, nProc, nCall: INTEGER; VAR com, mod, line: INTEGER): sys.BOOL32;
BEGIN
  result := FALSE;
  IF UtilityType = pt.TRACE_CALLS_PROFILE THEN
    Com  := Components.Components^[nCom].Modules^[nModule].Procedures.Proc^[nProc].Calls.Calls^[nCall].ComNo;
    IF Com # dt.Invalid_Component THEN
      base := Components.Components^[Com].Base;
      addr := Components.Components^[nCom].Modules^[nModule].Procedures.Proc^[nProc].Calls.Calls^[nCall].Addr;
      DEC (addr, base);
      result := tls.FindModInCompByAddr (Com, addr, Mod) AND tls.SourceByAddrInMod (Com, Mod, addr, Line);
      IF result THEN
        com := Com;
        mod := Mod;
        IF mod > 0 THEN
          DEC (mod);
        END;
        line := Line;
        IF line > 0 THEN
          DEC (line);
        END;
      END;
    END;
  END;
  RETURN result;
END CallPlace;

PROCEDURE ["C"] GetExecutionTime (VAR time: pt.TIME): sys.BOOL32;
BEGIN
  IF UtilityType = pt.TRACE_CALLS_PROFILE THEN
    time := pb.CProfileTime;
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END GetExecutionTime;


PROCEDURE ["C"] GetProfilerMode (): pt.PROF_MODE;
BEGIN
  RETURN pb.ProfMode;
END GetProfilerMode;


BEGIN
  Components := pt.COMPONENTS{ NIL, 0 };
END Profile.

