<* Storage+ *>

IMPLEMENTATION MODULE DI_Read;

IMPORT dll := dllRTS;
IMPORT fmt := FormStr;

IMPORT xs  := xStr;
IMPORT msg := MsgNo;
IMPORT Dll;

FROM KrnTypes IMPORT EXEC_INFO;
FROM DI_Types IMPORT ComNo, ModNo, COMPONENT, RAW_DEBUG_INFO,
                     PROCESS_DEBUGINFO_PROC, BUILD_FOR_MOD;

CONST
  ProcessDebugInfoProcedureName = 'ProcessDebugInfo';
  ReadModulesProcedureName      = 'ReadModules';
  BuildForModProcedureName      = 'BuildForMod';


<* PUSH *>
<* WOFF301+ *>
PROCEDURE DummyProcess(com: ComNo; VAR Component: COMPONENT): CARDINAL;
BEGIN
  RETURN msg.WrongDebugInfo;
END DummyProcess;

PROCEDURE DummyBuildForMod (modno: ModNo; com: ComNo; VAR Component: COMPONENT);
END DummyBuildForMod;

<* POP *>


PROCEDURE CheckDebugInfoVersion(exec_info: EXEC_INFO): BOOLEAN;
VAR
  name: xs.String;
  proc: PROC;
  hmod: dll.HMOD;
BEGIN
  ProcessDebugInfo := DummyProcess;
  ReadModules := DummyProcess;
  BuildForMod := DummyBuildForMod;
  IF exec_info.DebugInfoSize = 0 THEN
    RETURN FALSE;
  END;
  fmt.print(name, 'XD_%s', exec_info.DebugInfoTag);
  hmod := dll.GetModuleHandle (name);
  IF NOT Dll.CheckHandle (hmod) THEN
    hmod := dll.LoadModule (name);
    IF hmod = NIL THEN
      RETURN FALSE;
    END;
    Dll.AddHandle (hmod);
  END;
  proc := dll.GetProcAdr (hmod, ProcessDebugInfoProcedureName);
  IF proc = NIL THEN
    RETURN FALSE;
  END;
  ProcessDebugInfo := PROCESS_DEBUGINFO_PROC (proc);
  proc := dll.GetProcAdr(hmod, ReadModulesProcedureName);
  IF proc = NIL THEN
    RETURN FALSE;
  END;
  ReadModules := PROCESS_DEBUGINFO_PROC (proc);
  proc := dll.GetProcAdr(hmod, BuildForModProcedureName);
  IF proc = NIL THEN
    RETURN FALSE;
  END;
  BuildForMod := BUILD_FOR_MOD (proc);
  RETURN TRUE;
END CheckDebugInfoVersion;


BEGIN
  ProcessDebugInfo := DummyProcess;
END DI_Read.
