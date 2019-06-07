--------------------------------------------------------------------------------
--                      Excelsior XDS Test Coverage System
--                          (c) 2015, Excelsior Ltd.
-- Module:   tcConfig
-- Mission:  Test Coverage Configuration.
-- Authors:  Lvov Konstantin
-- Created:  07-Mar-2006
--
-- It's a part of Excelsior XDS O2/M2 compiler. 
-- Set option '-target_testcoverage:+' to build in this tool into compiler.
--------------------------------------------------------------------------------
MODULE tcConfig;

IMPORT  sys := SYSTEM,         RegComp,              tcLib
     ,  env := xiEnv,          DStrings,             xfs := xiFiles
<* IF    TARGET_386  THEN *>,  tc := xrTCSx86
<* ELSIF TARGET_PPC   THEN *>,  tc := xrTCSppc
<* ELSIF TARGET_MIPS THEN *>,  tc := xrTCSmips
<* ELSIF TARGET_VAX  THEN *>,  tc := xrTCSvax
<* ELSIF TARGET_SPARC THEN *>,  tc := xrTCSsparc
<* END *>
     ;

TYPE 
  String *= DStrings.String;

CONST 
  version     * = "v4.10";
  versionRTS  * = tc.TCS_RTS_VERSION;

  EQU_TCSMODULE *= "TCSMODULE";

VAR
  ProjectName    *: String;
  MainModuleName *: String;

  ModuleMaskExpr *: RegComp.Expr;

  TestcoverageActivated *: BOOLEAN;

--------------------------------------------------------------------------------
PROCEDURE InitModuleMask ();
VAR str: String;
    int: LONGINT;
BEGIN
  env.config.Equation(env.EQU_TESTCOVERAGEMASK, str);
  IF str = NIL THEN
    env.errors.Error (env.null_pos, 1025);
    RETURN;
  END;

  xfs.sys.ConvertFromHost(str^, str);
  ModuleMaskExpr := tcLib.GetModuleMask(str^, int);
  IF (int <= 0) THEN
    env.errors.Fault(env.null_pos, 451, str^);
  END;
END InitModuleMask;

--------------------------------------------------------------------------------
PROCEDURE isTestCoverageEnable * (check_activated:=FALSE: BOOLEAN): BOOLEAN;
BEGIN
  IF env.config.Option(env.OPT_TESTCOVERAGE) THEN
    IF (env.config.Option("multithread")) THEN
      env.errors.Fault(env.null_pos, 452);
      RETURN FALSE;
    ELSE
      RETURN NOT check_activated
          OR TestcoverageActivated;
    END;
  ELSE
    RETURN FALSE;
  END;
END isTestCoverageEnable;


--------------------------------------------------------------------------------
PROCEDURE IgnoreModule * (file_name-: ARRAY OF CHAR): BOOLEAN;
BEGIN
  IF NOT isTestCoverageEnable() THEN
    RETURN TRUE;
  ELSIF ModuleMaskExpr = NIL THEN
    InitModuleMask();
 END;

  IF ModuleMaskExpr = NIL THEN
    RETURN TRUE;
  ELSIF NOT tcLib.MatchModuleMask(ModuleMaskExpr, file_name, 0) THEN
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END IgnoreModule;


--------------------------------------------------------------------------------
PROCEDURE DynamicModuleRegistration * (): BOOLEAN;
BEGIN
  RETURN env.config.Option("GENDLL");
END DynamicModuleRegistration;


--------------------------------------------------------------------------------
PROCEDURE SetProjectName * (projectName: String);
BEGIN
  ProjectName := projectName;
END SetProjectName;

--------------------------------------------------------------------------------
PROCEDURE SetMainModuleName * (moduleName-: ARRAY OF CHAR);
BEGIN
-- KMS-183: Развал компилятора для проекта с несколькими программными модулями
--  ASSERT( MainModuleName = NIL );
  DStrings.Assign(moduleName, MainModuleName);
END SetMainModuleName;


--------------------------------------------------------------------------------
PROCEDURE Reset * ();
BEGIN
  ProjectName    := NIL;
  MainModuleName := NIL;
  ModuleMaskExpr := NIL;
  TestcoverageActivated := FALSE;
END Reset;

--------------------------------------------------------------------------------
BEGIN
  Reset();
END tcConfig.
