(* Copyright (c) Excelsior 2002. All Rights Reserved. *)

(* This module provides subset of Proccess Status API (PSAPI) *)

<* +M2EXTENSIONS *>
<* ALIGNMENT = "1" *>

IMPLEMENTATION MODULE PSAPI;

IMPORT SYSTEM;

IMPORT dll := dllRTS;
IMPORT win := Windows;


TYPE
  FN_GetModuleFileNameEx = PROCEDURE ["StdCall"]
    ( win.HANDLE  -- handle to process
    , win.HMODULE -- handle to module
    , win.PTSTR   -- path buffer
    , win.DWORD   -- maximum characters to retrieve
    ): win.DWORD; -- the length of the string copied to the buffer


VAR
  fn_GetModuleFileNameEx: FN_GetModuleFileNameEx;





VAR
  PSAPI_Initialized: BOOLEAN;
  PSAPI_Available: BOOLEAN;
  PSAPI_hDll: dll.HMOD;


-- initialize Proccess Status API (PSAPI)
PROCEDURE InitializePSAPI (): BOOLEAN;
VAR
  ver: win.OSVERSIONINFO;
BEGIN
  IF PSAPI_Initialized THEN
    RETURN PSAPI_Available;
  END;
  PSAPI_Initialized := TRUE;
  PSAPI_Available := FALSE;

  ver.dwOSVersionInfoSize := SIZE (ver);
  win.GetVersionEx (ver);
  IF (ver.dwPlatformId # win.VER_PLATFORM_WIN32_NT) OR
     (ver.dwMajorVersion < 4)
  THEN
    -- PSAPI available on Windows NT only since NT 4.0
    RETURN FALSE;
  END;

  PSAPI_hDll := dll.LoadModule ("psapi.dll");
  IF PSAPI_hDll = NIL THEN
    -- dll not found
    RETURN FALSE;
  END;

  fn_GetModuleFileNameEx := FN_GetModuleFileNameEx (dll.GetProcAdr (PSAPI_hDll, "GetModuleFileNameExA"));
  IF fn_GetModuleFileNameEx = NIL THEN
    -- function not found
    RETURN FALSE;
  END;

  PSAPI_Available := TRUE;
  RETURN PSAPI_Available;
END InitializePSAPI;


PROCEDURE ReleasePSAPI ();
VAR
  tmp: BOOLEAN;
BEGIN
  IF PSAPI_Initialized THEN
    dll.FreeModule (PSAPI_hDll, tmp);
  END;
END ReleasePSAPI;




PROCEDURE ["StdCall"] GetModuleFileNameEx
    ( hProcess   : win.HANDLE  -- handle to process
    ; hModule    : win.HMODULE -- handle to module
    ; lpFilename : win.PTSTR   -- path buffer
    ; nSize      : win.DWORD   -- maximum characters to retrieve
    ): win.DWORD;              -- the length of the string copied to the buffer
BEGIN
  IF NOT InitializePSAPI () THEN
    RETURN 0; -- error
  END;
  RETURN fn_GetModuleFileNameEx (hProcess, hModule, lpFilename, nSize);
END GetModuleFileNameEx;




BEGIN
  PSAPI_Initialized := FALSE;
FINALLY
  ReleasePSAPI ();
END PSAPI.