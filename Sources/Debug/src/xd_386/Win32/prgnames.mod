IMPLEMENTATION MODULE PrgNames;

<* Storage+ *>

IMPORT sys := SYSTEM;

IMPORT fmt := FormStr;
IMPORT fs  := FileSys;
IMPORT dll := dllRTS;

IMPORT xs  := xStr;
IMPORT fil := File;
IMPORT red := RedFile;

IMPORT kt  := KrnTypes;
IMPORT dbg := Krn_Dbg;
IMPORT mem := Krn_Mem;

IMPORT win := Windows;
IMPORT nls := WinNLS;
IMPORT ps  := PSAPI;



PROCEDURE SearchProgram (name-: ARRAY OF CHAR; VAR full_name: ARRAY OF CHAR): BOOLEAN;
VAR
  rc  : CARDINAL;
  path: ARRAY [0..1024] OF CHAR;
  a   : win.PSTR;
  res : red.RedirectionResults;
BEGIN
  IF fs.Exists (name) THEN
    fil.ModifyFileName (name, full_name);
    RETURN TRUE;
  ELSE
    res := red.Read (name, full_name);
    IF res = red.Ok THEN
      RETURN TRUE;
    ELSE
      rc := win.GetEnvironmentVariable ("PATH", path, HIGH(path));
      IF rc = 0 THEN
        rc := win.SearchPath (NIL, name, NIL, SIZE(full_name), full_name, a);
      ELSE
        rc := win.SearchPath (path, name, NIL, SIZE(full_name), full_name, a);
      END;
      RETURN rc <> 0;
    END;
  END;
END SearchProgram;



VAR
  FakeNameIndex: CARDINAL;


(* Retrieve name of module from module's open file handle *)
PROCEDURE RetrieveModuleInfo (hProcess: win.HANDLE; VAR exec_info: kt.EXEC_INFO; get_name: BOOLEAN): CARDINAL;
TYPE
  A = ARRAY [0..15] OF CARDINAL;

VAR
  hFile         : win.HANDLE;
  hMapFile      : win.HANDLE;
  lpFile        : win.PVOID;
  plpFile       : POINTER TO win.USHORT;
  nSections     : INTEGER;
  VAExportDir   : CARDINAL;
  i             : INTEGER;
  pA            : POINTER TO A;
  ImageHdrOffset: INTEGER;
  pCARDINAL     : POINTER TO CARDINAL;
  pfh           : win.PIMAGE_FILE_HEADER;
  poh           : win.PIMAGE_OPTIONAL_HEADER;
  psh           : win.PIMAGE_SECTION_HEADER;
  ped           : win.PIMAGE_EXPORT_DIRECTORY;
  pname         : xs.txt_ptr;

  MODULE Exit;
  IMPORT win, lpFile, hMapFile;
  BEGIN
  FINALLY
    (* clean up before exiting *)
    win.UnmapViewOfFile (lpFile);
    win.CloseHandle (hMapFile);
  END Exit;  


BEGIN
  hFile := win.HANDLE (exec_info.hFile);

  IF get_name THEN
    -- try to get component name by PS API,
    -- otherwise try to read it from PE header (see below)
    IF ps.GetModuleFileNameEx (hProcess, win.HMODULE (exec_info.Handle),
                           exec_info.full_name, SIZE(exec_info.full_name)) > 0
    THEN
      fil.ExtractFileName (exec_info.full_name, exec_info.short_name);
      get_name := FALSE;
    END;
  END;

  lpFile := NIL;

  (* memory map handle to DLL for easy access *)
  hMapFile := win.CreateFileMapping (hFile, NIL, win.PAGE_READONLY, 0, 0, NIL);
  IF hMapFile = NIL THEN
    RETURN win.GetLastError();
  END;

  (* map view of entire file *)
  lpFile := win.MapViewOfFile (hMapFile, win.FILE_MAP_READ, 0, 0, 0);
  IF lpFile = NIL THEN
    RETURN win.GetLastError();
  END;

  (* if DOS based file *)
  plpFile := sys.ADDRESS(lpFile);
  IF plpFile^ = win.IMAGE_DOS_SIGNATURE THEN

    (* file image header offset exists after DOS header and nt signature *)
    pA := sys.ADDRESS(lpFile);
    ImageHdrOffset := INTEGER(pA^[15]) + SIZE(CARDINAL);

    pCARDINAL := sys.ADDADR(lpFile,VAL(CARDINAL, ImageHdrOffset)-SIZE(CARDINAL));
    IF pCARDINAL^ # win.IMAGE_NT_SIGNATURE THEN
      RETURN MAX(CARDINAL);
    END;  

    pfh := win.PIMAGE_FILE_HEADER(sys.ADDADR(lpFile,VAL(CARDINAL, ImageHdrOffset)));

    (* if optional header exists and exports directory exists proceed *)
    IF pfh^.SizeOfOptionalHeader # 0 THEN
      (* locate virtual address for Export Image Directory in OptionalHeader *)

      poh := win.PIMAGE_OPTIONAL_HEADER(sys.ADDADR(pfh, SIZE(win.IMAGE_FILE_HEADER)));
      VAExportDir := poh^.DataDirectory[win.IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress;

      CASE poh^.Subsystem OF
      | win.IMAGE_SUBSYSTEM_WINDOWS_GUI:
        (* Image runs in the Windows GUI subsystem. *)
        exec_info.app_type := kt.windowed;
      | win.IMAGE_SUBSYSTEM_WINDOWS_CUI:
        (* Image runs in the Windows character subsystem. *)
        exec_info.app_type := kt.console;
      ELSE
        exec_info.app_type := kt.console;
      END;  

      (* locate section where export virtual address is located *)
      psh := win.PIMAGE_SECTION_HEADER(sys.ADDADR(poh, pfh^.SizeOfOptionalHeader));
      nSections := pfh^.NumberOfSections;
      ASSERT( exec_info.Objects # NIL);
      ASSERT( VAL(CARDINAL, nSections) = HIGH(exec_info.Objects^)+1 );
      i := 0;
      LOOP
        IF (i = nSections) THEN
          EXIT;
        END;
        exec_info.Objects^[i].RelocationBase := poh^.ImageBase+psh^.VirtualAddress;
        IF get_name THEN
          IF ((psh^.VirtualAddress <= VAExportDir) AND (psh^.VirtualAddress + psh^.SizeOfRawData > VAExportDir)) THEN
            (* locate export image directory *)
            ped := win.PIMAGE_EXPORT_DIRECTORY(sys.ADDADR(lpFile, VAExportDir - psh^.VirtualAddress + psh^.PointerToRawData));
            (* read name from export directory *)
            pname := sys.ADDADR(lpFile,ped^.Name+psh^.PointerToRawData-psh^.VirtualAddress);
            COPY (pname^, exec_info.short_name);
            IF NOT SearchProgram (exec_info.short_name, exec_info.full_name) THEN
              COPY (exec_info.short_name, exec_info.full_name);
            END;
            get_name := FALSE;
          END;
        END;
        psh := sys.ADDADR(psh, SIZE(psh^));
        INC(i);
      END;
      IF get_name THEN
        -- general joppa
        fmt.print (exec_info.short_name, "fake%d", FakeNameIndex);
        fmt.print (exec_info.full_name, "fake%d", FakeNameIndex);
        INC(FakeNameIndex);
      END;
      RETURN 0; -- ok! fake name
    ELSE
      RETURN MAX(CARDINAL);
    END;  
  ELSE
    RETURN MAX(CARDINAL);
  END;
END RetrieveModuleInfo;    


PROCEDURE ConvertUnicode (VAR name: ARRAY OF CHAR; len: CARDINAL);
VAR
  i: CARDINAL;
  buf: xs.String;
  p: POINTER TO ARRAY [0..1024] OF sys.CARD16;
  flag: BOOLEAN;
BEGIN
  p := sys.ADR (name);
  flag := TRUE;
  i := nls.WideCharToMultiByte ( nls.CP_OEMCP, nls.WC_SET {}, p^, len
                               , buf, SIZE (buf), "?", sys.ADR (flag));
  IF i # 0 THEN
    buf [i DIV 2] := 0C;
    COPY (buf, name);
    RETURN;
  END;
END ConvertUnicode;


PROCEDURE ReadName (lpImageName: win.PVOID; fUnicode: win.WORD; VAR name: ARRAY OF CHAR): BOOLEAN;
VAR
  i: CARDINAL;
  buf: ARRAY [0..win.MAX_PATH] OF CHAR;
  p: kt.ADDRESS;
BEGIN
  IF NOT mem.Get (kt.ADDRESS(lpImageName), sys.ADR(p), SIZE(p)) THEN
    RETURN FALSE;
  END;
  i := 0;
  IF fUnicode = 0 THEN -- ASCII
    LOOP
      IF NOT mem.Get (p+i, sys.ADR(buf[i]), 1) THEN
        RETURN FALSE;
      END;
      IF buf[i] = 0C THEN
        EXIT;
      END;
      IF i = HIGH(buf) THEN
        RETURN FALSE;
      END;
      INC (i);
    END;
  ELSE
    LOOP
      IF NOT mem.Get (p+i, sys.ADR(buf[i]), 1) THEN
        RETURN FALSE;
      END;
      IF (i > 0) AND (buf[i-1] = 0C) AND (buf[i] = 0C) THEN
        EXIT;
      END;
      IF i = HIGH(buf) THEN
        RETURN FALSE;
      END;
      INC (i);
    END;
    ConvertUnicode (buf, i);
  END;
  COPY (buf, name);
  RETURN (buf[i] = 0C) AND (LENGTH(buf) <= HIGH(name));
END ReadName;


BEGIN
  FakeNameIndex := 1;
END PrgNames.