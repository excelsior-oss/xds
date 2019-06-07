<* Storage+ *>
<* ALIGNMENT="1" *>

IMPLEMENTATION MODULE ReadExp;

IMPORT sys := SYSTEM;
IMPORT rf  := RndFile;
IMPORT xfp := xFilePos;
IMPORT ioc := IOChan;
IMPORT rio := RawIO;
IMPORT fmt := FormStr;

IMPORT win := Windows;

IMPORT kt  := KrnTypes;

IMPORT Printf;


PROCEDURE ReadExport (full_name-: ARRAY OF CHAR; VAR exp: kt.EXPORTS): BOOLEAN;
TYPE
  A = ARRAY [0..15] OF CARDINAL;

VAR
  hMapFile      : win.HANDLE;
  lpFile        : win.PVOID;
  plpFile       : POINTER TO win.USHORT;
  pfh           : win.PIMAGE_FILE_HEADER;
  poh           : win.PIMAGE_OPTIONAL_HEADER;
  psh           : POINTER TO ARRAY [0..03FFEH] OF win.IMAGE_SECTION_HEADER;
  ped           : win.PIMAGE_EXPORT_DIRECTORY;
  hfile         : win.HFILE;
  ReOpenBuff    : win.OFSTRUCT;
  nSections     : CARDINAL;
  VAExportDir   : CARDINAL;
  i, j          : CARDINAL;
  ImageHdrOffset: CARDINAL;
  pA            : POINTER TO A;
  pCARDINAL     : POINTER TO CARDINAL;
  table_offset  : POINTER TO ARRAY [0..0FFFEH] OF CARDINAL;
  table_names   : POINTER TO ARRAY [0..0FFFEH] OF sys.ADDRESS;
  table_ordinals: POINTER TO ARRAY [0..0FFFEH] OF sys.CARD16;
  pname         : POINTER TO ARRAY [0..1023] OF CHAR;
  ordinal_base  : CARDINAL;

  MODULE Exit;
  IMPORT win, hfile, lpFile, hMapFile;
  BEGIN
  FINALLY
    (* clean up before exiting *)
    IF hfile # 0 THEN win._lclose (hfile); END;
    IF lpFile # NIL THEN win.UnmapViewOfFile (lpFile); END;
    IF hMapFile # NIL THEN win.CloseHandle (hMapFile); END;
  END Exit;

  PROCEDURE Rel2Abs (sect: CARDINAL; addr: sys.ADDRESS): sys.ADDRESS;
  BEGIN
    RETURN sys.ADDADR(lpFile, CARDINAL(addr)+psh^[sect].PointerToRawData-psh^[sect].VirtualAddress);
  END Rel2Abs;

  PROCEDURE FindSection (addr: CARDINAL; VAR i: CARDINAL): BOOLEAN;
  VAR
    j: CARDINAL;
  BEGIN
    FOR j := 0 TO nSections-1 DO
      IF (psh^[j].VirtualAddress <= addr) AND (addr < psh^[j].VirtualAddress+psh^[j].VirtualSize) THEN
        i := j;
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END FindSection;


BEGIN
  exp := NIL;
  hMapFile := NIL;
  lpFile := NIL;

  hfile := win.OpenFile (full_name, ReOpenBuff, win.OF_READ);
  IF hfile = win.HFILE_ERROR THEN RETURN FALSE; END;

  hMapFile := win.CreateFileMapping (win.HANDLE(hfile), NIL, win.PAGE_READONLY, 0, 0, NIL);
  lpFile := win.MapViewOfFile (hMapFile, win.FILE_MAP_READ, 0, 0, 0);

  plpFile := sys.ADDRESS(lpFile);
  IF plpFile^ # win.IMAGE_DOS_SIGNATURE THEN RETURN FALSE; END;

  pA := sys.ADDRESS(lpFile);
  ImageHdrOffset := pA^[15]+SIZE(CARDINAL);

  pCARDINAL := sys.ADDADR(lpFile, ImageHdrOffset-SIZE(CARDINAL));
  IF pCARDINAL^ # win.IMAGE_NT_SIGNATURE THEN RETURN FALSE; END;

  pfh := win.PIMAGE_FILE_HEADER(sys.ADDADR(lpFile, ImageHdrOffset));
  IF pfh^.SizeOfOptionalHeader = 0 THEN RETURN FALSE; END;

  nSections := pfh^.NumberOfSections;
  IF nSections = 0 THEN RETURN FALSE; END;

  poh := win.PIMAGE_OPTIONAL_HEADER(sys.ADDADR(pfh, SIZE(win.IMAGE_FILE_HEADER)));
  VAExportDir := poh^.DataDirectory[win.IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress;
  psh := sys.ADDADR(poh, pfh^.SizeOfOptionalHeader);

  IF NOT FindSection (VAExportDir, i) THEN RETURN FALSE; END;

  ped := Rel2Abs(i, sys.ADDRESS(VAExportDir));
  IF ped^.NumberOfNames = 0 THEN
    RETURN FALSE;
  END;
  table_offset := Rel2Abs(i, ped^.AddressOfFunctions);
  table_names := Rel2Abs(i, ped^.AddressOfNames);
  table_ordinals := Rel2Abs(i, ped^.AddressOfNameOrdinals);
  ordinal_base := ped^.Base;
  NEW (exp, ped^.NumberOfNames);
  FOR j := 0 TO ped^.NumberOfNames-1 DO
    WITH exp^[j] DO
      IF table_names^[j] = NIL THEN
        fmt.print (name, "@%d", table_ordinals^[j]+ordinal_base);
      ELSE
        pname := Rel2Abs(i, table_names^[j]);
        COPY(pname^, name);
      END;
      offset := table_offset^[table_ordinals^[j]];
      IF NOT FindSection (offset, obj) THEN
        obj := 0;
      ELSE
        DEC(offset, psh^[obj].VirtualAddress);
        INC(obj);
      END;
    END;
  END;

  RETURN TRUE;
EXCEPT
  IF exp # NIL THEN DISPOSE(exp); END;
  RETURN FALSE;
END ReadExport;


END ReadExp.

