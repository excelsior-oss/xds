IMPLEMENTATION MODULE PrgNames;

<* Storage+ *>

IMPORT sys := SYSTEM;

IMPORT fmt := FormStr;
IMPORT fs  := FileSys;
--IMPORT dll := dllRTS;

IMPORT xs  := xStr;
IMPORT fil := File;
IMPORT red := RedFile;

IMPORT kt  := KrnTypes;
IMPORT dbg := Krn_Dbg;
IMPORT mem := Krn_Mem;

--IMPORT nls := WinNLS;


PROCEDURE SearchProgram (name-: ARRAY OF CHAR; VAR full_name: ARRAY OF CHAR): BOOLEAN;
BEGIN
    RETURN TRUE;
END SearchProgram;



VAR
  FakeNameIndex: CARDINAL;


(* Retrieve name of module from module's open file handle *)
PROCEDURE RetrieveModuleInfo (hFile: PI (*win.HANDLE*); VAR exec_info: kt.EXEC_INFO; get_name: BOOLEAN): CARDINAL;
BEGIN
    RETURN MAX(CARDINAL);
END RetrieveModuleInfo;    


PROCEDURE ConvertUnicode (VAR name: ARRAY OF CHAR; len: CARDINAL);
BEGIN
END ConvertUnicode;


PROCEDURE ReadName (lpImageName: PI (*win.PVOID*); fUnicode: INTEGER(*win.WORD*); VAR name: ARRAY OF CHAR): BOOLEAN;
BEGIN
  RETURN TRUE;
END ReadName;


BEGIN
  FakeNameIndex := 1;
END PrgNames.