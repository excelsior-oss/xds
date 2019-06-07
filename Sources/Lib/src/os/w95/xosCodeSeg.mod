(* Copyright (c) xTech 1995,97.  All Rights Reserved *)

<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xosCodeSeg;

IMPORT
   SYSTEM
  ,OS:=xrtsOS
  ,xWin32
  ;


TYPE
  BYTE  = SYSTEM.CARD8;
  WORD  = SYSTEM.CARD16;
  DWORD = SYSTEM.CARD32;

CONST
  PEDDLen = 16;

TYPE
  PEHeader = RECORD
    Machine: WORD;
    NumberOfSections: WORD;
    TimeDateStamp: DWORD;
    PointerToSymbolTable: DWORD;
    NumberOfSymbols: DWORD;
    SizeOfOptionalHeader: WORD;
    Characteristics: SYSTEM.SET16;
  END;
  pPEHeader = POINTER TO PEHeader;

  PEDataDirEntry = RECORD
    virtAddr: DWORD;
    size: DWORD;
  END;

  PEOptionalHeader = RECORD
    Magic: WORD;
    MajorLinkerVersion: BYTE;
    MinorLinkerVersion: BYTE;
    SizeOfCode: DWORD;
    SizeOfInitializedData: DWORD;
    SizeOfUninitializedData: DWORD;
    AddressOfEntryPoint: DWORD;
    BaseOfCode: DWORD;
    BaseOfData: DWORD;
    ImageBase: DWORD;
    SectionAlignment: DWORD;
    FileAlignment: DWORD;
    MajorOperatingSystemVersion: WORD;
    MinorOperatingSystemVersion: WORD;
    MajorImageVersion: WORD;
    MinorImageVersion: WORD;
    MajorSubsystemVersion: WORD;
    MinorSubsystemVersion: WORD;
    Reserved1: DWORD;
    SizeOfImage: DWORD;
    SizeOfHeaders: DWORD;
    CheckSum: DWORD;
    Subsystem: WORD;
    DllCharacteristics: WORD;
    SizeOfStackReserve: DWORD;
    SizeOfStackCommit: DWORD;
    SizeOfHeapReserve: DWORD;
    SizeOfHeapCommit: DWORD;
    LoaderFlags: DWORD;
    NumberOfRvaAndSizes: DWORD;
    DataDirectory: ARRAY [0..PEDDLen-1] OF PEDataDirEntry;
  END;
  pPEOheader = POINTER TO PEOptionalHeader;


  SectionHeader = RECORD
    Name: ARRAY [0..7] OF CHAR;
    CASE : BOOLEAN OF
      |TRUE : PhysicalAddress: DWORD;
      |FALSE: VirtualSize: DWORD;
    END;
    VirtualAddress: DWORD;
    SizeOfRawData: DWORD;
    PointerToRawData: DWORD;
    PointerToRelocations: DWORD;
    PointerToLinenumbers: DWORD;
    NumberOfRelocations: WORD;
    NumberOfLinenumbers: WORD;
    Characteristics: DWORD;
  END;

PROCEDURE calcCodeExtent ( mh :SYSTEM.ADDRESS; rva :CARDINAL; VAR from, to, sec :CARDINAL );
  VAR
    sig  :POINTER TO ARRAY [0..1] OF CHAR;
    ofs  :POINTER TO CARDINAL;
    ph   :pPEHeader;
    oh   :pPEOheader;
    psh  :POINTER TO ARRAY[0..0FFFFFH] OF SectionHeader; 
    nSec :CARDINAL;
BEGIN
  (* ASSUME: value of a module handle is the address where module is mapped to *)
  sig  := mh;
  IF (sig^ # "MZ") THEN RETURN END;
  
  ofs := SYSTEM.ADDADR( mh, 60 ); IF (ofs^ = 0) THEN RETURN END;
  
  sig := SYSTEM.ADDADR( mh, ofs^ ); IF (sig^ # "PE") THEN RETURN END;
  
  ph := SYSTEM.ADDADR(sig,4);
  IF (ph^.SizeOfOptionalHeader < SIZE(PEOptionalHeader)) THEN RETURN END;
  
  oh  := SYSTEM.ADDADR(ph,SIZE(ph^));
  psh := SYSTEM.ADDADR(oh,SIZE(oh^));
  
  nSec := ph^.NumberOfSections;
  sec  := 0;
  WHILE (sec < nSec) DO
    WITH psh^[sec] DO
       from := CARDINAL(mh)+VirtualAddress; 
       to   := from + SizeOfRawData-1;
       IF (from <= rva) & ( rva <= to) THEN RETURN END;
    END;  
    INC(sec);
  END;
  to   := 0;
END calcCodeExtent;


END xosCodeSeg.