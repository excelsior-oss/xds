<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE xruSTABS;

IMPORT SYSTEM,stdio:=xPOSIX;

TYPE  FILE = POINTER TO stdio.FILE;

VAR MyExe: FILE;

    StabSectionOffset   : CARD32;
    StabSectionSize     : CARD32;
    StabsNumber         : CARD32;
    StabStrSectionOffset: CARD32;
    StabStrSectionSize  : CARD32;

  PROCEDURE GetStabsNumber(): CARD32; BEGIN RETURN StabsNumber END GetStabsNumber;

  PROCEDURE Init;
  BEGIN
    MyExe:=NIL;
    StabSectionOffset   :=0;
    StabSectionSize     :=0;
    StabStrSectionOffset:=0;
    StabStrSectionSize  :=0;
    StabsNumber         :=0;
  END Init;
  
  PROCEDURE Open(VAR Error: BOOLEAN);
  BEGIN
    MyExe:=stdio.fopen("/proc/self/exe","rb");
    IF MyExe=NIL THEN Error:=TRUE; RETURN END;
  END Open;

  PROCEDURE Close;
  BEGIN
    stdio.fclose(MyExe^);
    MyExe:=NIL;
  END Close;

  PROCEDURE Read(VAR Error: BOOLEAN; VAR buff: ARRAY OF SYSTEM.BYTE; from: INTEGER; length: INTEGER);
  BEGIN
    IF 0#stdio.fseek(MyExe^,from,stdio.SEEK_SET) THEN Error:=TRUE; RETURN END;
    IF 1#stdio.fread(SYSTEM.ADR(buff),length,1,MyExe^) THEN Error:=TRUE; RETURN END;
  END Read;

  PROCEDURE ReadString(VAR Error: BOOLEAN; OffsetInFile: INTEGER; VAR name: ARRAY OF CHAR);
  BEGIN
    IF 0#stdio.fseek(MyExe^,OffsetInFile,stdio.SEEK_SET) THEN Error:=TRUE; RETURN END;
    stdio.fread(SYSTEM.ADR(name),HIGH(name),1,MyExe^);
    name[HIGH(name)]:=0C;
  END ReadString;


PROCEDURE OpenMyExe(VAR Error: BOOLEAN);

  VAR ELF_Header: ELF_HEADER;
      SHStrTabOffset: CARD32;

  PROCEDURE ParseFile(VAR Error: BOOLEAN);
    VAR SectionHeader: SECTION_HEADER;
        sname: ARRAY [0..11] OF CHAR;
        i: CARD16;
  BEGIN
    Read(Error, ELF_Header, 0, ELF_HEADER_SIZE); IF Error=TRUE THEN RETURN END;
    IF (ELF_Header.e_type#ET_EXEC) OR (ELF_Header.e_shentsize<SECTION_HEADER_SIZE) OR 
       (ELF_Header.e_shoff=0) OR (ELF_Header.e_shstrndx=0) THEN Error:=TRUE; RETURN END;
    Read(Error, SectionHeader, ELF_Header.e_shoff+ELF_Header.e_shstrndx*ELF_Header.e_shentsize, SECTION_HEADER_SIZE); IF Error=TRUE THEN RETURN END;
    SHStrTabOffset:=SectionHeader.sh_offset;
    FOR i:=1 TO ELF_Header.e_shnum-1 DO
      Read      (Error, SectionHeader, ELF_Header.e_shoff+i*ELF_Header.e_shentsize, SECTION_HEADER_SIZE); IF Error=TRUE THEN RETURN END;
      ReadString(Error, SectionHeader.sh_name+SHStrTabOffset, sname);                                     IF Error=TRUE THEN RETURN END;
      IF sname=".stab" THEN
        StabSectionOffset:=SectionHeader.sh_offset;
        StabSectionSize  :=SectionHeader.sh_size;
        StabsNumber      :=StabSectionSize DIV STAB_SIZE;
      ELSIF sname=".stabstr" THEN
        StabStrSectionOffset:=SectionHeader.sh_offset;
        StabStrSectionSize  :=SectionHeader.sh_size;
      END;
    END;
  END ParseFile;

BEGIN
  Init;
  Error:=FALSE;
  Open(Error);  IF Error THEN RETURN END;
  ParseFile(Error);
  IF Error OR (StabSectionOffset=0) OR (StabSectionSize=0) OR
    (StabStrSectionOffset=0) OR (StabStrSectionSize=0) OR (StabsNumber=0)
  THEN
    Close;
    Error:=TRUE;
    RETURN;
  END
END OpenMyExe;

PROCEDURE CloseMyExe;
BEGIN
   stdio.fclose(MyExe^);
   Init;
END CloseMyExe;


PROCEDURE GetStab(stabstr_off: CARD32; VAR Error: BOOLEAN; indx: CARD32; VAR stab: STAB; VAR name: ARRAY OF CHAR);
BEGIN
  Read(Error, stab, StabSectionOffset+indx*STAB_SIZE, STAB_SIZE); IF Error THEN RETURN END;
  ReadString(Error, stabstr_off+StabStrSectionOffset+stab.n_strx, name); IF Error THEN RETURN END;
END GetStab;


BEGIN
  Init;
END xruSTABS.
