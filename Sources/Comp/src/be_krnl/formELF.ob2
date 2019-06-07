
MODULE formELF;

<* ALIGNMENT = "1" *>

IMPORT
    sys := SYSTEM,
    cmd := CodeDef,
    nms := ObjNames,
    env := xiEnv,
    at:=opAttrs,
    str := Strings,
    fc  := CodeFace,
    io  := opIO,
    dbg := DbgFace,
    opt := Options,
    reg := Registry,
    pc  := pcK;

<* IF DBG_DWARF THEN *>
IMPORT ddw := dbgDWARF;
<* END *>

IMPORT Printf;


VAR
  -- Print debug information
  print_dbg_info: BOOLEAN;

PROCEDURE print (f: ARRAY OF CHAR; SEQ args: sys.BYTE);
BEGIN
  IF print_dbg_info THEN
    Printf.printf (f, args);
  END;
END print;


TYPE CODE_SEGM = cmd.CODE_SEGM;
     OBJECT    = fc.OBJECT;
     STRING    = fc.STRING;

(*
 *********************************************
 *         General ELF description           *
 *********************************************
*)

  TYPE INT32  = sys.INT32;
       CARD32 = sys.CARD32;
       CARD16 = sys.CARD16;
       CARD8  = sys.CARD8;
       UCHAR  = CARD8;

(*
    ELF HEADER descritption
    -----------------------
*)
    CONST
      EI_CLASS        = 4;
      EI_DATA         = 5;
      EI_VERSION      = 6;
      EI_NIDENT       = 16;            -- Size of e_ident

      ELFCLASS32      = 1;             -- 32bit architecture
      ELFDATA2LSB     = 1;             -- little-endian byte order
      ELFDATA2MSB     = 2;             -- big-endian byte order
      EV_CURRENT      = 1;             -- Current ELF version
      ET_REL          = 1;             -- Relocatable object file
      EM_386          = 3;             -- Intel 386 machine
      EM_PPC          = 20;            -- PPC machine
      ELF_HEADER_SIZE = 034H;

    TYPE ELF_HEADER = RECORD
           e_ident     : ARRAY EI_NIDENT OF UCHAR;
           e_type      : CARD16;
           e_machine   : CARD16;
           e_version   : CARD32;
           e_entry     : CARD32;
           e_phoff     : CARD32;
           e_shoff     : CARD32;
           e_flags     : CARD32;
           e_ehsize    : CARD16;
           e_phentsize : CARD16;
           e_phnum     : CARD16;
           e_shentsize : CARD16;
           e_shnum     : CARD16;
           e_shstrndx  : CARD16;
         END;

    CONST
      ALIGN_PARA = 16;

(*
    ELF SECTIONs descritption
    -------------------------
*)
    CONST
      SECTION_HEADER_SIZE = 028H;

    CONST                    -- Section Header Table Indexes
      SHN_UNDEF      = 0;
      SHN_LORESERVE  = 0FF00H;
      SHN_ABS        = 0FFF1H;
      SHN_COMMON     = 0FFF2H;
      SHN_HIRESERVE  = 0FFFFH;

    CONST               -- Setion Header types
      SHT_NULL     = 0;
      SHT_PROGBITS = 1;
      SHT_SYMTAB   = 2;
      SHT_STRTAB   = 3;
      SHT_RELA     = 4;
      SHT_NOTE     = 7;
      SHT_NOBITS   = 8;
      SHT_REL      = 9;

    CONST                -- Section Attribute flags
      SHF_WRITE     = 1;
      SHF_ALLOC     = 2;
      SHF_EXECINSTR = 4;

    TYPE SECTION_HEADER = RECORD
           sh_name      : CARD32;
           sh_type      : CARD32;
           sh_flags     : CARD32;
           sh_addr      : CARD32;
           sh_offset    : CARD32;
           sh_size      : INT32;
           sh_link      : CARD32;
           sh_info      : CARD32;
           sh_addralign : CARD32;
           sh_entsize   : CARD32;
         END;


(*
    SYMBOL TABLE descritption
    -------------------------
*)
    CONST
      SYMBOL_SIZE = 010H;

    CONST             -- Symbol's binding
      STB_LOCAL  = 0;
      STB_GLOBAL = 1;
      STB_WEAK   = 2;

    CONST              -- Symbol's type
      STT_NOTYPE  = 0;
      STT_OBJECT  = 1;
      STT_FUNC    = 2;
      STT_SECTION = 3;
      STT_FILE    = 4;

    TYPE SYMBOL = RECORD
           st_name  : CARD32;
           st_value : CARD32;
           st_size  : CARD32;
           st_info  : UCHAR;
           st_other : UCHAR;
           st_shndx : CARD16;
         END;


(*
    RELOCATION ENTRIES (Fixups) descritption
    ----------------------------------------
*)
    CONST             -- Relocation types
  <* IF TARGET_RISC THEN *>
      R_PPC_NONE   = 0;
      R_PPC_ADDR32 = 1;
      R_PPC_ADDR16_LO = 4;
      R_PPC_ADDR16_HI = 5;
      R_PPC_ADDR16_HA = 6;
      R_PPC_REL24  = 10;
  <* ELSE *>
      R_386_NONE = 0;
      R_386_32   = 1;
      R_386_PC32 = 2;
  <* END *>


    CONST
      REL_SIZE  = 8;
      RELA_SIZE = 12;

    TYPE REL = RECORD
           r_offset : CARD32;
           r_info   : CARD32;
         END;

    TYPE RELA = RECORD
           r_offset : CARD32;
           r_info   : CARD32;
           r_addend : INT32;
         END;

    --** These depends on type of relocation, currently used :

<* IF TARGET_RISC THEN *>

(* let me tell you guys, i hate gnu binutils, cause when you work on i386
binutils crashes on RELA but works on REL, and when PPC-EABI is the target
binutils crashes on REL but works on RELA. this stuff is gnubinutils bug
workaround... *)

    CONST RELOCATION_SIZE = RELA_SIZE;
          SHT_RELOCATION  = SHT_RELA;
     TYPE RELOCATION      = RELA;
<* ELSE *>
    CONST RELOCATION_SIZE = REL_SIZE;
          SHT_RELOCATION  = SHT_REL;
     TYPE RELOCATION      = REL;
<* END *>
    CONST MIN_RELSTAB_SIZE = RELOCATION_SIZE*2; -- Contains relocations for source file name, and its end-of-file

(*
    Debugging symbols (STABS) descritption
    --------------------------------------
*)
    CONST STAB_SIZE = 12;
          N_UNDF  = 0;    -- N_* are STAB types
          N_SO    = 064H;
          N_SLINE = 044H;
          N_FUN   = 024H;

    TYPE STAB = RECORD
           n_strx : CARD32;
           n_type : CARD8;
           n_other: CARD8;
           n_desc : CARD16;
           n_value: CARD32;
         END;


(*
 *********************************************
 *          Custom file description          *
 *********************************************
*)

    VAR Header  : ELF_HEADER;
        SHN_max : INT32;

    CONST
      SHN_FIRST_USED                =  1;
      -- always write next sections
      SHN_TEXT                      =  1; S_TEXT_NAME                     = ".text";
      SHN_DATA                      =  2; S_DATA_NAME                     = ".data";
      SHN_RODATA                    =  3; S_RODATA_NAME                   = ".rodata";
      SHN_BSS                       =  4; S_BSS_NAME                      = ".bss";
      SHN_COMMENT                   =  5; S_COMMENT_NAME                  = ".comment";
      SHN_SYMTAB                    =  6; S_SYMTAB_NAME                   = ".symtab";
      SHN_SHSTRTAB                  =  7; S_SHSTRTAB_NAME                 = ".shstrtab";
      SHN_STRTAB                    =  8; S_STRTAB_NAME                   = ".strtab";
      SHN_RELOCATION_TEXT           =  9; S_RELOCATION_TEXT_NAME          = ".rel.text";
      SHN_RELOCATION_DATA           = 10; S_RELOCATION_DATA_NAME          = ".rel.data";
      SHN_RELOCATION_RODATA         = 11; S_RELOCATION_RODATA_NAME        = ".rel.rodata";
      -- write next sections only if debug info is STAB and LINENO
      SHN_STAB                      = 12; S_STAB_NAME                     = ".stab";
      SHN_STABSTR                   = 13; S_STABSTR_NAME                  = ".stabstr";
      SHN_RELOCATION_STAB           = 14; S_RELOCATION_STAB_NAME          = ".rel.stab";
      -- write next sections only if debug info is DWARF and LINENO
      SHN_DEBUG_LINE                = 15; S_DEBUG_LINE_NAME               = ".debug_line";
      SHN_RELOCATION_DEBUG_LINE     = 16; S_RELOCATION_DEBUG_LINE_NAME    = ".rel.debug_line";
      SHN_DEBUG_ARANGES             = 17; S_DEBUG_ARANGES_NAME            = ".debug_aranges";
      SHN_RELOCATION_DEBUG_ARANGES  = 18; S_RELOCATION_DEBUG_ARANGES_NAME = ".rel.debug_aranges";
      -- write next sections only if debug info is DWARF and GENDEBUG
      SHN_DEBUG_INFO                = 19; S_DEBUG_INFO_NAME               = ".debug_info";
      SHN_DEBUG_ABBREV              = 20; S_DEBUG_ABBREV_NAME             = ".debug_abbrev";
      SHN_DEBUG_PUBNAMES            = 21; S_DEBUG_PUBNAMES_NAME           = ".debug_pubnames";
      SHN_RELOCATION_DEBUG_INFO     = 22; S_RELOCATION_DEBUG_INFO_NAME    = ".rel.debug_info";
      -- last section number
      SHN_LAST_USED                 = SHN_RELOCATION_RODATA;
      SHN_LAST_USED_STAB            = SHN_RELOCATION_STAB;
---&&&    SHN_LAST_USED_DWARF_LINENO    = SHN_DEBUG_LINE;
--&&&     SHN_LAST_USED_DWARF_LINENO    = SHN_RELOCATION_DEBUG_LINE;
      SHN_LAST_USED_DWARF_LINENO    = SHN_RELOCATION_DEBUG_ARANGES;
      SHN_LAST_USED_DWARF_DEBUG     = SHN_RELOCATION_DEBUG_INFO;
      SHN_LAST                      = SHN_LAST_USED_DWARF_DEBUG;

TYPE
    ArrayShnOfArrayOfChar = ARRAY SHN_LAST+1 OF ARRAY 19 OF CHAR;
CONST
    SHN_NAME = ArrayShnOfArrayOfChar {
        "",
        ".text",
        ".data",
        ".rodata",
        ".bss",
        ".comment",
        ".symtab",
        ".shstrtab",
        ".strtab",
        ".rel.text",
        ".rel.data",
        ".rel.rodata",

        ".stab",
        ".stabstr",
        ".rel.stab",

        ".debug_line",
        ".rel.debug_line",
        ".debug_aranges",
        ".rel.debug_aranges",

        ".debug_info",
        ".debug_abbrev",
        ".debug_pubnames",
        ".rel.debug_info"
        };
                     






VAR
  GENDEBUG     : BOOLEAN;
  LINENO       : BOOLEAN;
  dbgFMT       : SHORTINT;
  CountSection : CARD16;

 CONST
   dbgFMT_None  = 0;
   dbgFMT_STAB  = 1;
   dbgFMT_DWARF = 2;



    PROCEDURE GetRelocSec(sec: CARD16): CARD16;
    BEGIN
      CASE sec OF
        SHN_TEXT   : RETURN SHN_RELOCATION_TEXT   |
        SHN_DATA   : RETURN SHN_RELOCATION_DATA   |
        SHN_RODATA : RETURN SHN_RELOCATION_RODATA |
        SHN_STAB   : RETURN SHN_RELOCATION_STAB   |
      ELSE           RETURN SHN_UNDEF;
      END;
    END GetRelocSec;



    TYPE
      PROCESS_ONE_SECTION = PROCEDURE (i: INT32);

    PROCEDURE IterateSections (p: PROCESS_ONE_SECTION);
    VAR
      i: INTEGER;
    BEGIN
      print (">>> IterateSections\n");
      -- always write next sections
      FOR i := SHN_UNDEF TO SHN_LAST_USED DO
        p (i);
      END;
      CASE dbgFMT OF
      | dbgFMT_STAB:
        IF GENDEBUG THEN
          -- write next sections only if debug info is STAB
          p (SHN_STAB);
          p (SHN_STABSTR);
          p (SHN_RELOCATION_STAB);
	END;  
      | dbgFMT_DWARF:
        IF LINENO THEN
          -- write next sections only if debug info is DWARF and LINENO
          p (SHN_DEBUG_LINE);
          p (SHN_RELOCATION_DEBUG_LINE);
--&&&
          p (SHN_DEBUG_ARANGES);
--&&&
          p (SHN_RELOCATION_DEBUG_ARANGES);
        END;
        IF GENDEBUG THEN
          -- write next sections only if debug info is DWARF and GENDEBUG
          p (SHN_DEBUG_INFO);
          p (SHN_DEBUG_ABBREV);
          p (SHN_DEBUG_PUBNAMES);
          p (SHN_RELOCATION_DEBUG_INFO);
        END;
      | dbgFMT_None:
        -- nothing to do
      END;
      print ("<<< IterateSections\n");
    END IterateSections;



    PROCEDURE CountOneSection (i: INT32);
    BEGIN
      CountSection := CountSection+1;
    END CountOneSection;

    PROCEDURE CountSections;
    BEGIN
      CountSection := 0;
      IterateSections (CountOneSection);
    END CountSections;


    PROCEDURE InitHeader();
      PROCEDURE flags(): CARD32;
        BEGIN
        <* IF TARGET_RISC THEN *>IF at.ABI IN {at.eabi} THEN RETURN 80000000H END; <* END *>
         RETURN 0; 
      END flags;
    BEGIN
      Header.e_ident[0]          := 07FH;
      Header.e_ident[1]          := ORD('E');
      Header.e_ident[2]          := ORD('L');
      Header.e_ident[3]          := ORD('F');
      Header.e_ident[EI_CLASS]   := ELFCLASS32;
      Header.e_ident[EI_DATA]    := <* IF TARGET_RISC THEN *> ELFDATA2MSB <* ELSE *> ELFDATA2LSB <* END *>;
      Header.e_ident[EI_VERSION] := EV_CURRENT;
      Header.e_type              := ET_REL;
      Header.e_machine           := <* IF TARGET_RISC THEN *> EM_PPC <* ELSE *> EM_386 <* END *>;
      Header.e_version           := EV_CURRENT;
      Header.e_entry             := 0;
      Header.e_phoff             := 0;
      Header.e_shoff   := 0;  -- will take the value in CountSectionOffsets()
      Header.e_flags             := flags();
      Header.e_ehsize            := ELF_HEADER_SIZE;
      Header.e_phentsize         := 0;
      Header.e_phnum             := 0;
      Header.e_shentsize         := SECTION_HEADER_SIZE;
      Header.e_shnum             := CountSection;
      Header.e_shstrndx          := SHN_SHSTRTAB;
    END InitHeader;

    VAR Sections: ARRAY SHN_LAST_USED_DWARF_DEBUG+1 OF SECTION_HEADER;

      PROCEDURE InitSections();
        VAR i: LONGINT;
      BEGIN
        FOR i:=SHN_UNDEF TO SHN_max DO
          Sections[i].sh_addr      := 0;
          Sections[i].sh_offset    := 0;  -- sh_offset & sh_size will be counted
          Sections[i].sh_size      := 0;  -- aside InitSections()
          Sections[i].sh_info      := 0;
          Sections[i].sh_entsize   := 0;
          Sections[i].sh_flags     := 0;
          Sections[i].sh_link      := SHN_UNDEF;
          CASE i OF
            SHN_UNDEF:
                          Sections[i].sh_type      := SHT_NULL;
                          Sections[i].sh_addralign := 0;          |
            SHN_TEXT:
                          Sections[i].sh_type      := SHT_PROGBITS;
                          Sections[i].sh_flags     := SHF_ALLOC + SHF_EXECINSTR;
                          Sections[i].sh_addralign := 010H;       |
            SHN_DATA:
                          Sections[i].sh_type      := SHT_PROGBITS;
                          Sections[i].sh_flags     := SHF_ALLOC + SHF_WRITE;
                          Sections[i].sh_addralign := 4;          |
            SHN_RODATA:
                          Sections[i].sh_type      := SHT_PROGBITS;
                          Sections[i].sh_flags     := SHF_ALLOC;
                          Sections[i].sh_addralign := 4;          |
            SHN_BSS:
                          Sections[i].sh_type      := SHT_NOBITS;
                          Sections[i].sh_flags     := SHF_ALLOC + SHF_WRITE;
                          Sections[i].sh_addralign := 4;          |
            SHN_COMMENT:
                          Sections[i].sh_type      := SHT_PROGBITS;
                          Sections[i].sh_addralign := 1;          |
            SHN_SYMTAB:
                          Sections[i].sh_type      := SHT_SYMTAB;
                          Sections[i].sh_link      := SHN_STRTAB;
                          Sections[i].sh_addralign := 4;
                          Sections[i].sh_entsize   := SYMBOL_SIZE;|
            SHN_SHSTRTAB:
                          Sections[i].sh_type      := SHT_STRTAB;
                          Sections[i].sh_addralign := 1;          |
            SHN_STRTAB:
                          Sections[i].sh_type      := SHT_STRTAB;
                          Sections[i].sh_addralign := 1;          |
            SHN_RELOCATION_TEXT:
                          Sections[i].sh_type      := SHT_RELOCATION;
                          Sections[i].sh_link      := SHN_SYMTAB;
                          Sections[i].sh_info      := SHN_TEXT;
                          Sections[i].sh_addralign := 4;
                          Sections[i].sh_entsize   := RELOCATION_SIZE;  |
            SHN_RELOCATION_DATA:
                          Sections[i].sh_type      := SHT_RELOCATION;
                          Sections[i].sh_link      := SHN_SYMTAB;
                          Sections[i].sh_info      := SHN_DATA;
                          Sections[i].sh_addralign := 4;
                          Sections[i].sh_entsize   := RELOCATION_SIZE;  |
            SHN_RELOCATION_RODATA:
                          Sections[i].sh_type      := SHT_RELOCATION;
                          Sections[i].sh_link      := SHN_SYMTAB;
                          Sections[i].sh_info      := SHN_RODATA;
                          Sections[i].sh_addralign := 4;
                          Sections[i].sh_entsize   := RELOCATION_SIZE;  |
            SHN_STAB:
                          IF (dbgFMT = dbgFMT_STAB) THEN
                            Sections[i].sh_type      := SHT_PROGBITS;
                            Sections[i].sh_link      := SHN_STABSTR;
                            Sections[i].sh_entsize   := STAB_SIZE;
                            Sections[i].sh_addralign := 4;
                          END;            |

            SHN_STABSTR:
                          IF (dbgFMT = dbgFMT_STAB) THEN
                            Sections[i].sh_type      := SHT_STRTAB;
                            Sections[i].sh_addralign := 1;
                          END;
                                      |
            SHN_RELOCATION_STAB:
                          IF (dbgFMT = dbgFMT_STAB) THEN
                            Sections[i].sh_type      := SHT_RELOCATION;
                            Sections[i].sh_link      := SHN_SYMTAB;
                            Sections[i].sh_info      := SHN_STAB;
                            Sections[i].sh_addralign := 4;
                            Sections[i].sh_entsize   := RELOCATION_SIZE;
                          END;    |

            SHN_DEBUG_INFO:
                          Sections[i].sh_type      := SHT_PROGBITS;
                          Sections[i].sh_addralign := 1;          |
            SHN_DEBUG_LINE:
                          Sections[i].sh_type      := SHT_PROGBITS;
                          Sections[i].sh_flags     := SHF_ALLOC;
                          Sections[i].sh_addralign := 1;          |
            SHN_DEBUG_ABBREV:
                          Sections[i].sh_type      := SHT_PROGBITS;
                          Sections[i].sh_addralign := 1;          |
            SHN_DEBUG_ARANGES:
                          Sections[i].sh_type      := SHT_PROGBITS;
                          Sections[i].sh_addralign := 1;          |
            SHN_DEBUG_PUBNAMES:
                          Sections[i].sh_type      := SHT_PROGBITS;
                          Sections[i].sh_addralign := 1;          |
            SHN_RELOCATION_DEBUG_INFO:
                          Sections[i].sh_type      := SHT_RELOCATION;
                          Sections[i].sh_link      := SHN_SYMTAB;
                          Sections[i].sh_info      := SHN_DEBUG_INFO;
                          Sections[i].sh_addralign := 4;
                          Sections[i].sh_entsize   := RELOCATION_SIZE;  |
            SHN_RELOCATION_DEBUG_LINE:
                          Sections[i].sh_type      := SHT_RELOCATION;
                          Sections[i].sh_link      := SHN_SYMTAB;
                          Sections[i].sh_info      := SHN_DEBUG_LINE;
                          Sections[i].sh_addralign := 4;
                          Sections[i].sh_entsize   := RELOCATION_SIZE;  |
            SHN_RELOCATION_DEBUG_ARANGES:
                          Sections[i].sh_type      := SHT_RELOCATION;
                          Sections[i].sh_link      := SHN_SYMTAB;
                          Sections[i].sh_info      := SHN_DEBUG_ARANGES;
                          Sections[i].sh_addralign := 4;
                          Sections[i].sh_entsize   := RELOCATION_SIZE;
          ELSE
          END;
        END;

        Sections[SHN_STRTAB  ].sh_size:= 1; -- Zerro name is always there, other names will be counted later
        Sections[SHN_SYMTAB  ].sh_size:= SYMBOL_SIZE; -- Have to contain an empty symbol
      END InitSections;


(*
 *********************************************
 *          Objects attributes               *
 *********************************************
*)

    PROCEDURE set_adr(o: OBJECT; symtab_idx: CARD32; name_idx: CARD32;
                                 offs: CARD32; sec: CARD16);
    BEGIN
      fc.set_adr(o,symtab_idx,name_idx,offs,VAL(CARD32,sec),VAL(CARD32,0),VAL(CARD32,0));
    END set_adr;

    PROCEDURE get_adr(o: OBJECT; VAR symtab_idx: CARD32; VAR name_idx: CARD32;
                                 VAR offs: CARD32; VAR sec: CARD16);
      VAR dummy,sec1: CARD32;
    BEGIN
      fc.get_adr(o,symtab_idx,name_idx,offs,sec1,dummy,dummy);
      sec:=VAL(CARD16,sec1);
    END get_adr;

(*
 *********************************************
 *          Objects classification           *
 *********************************************
*)
    PROCEDURE TargetSection (o: OBJECT): CARD16;
    BEGIN
      IF fc.ObjectStatus(o) = fc.status_Extern THEN
         RETURN SHN_UNDEF;
      ELSE
        CASE fc.ObjectClass(o) OF
          fc.class_Text   : RETURN SHN_TEXT   |
          fc.class_Data   : RETURN SHN_DATA   |
          fc.class_ROData : RETURN SHN_RODATA |
          fc.class_BSS    : RETURN SHN_BSS    |
        END;
      END
    END TargetSection;

    PROCEDURE SymbolType(o: OBJECT): UCHAR;
    BEGIN
      CASE TargetSection(o) OF
        SHN_TEXT                      : RETURN STT_FUNC   |
        SHN_DATA, SHN_RODATA, SHN_BSS : RETURN STT_OBJECT |
      ELSE
        RETURN STT_NOTYPE;
      END;
    END SymbolType;

    PROCEDURE RelocationType(kind: SHORTINT): UCHAR;
       -- Gives relocation type by fixup kind.
    BEGIN
      CASE kind OF
    <* IF TARGET_RISC THEN *>
        cmd.fx_obj32  : RETURN R_PPC_ADDR32    |
        cmd.fx_lo16   : RETURN R_PPC_ADDR16_LO |
        cmd.fx_hi16   : RETURN R_PPC_ADDR16_HA | --???
        cmd.fx_rel24  : RETURN R_PPC_REL24     |
    <* ELSE *>
        cmd.fx_obj32  : RETURN R_386_32   |
        cmd.fx_relcall: RETURN R_386_PC32 |
    <* END *>
      END;
    END RelocationType;

   VAR
     LocalsNamed, DoRevert: BOOLEAN;


   PROCEDURE Revert(VAR d: ARRAY 4 OF sys.BYTE);
     VAR t: sys.BYTE;
   BEGIN
     t:=d[0]; d[0]:=d[3]; d[3]:=t;
     t:=d[1]; d[1]:=d[2]; d[2]:=t;
   END Revert;

(*
 *********************************************
 *          Context Iterator                 *
 *********************************************
*)
    TYPE
      iter_proc_type   = fc.iter_proc_type;
      filter_proc_type = fc.filter_proc_type;
    VAR FirstNonLocal: BOOLEAN;

      PROCEDURE filter_locals (o: OBJECT):BOOLEAN;
      BEGIN
        RETURN fc.ObjectStatus(o)=fc.status_Local;
      END filter_locals;

      PROCEDURE filter_nonlocals (o: OBJECT):BOOLEAN;
      BEGIN
        RETURN fc.ObjectStatus(o)#fc.status_Local;
      END filter_nonlocals;

    PROCEDURE IterateContext(iterator: iter_proc_type; user_filter: filter_proc_type);
    BEGIN
      FirstNonLocal:=FALSE;
      fc.IterateContext(iterator,filter_locals,user_filter);
      FirstNonLocal:=TRUE;
      fc.IterateContext(iterator,filter_nonlocals,user_filter);
    END IterateContext;

(*
    Set of filters for context iteration
    ------------------------------------
*)
    PROCEDURE FilterCode(o: OBJECT): BOOLEAN;
    BEGIN
      RETURN TargetSection(o)=SHN_TEXT;
    END FilterCode;

    PROCEDURE FilterData(o: OBJECT): BOOLEAN;
    BEGIN
      RETURN TargetSection(o)=SHN_DATA;
    END FilterData;

    PROCEDURE FilterROData(o: OBJECT): BOOLEAN;
    BEGIN
      RETURN TargetSection(o)=SHN_RODATA;
    END FilterROData;

(*
 *********************************************
 *          Object Allocation                *
 *********************************************
*)

      PROCEDURE SourceFileNameSize():CARD32;
        VAR s: ARRAY 512 OF CHAR;
      BEGIN
        fc.SourceFileName(s);
        RETURN LENGTH(s)+1;
      END SourceFileNameSize;

      PROCEDURE ObjectName(o: OBJECT; VAR name: ARRAY OF CHAR);
      BEGIN
        IF (NOT LocalsNamed) AND (fc.ObjectStatus(o)=fc.status_Local) THEN
          name[0]:=0X;
        ELSE
          fc.ObjectName(o,name);
        END;
      END ObjectName;

      PROCEDURE ObjectNameSize(o: OBJECT):LONGINT;
        VAR s: ARRAY 512 OF CHAR;
            l: LONGINT;
      BEGIN
        ObjectName(o,s);
        l:=LENGTH(s);
        IF l # 0 THEN l:=l+1 END;
        RETURN l;
      END ObjectNameSize;

      PROCEDURE XrefSize(o: OBJECT): LONGINT;
        VAR rsegm: CODE_SEGM;
            Size,i,predof,predln,of,ln,pos: LONGINT;
            fname: STRING;
            xrefs: cmd.XREFs;
      BEGIN
        rsegm := cmd.get_ready(o);
        IF rsegm.xref=NIL THEN RETURN 0 END;
        ASSERT(rsegm.xref_len#0);
        xrefs:=rsegm.xref;
        Size:=0; predln:=-1; predof:=-1;
        FOR i:=0 TO rsegm.xref_len-1 DO
          xrefs[i].txtpos.unpack(fname, ln, pos);
          of := xrefs[i].offs;
          IF (ln # predln) & (of # predof) THEN
            predln := ln;
            predof := of;
            INC(Size,STAB_SIZE)
          END;
        END ;
        RETURN Size;
      END XrefSize;

      PROCEDURE StabFuncName(o: OBJECT; VAR s: ARRAY OF CHAR);
      BEGIN
        ASSERT(SymbolType(o)=STT_FUNC);
        fc.ObjectName(o,s);
        CASE fc.ObjectStatus(o) OF
          fc.status_Local  : str.Append(":f",s) |
          fc.status_Global : str.Append(":F",s) |
        ELSE ASSERT(FALSE,543);
        END;
      END StabFuncName;

      PROCEDURE StabFuncNameSize(o: OBJECT):CARD32;
        VAR s: ARRAY 512 OF CHAR;
      BEGIN
        StabFuncName(o,s);
        RETURN LENGTH(s)+1;
      END StabFuncNameSize;

      PROCEDURE MakeSectionAlign(sec: CARD16);
      BEGIN
        fc.MakeAlign(Sections[sec].sh_size, Sections[sec].sh_addralign)
      END MakeSectionAlign;

    VAR BeginTextSectionObject: OBJECT;

    PROCEDURE Alloc(o: OBJECT);
      VAR symtab_idx,name_idx,offset: CARD32;
          sec, rel_sec              : CARD16;
          name_size                 : LONGINT;

    BEGIN
      symtab_idx:=Sections[SHN_SYMTAB].sh_size DIV SYMBOL_SIZE;
      Sections[SHN_SYMTAB].sh_size:= Sections[SHN_SYMTAB].sh_size + SYMBOL_SIZE;

      name_idx:=0;
      name_size:=ObjectNameSize(o);
      IF name_size#0 THEN
        name_idx:=Sections[SHN_STRTAB].sh_size;
        Sections[SHN_STRTAB].sh_size:= Sections[SHN_STRTAB].sh_size + name_size;
      END;

      offset:=0;
      sec:=TargetSection(o);
      IF sec # SHN_UNDEF THEN
        MakeSectionAlign(sec);
        offset:=Sections[sec].sh_size;
        Sections[sec].sh_size:=Sections[sec].sh_size+fc.ObjectSize(o);
      END;

      rel_sec:=GetRelocSec(sec);
      IF (rel_sec # SHN_UNDEF) THEN
        Sections[rel_sec].sh_size:=Sections[rel_sec].sh_size + fc.FixupsNumber(o) * RELOCATION_SIZE;
      END;

      set_adr(o,symtab_idx,name_idx,offset,sec);

      IF (sec=SHN_TEXT) AND (dbgFMT = dbgFMT_STAB) AND GENDEBUG THEN  -- xrefs only for functions
        INC(Sections[SHN_STAB].sh_size, XrefSize(o)+STAB_SIZE); -- one is for function and others are for xrefs
        INC(Sections[SHN_STABSTR].sh_size, StabFuncNameSize(o));
        INC(Sections[SHN_RELOCATION_STAB].sh_size, RELOCATION_SIZE); -- exactly one relocation per object - for function. All xrefs are given relatively to the function they are included by
        IF BeginTextSectionObject=NIL THEN BeginTextSectionObject:=o END;
      END;

      IF FirstNonLocal THEN
        FirstNonLocal:=FALSE;
        Sections[SHN_SYMTAB].sh_info:=symtab_idx;
      END;
    END Alloc;

    PROCEDURE Allocate();
    BEGIN
      IF (dbgFMT = dbgFMT_STAB) AND GENDEBUG THEN
        Sections[SHN_STAB            ].sh_size:= STAB_SIZE*3; -- Contains pseudo entry, source file name, and its end-of-file
        Sections[SHN_RELOCATION_STAB ].sh_size:= MIN_RELSTAB_SIZE;
        Sections[SHN_STABSTR         ].sh_size:= 1 + 2*SourceFileNameSize(); -- 1 for null string, and EOF doesn't have a name
        BeginTextSectionObject:=NIL;
      END;
      IterateContext(Alloc,NIL);
    END Allocate;

    PROCEDURE isSectionLive(i: INT32):BOOLEAN;
    BEGIN
      RETURN ~((((i=SHN_RELOCATION_DATA) OR (i=SHN_RELOCATION_RODATA))
                  AND(Sections[i].sh_size=0))
                 OR
                ((i=SHN_RELOCATION_STAB)AND(Sections[i].sh_size=MIN_RELSTAB_SIZE))
                );
    END isSectionLive;

    PROCEDURE CountSectionOffsets();
      VAR i,offset: LONGINT;
    BEGIN
      offset:=ELF_HEADER_SIZE;
      FOR i:=SHN_UNDEF TO SHN_max DO
        IF isSectionLive(i) THEN
          fc.MakeAlign(offset,16); -- Align each section to paragraph
          Sections[i].sh_offset:=offset;
          IF i#SHN_BSS THEN     -- BSS occupies no space in file
            offset:=offset + Sections[i].sh_size;
          END;
        END;
      END;
      fc.MakeAlign(offset,16);
      Header.e_shoff:= offset;
      Sections[SHN_UNDEF].sh_offset:=0;  -- shouldn't hold actual offset.
    END CountSectionOffsets;

(*
 *********************************************
 *            Object Writing                 *
 *********************************************
*)

    CONST out = fc.out;

    PROCEDURE out4(d: ARRAY 4 OF sys.BYTE);
    BEGIN
      IF DoRevert THEN Revert(d) END;
      out(d,4);
    END out4;

    PROCEDURE out2(d: ARRAY 2 OF sys.BYTE);
    BEGIN
      IF DoRevert THEN
        out(d[1],1);
        out(d[0],1);
      ELSE
        out(d,2);
      END;
    END out2;

    PROCEDURE ins4(pos: CARD32; d: ARRAY 4 OF sys.BYTE);
    BEGIN
      IF DoRevert THEN Revert(d) END;
      fc.ins (pos, d, 4);
    END ins4;


    PROCEDURE AlignPara;
    BEGIN
      fc.align_file(ALIGN_PARA);
    END AlignPara;


    PROCEDURE PrintHeader;
    BEGIN
      print (">>> PrintHeader\n");
      print ("  e_entry=%$8X\n", Header.e_entry);
      print ("  e_phoff=%$8X\n", Header.e_phoff);
      print ("  e_shoff=%$8X\n", Header.e_shoff);
      print ("  e_shnum=%d\n", Header.e_shnum);
      print ("  e_shstrindx=%$8X\n", Header.e_shstrndx);
      print ("<<< PrintHeader\n");
    END PrintHeader;


    PROCEDURE WriteHeader;
    BEGIN
      PrintHeader;
      out(Header.e_ident,EI_NIDENT);
      out2(Header.e_type);
      out2(Header.e_machine);
      out4(Header.e_version);
      out4(Header.e_entry);
      out4(Header.e_phoff);
      out4(Header.e_shoff);
      out4(Header.e_flags);
      out2(Header.e_ehsize);
      out2(Header.e_phentsize);
      out2(Header.e_phnum);
      out2(Header.e_shentsize);
      out2(Header.e_shnum);
      out2(Header.e_shstrndx);
    END WriteHeader;



    PROCEDURE PrintOneSectionTable (i: INT32);
    BEGIN
      print ("   section=%d\n", i);
      print ("     type=%d\n", Sections[i].sh_type);
      print ("     flag=%$8X\n", Sections[i].sh_flags);
      print ("     addr=%$8X\n", Sections[i].sh_addr);
      print ("     offs=%$8X\n", Sections[i].sh_offset);
      print ("     size=%d\n", Sections[i].sh_size);
      print ("     link=%d\n", Sections[i].sh_link);
      print ("     info=%d\n", Sections[i].sh_info);
      print ("     alig=%d\n", Sections[i].sh_addralign);
      print ("     ensz=%$8X\n", Sections[i].sh_entsize);
    END PrintOneSectionTable;

    PROCEDURE PrintSectionHeaderTable;
    BEGIN
      print (">>> PrintSectionrTable\n");
      IterateSections (PrintOneSectionTable);
      print ("<<< PrintSectionTable\n");
    END PrintSectionHeaderTable;


    VAR name_write_idx: LONGINT;

    PROCEDURE WriteOneSectionHeaderTable (i: INT32);
    BEGIN
      IF isSectionLive(i) THEN
        out4(Sections[i].sh_name);
        out4(Sections[i].sh_type);
        out4(Sections[i].sh_flags);
        out4(Sections[i].sh_addr);
        out4(Sections[i].sh_offset);
        out4(Sections[i].sh_size);
        out4(Sections[i].sh_link);
        out4(Sections[i].sh_info);
        out4(Sections[i].sh_addralign);
        out4(Sections[i].sh_entsize);
      END;
    END WriteOneSectionHeaderTable;

    PROCEDURE AllocOneSectionHeaderTable (i: INT32);
    BEGIN
      IF isSectionLive(i) THEN
        Sections[i].sh_name      := name_write_idx;
        INC(name_write_idx,LENGTH(SHN_NAME[i])+1);
      END;
    END AllocOneSectionHeaderTable;

    PROCEDURE WriteSectionHeaderTable();
    BEGIN
      print (">>> WriteSectionHeaderTable\n");
      IterateSections (WriteOneSectionHeaderTable);
      print ("<<< WriteSectionHeaderTable\n");
    END WriteSectionHeaderTable;

    PROCEDURE AllocSHStrTab();
    BEGIN
      name_write_idx:=0;
      IterateSections (AllocOneSectionHeaderTable);
      Sections[SHN_SHSTRTAB].sh_size:=name_write_idx;
    END AllocSHStrTab;

    PROCEDURE WriteToSection(o: OBJECT);
      VAR rsegm: CODE_SEGM;

      PROCEDURE MakeAddends();
        VAR  add, inf:                  INT32;
             CodeAddr, off4app, fx_adr: LONGINT;
             i:                         INTEGER;
      BEGIN
        IF rsegm.fxup=NIL THEN RETURN END;
        FOR i:=0 TO rsegm.fxup_len-1 DO
          add     :=rsegm.fxup[i].fx_offs;
          ASSERT ( (add=0) OR (RelocationType(rsegm.fxup[i].kind)=<* IF TARGET_RISC THEN *>R_PPC_ADDR32<*ELSE*>R_386_32<*END*>));
           (* fx_offs isn't zerro only for CASE statement, in this case fixup should point to the same object i.e. to the object, fixup is applied to *)
          off4app :=sys.VAL(LONGINT,rsegm.fxup[i].offs);
          CodeAddr:=sys.VAL(LONGINT,sys.ADR(rsegm.bcode[0]));
          fx_adr := CodeAddr + off4app;
          sys.GET(fx_adr, inf);
          IF DoRevert THEN Revert(inf) END;
          inf := inf + add;
        <* IF NOT TARGET_RISC THEN *> IF RelocationType(rsegm.fxup[i].kind) = R_386_PC32 THEN inf:=inf-4 END; <* END *>
          IF DoRevert THEN Revert(inf) END;
          sys.PUT(fx_adr, inf);
          rsegm.fxup[i].fx_offs:=0;
        END;
      END MakeAddends;

    BEGIN
      fc.align_file(Sections[TargetSection(o)].sh_addralign);
      rsegm:=cmd.get_ready(o);
      MakeAddends;   -- explicit addend
      out(rsegm.bcode^,rsegm.code_len);
    END WriteToSection;

    PROCEDURE WriteToRelSection(o: OBJECT);
      VAR rsegm: CODE_SEGM;  i: LONGINT;
          obj_offset,z1,z2: CARD32;
          z3: CARD16;
      PROCEDURE WriteFixup(fx: cmd.fixup_desc);
        VAR info,symtab_idx,offs_in_sec: CARD32;
      BEGIN
        get_adr(fx.obj,symtab_idx,z1,z2,z3);
        offs_in_sec:=VAL(CARD32,fx.offs)+obj_offset;
        out4(offs_in_sec);
        info:=symtab_idx*256+RelocationType(fx.kind);
        out4(info);
        <* IF TARGET_RISC THEN *> out4(sys.VAL(CARD32,0)); <* END *>
      END WriteFixup;
    BEGIN
      get_adr(o,z1,z2,obj_offset,z3);
      rsegm:=cmd.get_ready(o);
      IF (rsegm.fxup=NIL) THEN RETURN END;
      FOR i:=0 TO rsegm.fxup_len-1 DO
        WriteFixup(rsegm.fxup[i]);
      END;
    END WriteToRelSection;

    PROCEDURE WriteToSymTab(o: OBJECT);
      VAR symtab_idx,name,offs,size : CARD32;
          sec : CARD16;
          c   : UCHAR;
      PROCEDURE MakeSymInfo(VAR c: UCHAR);
      BEGIN
       c:=SymbolType(o);
       CASE fc.ObjectStatus(o) OF
         fc.status_Global, fc.status_Extern : c:=c+16  |
         fc.status_Local                    :          |
       END;
      END MakeSymInfo;
    BEGIN
     get_adr(o, symtab_idx, name, offs, sec);
     out4(name);
     out4(offs);
     size:=fc.ObjectSize(o);
     out4(size);
     MakeSymInfo(c);
     out(c,1);
     c:=0;
     out(c,1);
     out2(sec);
    END WriteToSymTab;


    PROCEDURE WriteString (s: ARRAY OF CHAR);
    BEGIN
      out (s, LENGTH(s)+1);
    END WriteString;

    PROCEDURE WriteOneSHStrTab (i: INT32);
    BEGIN
        IF isSectionLive(i) THEN
          WriteString (SHN_NAME[i]);
        END;
    END WriteOneSHStrTab;

    PROCEDURE WriteSHStrTab;
    BEGIN
       -- each strtab must contain leading zerro
--      WriteString("");
      print (">>> WriteSHStrTab\n");
      IterateSections (WriteOneSHStrTab);
      print ("<<< WriteSHStrTab\n");
    END WriteSHStrTab;

    PROCEDURE WriteToStrTab(o: OBJECT);
      VAR name: ARRAY 255 OF CHAR;
          size: LONGINT;
    BEGIN
      size:=ObjectNameSize(o);
      IF size#0 THEN
        ObjectName(o,name);
        out(name,size);
      END
    END WriteToStrTab;

    PROCEDURE WriteComment;
    BEGIN END WriteComment;

    PROCEDURE WriteEmptySymbol;
      VAR s: ARRAY SYMBOL_SIZE OF CHAR;
          i: INTEGER;
    BEGIN
      FOR i:=0 TO LEN(s)-1 DO s[i]:=0X END;
      out(s,LEN(s));
    END WriteEmptySymbol;


      VAR CurrStabStrIdx: CARD32;
      PROCEDURE WrSt(strx: CARD32; type: CARD8; other: CARD8; desc: CARD16; value: CARD32);
      BEGIN
        out4(strx);
        out(type,1);
        out(other,1);
        out2(desc);
        out4(value);
      END WrSt;

      PROCEDURE WriteToStab(o: OBJECT);
        VAR rsegm: CODE_SEGM;  of,n,ln,pos,i,predln,predof: LONGINT;
            xrefs: cmd.XREFs;
            fname: STRING;

      BEGIN
        rsegm:=cmd.get_ready(o);
        IF (rsegm.xref=NIL) THEN RETURN END;
        xrefs:=rsegm.xref;
        n := rsegm.xref_len;  ASSERT(n # 0);
        xrefs[0].txtpos.unpack(fname, ln, pos);
        WrSt(CurrStabStrIdx,N_FUN,0,sys.VAL(CARD16,ln+1),0);
          CurrStabStrIdx:=CurrStabStrIdx + StabFuncNameSize(o);

        predln:=-1; predof:=-1;
        FOR i := 0 TO n-1 DO
          of:=xrefs[i].offs;
          xrefs[i].txtpos.unpack(fname, ln, pos);
          IF (ln # predln) & (of # predof) THEN
            WrSt(0,N_SLINE,0,sys.VAL(CARD16,ln+1),of);
            predln := ln;
            predof := of;
          END;
        END;
      END WriteToStab;


    PROCEDURE WriteStab;
    BEGIN
      CurrStabStrIdx:=1;
      WrSt(CurrStabStrIdx,N_UNDF,0,sys.VAL(CARD16,Sections[SHN_STAB].sh_size DIV STAB_SIZE), Sections[SHN_STABSTR].sh_size);
        CurrStabStrIdx:=CurrStabStrIdx + SourceFileNameSize();
      WrSt(CurrStabStrIdx,N_SO,0,0,0);
        CurrStabStrIdx:=CurrStabStrIdx + SourceFileNameSize();
      IterateContext(WriteToStab, FilterCode);
      WrSt(0,N_SO,0,0,Sections[SHN_TEXT].sh_size);
    END WriteStab;


      PROCEDURE WriteToStabStr(o: OBJECT);
        VAR name: ARRAY 512 OF CHAR;
      BEGIN
        StabFuncName(o,name);
        out(name,StabFuncNameSize(o));
      END WriteToStabStr;

    PROCEDURE WriteStabStr;
      VAR s: ARRAY 256 OF CHAR;

    BEGIN
      s[0]:=0X;
      out(s,1);
      fc.SourceFileName(s);
      out(s,SourceFileNameSize());
      out(s,SourceFileNameSize());
      IterateContext(WriteToStabStr, FilterCode);
    END WriteStabStr;


      VAR CurrStabIdx,Add: CARD32;
      PROCEDURE WriteToRelStab(o: OBJECT);
        VAR info,symtab_idx,z1,z2: CARD32;
            z3: CARD16; 
      BEGIN
        get_adr(o, symtab_idx, z1, z2, z3);
        out4(CurrStabIdx);
        CurrStabIdx:=CurrStabIdx+STAB_SIZE+Add*VAL(CARD32,XrefSize(o));
        info:=symtab_idx*256 + <* IF TARGET_RISC THEN *> R_PPC_ADDR32 <* ELSE *> R_386_32 <* END *>;;
        out4(info);
        <* IF TARGET_RISC THEN *> out4(sys.VAL(CARD32,0)); <* END *>
      END WriteToRelStab;

    PROCEDURE WriteRelStab;
    BEGIN
      CurrStabIdx:=20;
--      WriteToRelStab(BeginTextSectionObject);
      Add:=0;
      WriteToRelStab(BeginTextSectionObject);
      Add:=1;
      IterateContext(WriteToRelStab, FilterCode);
      Add:=0;
      WriteToRelStab(BeginTextSectionObject);
    END WriteRelStab;


    PROCEDURE WriteDebugInfo_STAB;
    BEGIN
      ASSERT(Sections[SHN_STAB].sh_offset = VAL(CARD32, fc.get_pos()));
      WriteStab;                                                 AlignPara;
      ASSERT(Sections[SHN_STABSTR].sh_offset = VAL(CARD32, fc.get_pos()));
      WriteStabStr;                                              AlignPara;
      IF isSectionLive(SHN_RELOCATION_STAB) THEN
        ASSERT(Sections[SHN_RELOCATION_STAB].sh_offset = VAL(CARD32, fc.get_pos()));
        WriteRelStab;                                              AlignPara;
      END;
    END WriteDebugInfo_STAB;


    --------->>>>>>>>>>> D E B U G   I N F O <<<<<<<<<<<<<------------
   <* IF DBG_DWARF THEN *>

    CONST
      LINE_BASE   = 1;   -- возможное изменение номера строки в тексте
      LINE_RANGE  = 14;  -- макс. диапазон изменений
      OPCODE_BASE = 10;

      DW_LNS_copy             = 1;
      DW_LNS_advance_pc       = 2;
      DW_LNS_advance_line     = 3;
      DW_LNS_set_file         = 4;
      DW_LNS_set_column       = 5;
      DW_LNS_negate_stmt      = 6;
      DW_LNS_set_bacic_block  = 7;
      DW_LNS_const_add_pc     = 8;
      DW_LNS_fixed_advance_ps = 9;

      DW_LNE_end_sequence     = 1;
      DW_LNE_set_address      = 2;
      DW_LNE_define_file      = 3;
    TYPE
      PLTABENTRY = POINTER TO LTABENTRY;
      PFNAMEREC  = POINTER TO FNAMEREC;

      FNAMEREC = RECORD
        name:    ARRAY 512 OF CHAR;  -- имя исходного файла
        tree:    PLTABENTRY;         -- дерево, упорядоченное по адресам
        next:    PFNAMEREC;
      END;

      LTABENTRY = RECORD
        line: INT32;
        offs: CARD32;
        o   : OBJECT;
        obj_offs: CARD32;
        left, right: PLTABENTRY;
      END;

    VAR
      -- fnamelist : список (по файлам) деревьев LTABENTRY, упорядоченных
      -- по адресам, повторяющиеся строки выкидываются.
      fnamelist       : PFNAMEREC;


    -- Добавление новой структуры LTABENTRY в список деревьев
    -- если объект левее всех в дереве - вернет его PFNAMEREC, иначе - NIL
    PROCEDURE addLEntry (line: INT32; offs: CARD32; fname :ARRAY OF CHAR; o : OBJECT; obj_offs: CARD32);
    VAR
      curfile : PFNAMEREC;
      -- для имени файла вернет указатель на его PFNAMEREC (если надо - заведет)
      PROCEDURE get_fname(fname : ARRAY OF CHAR) : PFNAMEREC;
      VAR
        p : PFNAMEREC;
        PROCEDURE strcmp(s1,s2: ARRAY OF CHAR): BOOLEAN;
        VAR i : INT32;
        BEGIN
          FOR i:=0 TO LENGTH(s1) DO IF (s1[i]#s2[i]) THEN RETURN FALSE; END; END;
          RETURN TRUE;
        END strcmp;
      VAR i: INT32;
      BEGIN
        p := fnamelist;
        WHILE (p # NIL) DO
          IF (strcmp(p.name, fname)) THEN RETURN p; END;
          p := p.next;
        END;
        NEW(p);
        FOR i:=0 TO LENGTH(fname) DO p.name[i]:=fname[i]; END;
        p.tree    := NIL;
        p.next    := fnamelist;
        fnamelist := p;
        RETURN p;
      END get_fname;
      -- заводит новую запись LTABENTRY
      PROCEDURE new_ltabentry():PLTABENTRY;
      VAR p : PLTABENTRY;
      BEGIN
        NEW(p);
        p.line     := line;
        p.offs     := offs;
        p.o        := o;
        p.obj_offs := obj_offs;
        p.left     := NIL;
        p.right    := NIL;
        RETURN p;
      END new_ltabentry;
      -- рекурсивно ищет в дереве с корнем в 'p' место и, если такого адреса нет,
      -- вешает туда новую LTABENTRY
      PROCEDURE app_tree(p : PLTABENTRY);
      BEGIN
        IF (offs < p.offs) THEN
          IF (p.left # NIL) THEN app_tree(p.left);
          ELSE                   p.left := new_ltabentry(); END;
        ELSE
          IF (offs > p.offs) THEN
            IF (p.right # NIL) THEN app_tree(p.right);
            ELSE                   p.right := new_ltabentry(); END;
          END;
        END;
      END app_tree;
    BEGIN
      curfile := get_fname(fname);
      IF (curfile.tree = NIL) THEN
        curfile.tree := new_ltabentry();
      ELSE
        app_tree(curfile.tree);
      END;
    END addLEntry;

    PROCEDURE outLEB128 (val: sys.CARD32);
    VAR
      b: sys.CARD8;
    BEGIN
      print ("outLEB (val=%d)\n", val);
      REPEAT
        b   := VAL(sys.CARD8, val MOD 80H);
        val := val DIV 80H;
        IF val # 0 THEN b := b + VAL(sys.CARD8,80H); END;
        cmd.GenByte(b);
      UNTIL val=0;
    END outLEB128;

    PROCEDURE outSLEB128(val: sys.INT32);
    VAR
      b     : sys.CARD8;
      enough: BOOLEAN;
    BEGIN
      print ("outSLEB (val=%d)\n", val);
      enough := FALSE;
      REPEAT
        b   := VAL(sys.CARD8, val MOD 80H);
        val := val DIV 80H;
        IF ((val = 0) AND (b < 40H)) OR ((val = -1) AND (b >= 40H)) THEN
          enough := TRUE;
        ELSE
          b := b + VAL(sys.CARD8, 80H);
        END;
        cmd.GenByte(b);
      UNTIL enough;
    END outSLEB128;

    PROCEDURE gen_tree(tree: PLTABENTRY; VAR addr: CARD32; VAR line: INT32; offs_base: CARD32);

      PROCEDURE gen_opcode(dl: INT32; da: CARD32): BOOLEAN;
      VAR
        opcode: INT32;
      BEGIN
        RETURN FALSE;
        --???? как все это делать ????
        print ("gen_opcode (da=%$8X; dl=%d)\n", da, dl);
        IF (dl>=0) AND (dl <= LINE_BASE+LINE_RANGE-1) THEN
          opcode := dl-LINE_BASE+VAL(INT32, da)*LINE_RANGE;
          IF (0 <= opcode) AND (opcode <= 255) THEN
            cmd.GenByte (VAL(sys.INT8, opcode));
            RETURN TRUE;
          END;
        END;
        RETURN FALSE;
      END gen_opcode;

    VAR
      dl : INT32;
      da : CARD32;
    BEGIN
      IF (tree = NIL) THEN RETURN; END;
      print (">>> gen_tree (addr=%$8X; line=%d)\n", addr, line);
      print ("  left tree:\n");
      gen_tree(tree.left, addr, line, offs_base);
      print ("  root tree=(offs=%$8X; line=%d; obj_offs=%$8X)\n", tree.offs, tree.line, tree.obj_offs);
      dl := tree.line-line;
      da := tree.obj_offs+tree.offs-offs_base-addr;
      print ("  da=%$8X; dl=%d\n", da, dl);
      IF NOT gen_opcode (dl, da) THEN
        cmd.GenByte (DW_LNS_advance_line); -- DW_LNS_advance_line
        outSLEB128(dl);
        cmd.GenByte (DW_LNS_advance_pc);   -- DW_LNS_advance_pc
        outLEB128(da);
        cmd.GenByte (DW_LNS_copy);         -- строка таблицы - в установленных регистрах.
      END;
      INC(line, dl);
      addr := addr+da;
      print ("  right tree:\n");
      gen_tree(tree.right, addr, line, offs_base);
      print ("<<< gen_tree\n", addr, line);
    END gen_tree;


    -- вызывается для каждого объекта
    PROCEDURE iterLines(o: OBJECT);
    VAR
      line, col, oline, offs: INT32;
      x:         INT32;
      fname:     pc.STRING;
      ofname:    pc.STRING;
      attr:      at.ATTR_EXT;
      s:         cmd.CODE_SEGM;
      obj_offs, c1, c2, c4, c5, c6 : CARD32;
    BEGIN
      IF (o.mode # pc.ob_module) AND NOT (o.mode IN pc.PROCs) THEN RETURN; END;
      attr := at.attr (o.ext, cmd.a_ready_code);
      IF attr = NIL THEN RETURN; END;
      s := attr(cmd.CODE_SEGM);
      fc.get_adr(o, c1, c2, obj_offs, c4, c5, c6);
      x := 0; oline := -2; ofname := NIL;
      WHILE x < s.xref_len DO
        offs := s.xref[x].offs;
        REPEAT INC (x); UNTIL (x >= s.xref_len) OR (offs # s.xref[x].offs);
        DEC (x);
        s.xref[x].txtpos.unpack (fname, line, col);
        IF (line # oline) OR (fname # ofname) AND (fname # NIL) THEN
          addLEntry(line+1, offs, fname^, o, obj_offs);
        END;
        ofname := fname; oline := line;
        INC (x);
      END;
    END iterLines;

    PROCEDURE genLineInfo();
    -- Тут мы заполняем изначально пустые сегменты для .debug_line и .debug_aranges
    -- (пока .debug_aranges остается пустым. Возможно, он и не нужен (see DWARF Book p.51))
    VAR
      segm_old    : CODE_SEGM;
      lseg,seg    : CODE_SEGM;
      pf          : PFNAMEREC;
      i,line      : INT32;
      ltail,rtail : PLTABENTRY;
      offs_base   : CARD32;
      addr, aadv  : CARD32;
    BEGIN
      -- see DWARF DI Format, p.54 $6.2.3, 6.2.4
      print (">>> genLineInfo\n");
      cmd.get_segm(segm_old);
      lseg := ddw.aSegments[ddw.SEG_DEBUG_LINE];
      cmd.set_segm(lseg);
      fc.IterateContext(iterLines,NIL,NIL); -- строится fnamelist
      IF (fnamelist = NIL) THEN RETURN; END;
      cmd.GenLWord(0);            -- total_length (пропишем позднее)
      cmd.GenWord (2);            -- version
      cmd.GenLWord(0);            -- prologue_length (пропишем позднее)
      cmd.GenByte (1);            -- minimum_instruction_length
      cmd.GenByte (1);            -- default_is_stmt
      cmd.GenByte (LINE_BASE);    -- line_base
      cmd.GenByte (LINE_RANGE);   -- line_range
      cmd.GenByte (OPCODE_BASE);  -- opcode_base
      -- standard_opcode_lengths (number of LEB128 arguments for 1..(OPCODE_BASE=10)-1 standard opcodes)
      outLEB128(0);               -- stand. opcode 1 (DW_LNS_copy)
      outLEB128(1);               -- stand. opcode 2 (DW_LNS_advance_pc)
      outLEB128(1);               -- stand. opcode 3 (DW_LNS_advance_line)
      outLEB128(1);               -- stand. opcode 4 (DW_LNS_set_file)
      outLEB128(1);               -- stand. opcode 5 (DW_LNS_set_column)
      outLEB128(0);               -- stand. opcode 6 (DW_LNS_negate_stmt)
      outLEB128(0);               -- stand. opcode 7 (DW_LNS_set_basic_block)
      outLEB128(0);               -- stand. opcode 8 (DW_LNS_const_add_pc)
      outLEB128(1);               -- stand. opcode 9 (DW_LNS_fixed_advance_pc) (1?? Там не LEB128, а uhalf. Ладно, мы ее все равно не используем.)

      cmd.GenByte(0);             -- include_directories table (empty)
      pf := fnamelist;
      WHILE (pf # NIL) DO         -- write file names table
        FOR i := 0 TO LENGTH(pf.name)-1 DO cmd.GenByte(pf.name[i]) END;
        cmd.GenByte(0);
        outLEB128(0); --directory  : none
        outLEB128(0); --modif. time: ignore
        outLEB128(0); --file length: ignore
        pf := pf.next;
      END;
      cmd.GenByte(0); --file_names : done

      -- write prologue_length value
      i := lseg.code_len - 10; -- кол-во байт ПОСЛЕ prologue_length до начала кода
      cmd.move4b (sys.ADR (i),
                  sys.ADR (lseg.bcode[6]),
                  DoRevert);
      -- поехали...
      pf := fnamelist;
      i  := 1; -- source file counter
      WHILE (pf # NIL) DO
        ltail     := pf.tree; WHILE (ltail.left  # NIL) DO ltail := ltail.left;  END;
        rtail     := pf.tree; WHILE (rtail.right # NIL) DO rtail := rtail.right; END;
        offs_base := ltail.obj_offs; -- оффсет самого левого  объекта в дереве
        print ("  fname=%s\n", pf.name);
        print ("  ltail=(%$8X, %d)\n", ltail.offs, ltail.line);
        print ("  rtail=(%$8X, %d)\n", rtail.offs, rtail.line);
        print ("  offs_base=%$8X\n", offs_base);

        cmd.GenByte (DW_LNS_set_file);              -- DW_LNS_set_file
        outLEB128(i);                               -- source file number

        cmd.GenByte (0);                            -- 0 - start ext. opcode
        outLEB128(5);                               -- ext. opcode size = 5
        cmd.GenByte (DW_LNE_set_address);           -- DW_LNE_set_address
        cmd.gen_fixup(ltail.o, 0, cmd.fx_obj32);    -- фиксап на начало самого левого объекта в дереве
--&&&        cmd.GenLWord (0); -- вместо фиксапа

        addr := 0;
        line := 0;
        gen_tree(pf.tree, addr, line, offs_base);   -- выписываем дерево в таблицу

        seg  := cmd.get_ready(rtail.o);
        aadv := VAL(CARD32,seg.code_len) + rtail.obj_offs; -- адрес после последнего байта последнего листа дерева
        IF (offs_base+addr < aadv) THEN                    --   IF (он после указателя адреса) (* так должно быть всегда.. *)
          aadv := aadv - (offs_base+addr);
          cmd.GenByte (DW_LNS_advance_pc);     -- DW_LNS_advance_pc
          outLEB128(aadv);
        END;
        cmd.GenByte (0);                       -- 0 - start ext. opcode
        outLEB128(1);                          -- ext. opcode size = 1
        cmd.GenByte (DW_LNE_end_sequence);     -- DW_LNE_end_sequence
        INC(i);
        pf := pf.next;
      END;

      -- Write total_length header field
      i := lseg.code_len - 4;
      cmd.move4b (sys.ADR (i), sys.ADR (lseg.bcode[0]), cmd.inverse_byte_order);

      cmd.set_segm(segm_old);
      print ("<<< genLineInfo\n");
    END genLineInfo;


    PROCEDURE WriteDebugInfo_DWARF;
    (*
      Тут мы вызываем dbg.generate, после чего будут видны сегменты
        ddw.aSegments[ddw.SEG_DEBUG_INFO]     - готовый .debug_info
        ddw.aSegments[ddw.SEG_DEBUG_ABBREV]   - готовый .debug_abbrev
        ddw.aSegments[ddw.SEG_DEBUG_ARANGES]  - пустой, создадим тут
        ddw.aSegments[ddw.SEG_DEBUG_LOC]      - пустой (пропустим?)
        ddw.aSegments[ddw.SEG_DEBUG_LINE]     - пустой, создадим тут
        ddw.aSegments[ddw.SEG_DEBUG_PUBNAMES] - готовый .debug_pubnames
        ddw.aSegments[ddw.SEG_DEBUG_STR]      - готовый .debug_str (там - пустая табл. символов)
        ddw.aSegments[ddw.SEG_DEBUG_MACINFO]  - пустой (пропустим?)

       Теперь нам следует заполнить
        ddw.aSegments[ddw.SEG_DEBUG_LINE] и ddw.aSegments[ddw.SEG_DEBUG_ARANGES],
        затем - записать все в файл, записать фиксапные секции,
        поправить таблицу размещения сегментов (размеры этих сегментов были раньше
        неизвестны), поправить в уже записанном в файл хеадере позицию этой таблицы,
        сама таблица пропишется позднее.
        Желательно переписать геморрой с предварительным рассчетом размеров сегментов..
    *)

      PROCEDURE wr_dbg_sec(cseg : CODE_SEGM; section, relsection : INT32);
      -- Пишет содержимое сегмента cseg в секцию section, если там есть фиксапы, то пишет для них секцию relsection
      -- В таблице секций пропишет sh_offset и sh_size

        PROCEDURE WriteToSection(cseg : CODE_SEGM; section : INT32);

          PROCEDURE MakeAddends();
          -- Применяет к сегменту имеющиеся фиксапы, а в самих фиксапах
          -- обнуляет fx_offs
          VAR add, inf, i:  INT32;
              fx_adr: sys.ADDRESS;
          BEGIN
            print (">>> MakeAddends\n");
            IF cseg.fxup # NIL THEN
              FOR i:=0 TO cseg.fxup_len-1 DO
                add    :=cseg.fxup[i].fx_offs;
                fx_adr := sys.ADR(cseg.bcode[cseg.fxup[i].offs]);
                inf := 0;
                cmd.move4b (fx_adr, sys.ADR (inf), DoRevert);
                INC(inf, add);
               <* IF NOT TARGET_RISC THEN *>
                IF RelocationType(cseg.fxup[i].kind) = R_386_PC32 THEN
                  inf:=inf-4;
                END;
               <* END *>
                cmd.move4b (sys.ADR(inf), fx_adr, DoRevert);
                cseg.fxup[i].fx_offs:=0;
              END;
            END;
            print ("<<< MakeAddends\n");
          END MakeAddends;

        BEGIN
          print (">>> WriteToSection\n");
          AlignPara;
          Sections[section].sh_offset := fc.get_pos();
          Sections[section].sh_size   := cseg.code_len;
          MakeAddends;   -- explicit addend
          out(cseg.bcode^,cseg.code_len);
          AlignPara;
          print ("<<< WriteToSection\n");
        END WriteToSection;

        PROCEDURE WriteToRelSection(cseg : CODE_SEGM; relsection : INT32);
        VAR info,symtab_idx,offs_in_sec,z1,z2: CARD32;
            z3:                                CARD16;
            i:                                 INT32;
        BEGIN
          --&&& чтобы не писать .rel. секции
          --RETURN;
          IF (cseg.fxup#NIL) THEN
            ASSERT(relsection >= 0);
          ELSIF relsection < 0 THEN
            RETURN;
          END;
          AlignPara;
          Sections[relsection].sh_offset := VAL(CARD32, fc.get_pos());
          Sections[relsection].sh_size   := 0;
          IF (cseg.fxup=NIL) THEN RETURN; END;
          FOR i:=0 TO cseg.fxup_len-1 DO
            get_adr(cseg.fxup[i].obj,symtab_idx,z1,z2,z3);
            offs_in_sec:=VAL(CARD32,cseg.fxup[i].offs);
            out4(offs_in_sec);
            info:=symtab_idx*256+RelocationType(cseg.fxup[i].kind);
            out4(info);
            <* IF TARGET_RISC THEN *>
            out4(sys.VAL(CARD32,0));
            <* END *>
          END;
          Sections[relsection].sh_size := VAL(CARD32,fc.get_pos())-Sections[relsection].sh_offset;
          AlignPara;
        END WriteToRelSection;

      BEGIN --wr_dbg_sec--
        print (">>> wr_dbg_sec\n");
        WriteToSection (cseg, section);
        WriteToRelSection (cseg, relsection);
        print ("<<< wr_dbg_sec\n");
      END wr_dbg_sec;


      -- поправим в файле оффсет таблицы сегментов
      PROCEDURE correct_offset;
      BEGIN
        print (">>> correct_offset\n");
        print ("  old offest=%$8X\n", Header.e_shoff);
        Header.e_shoff := fc.get_pos();
        ASSERT(Header.e_shoff MOD ALIGN_PARA = 0);
        print ("  new offest=%$8X\n", Header.e_shoff);
        ins4 ( SIZE(Header.e_ident)
             + SIZE(Header.e_type)
             + SIZE(Header.e_machine)
             + SIZE(Header.e_version)
             + SIZE(Header.e_entry)
             + SIZE(Header.e_phoff), Header.e_shoff);
        fc.set_pos(Header.e_shoff);
        print ("<<< correct_offset\n");
      END correct_offset;


      PROCEDURE WriteLineNumSections;
      BEGIN
        print (">>> WriteLineNumSections\n");
        IF LINENO THEN
          cmd.new_segm(ddw.aSegments[ddw.SEG_DEBUG_LINE]);
          cmd.new_segm(ddw.aSegments[ddw.SEG_DEBUG_ARANGES]);
          genLineInfo();
          wr_dbg_sec(ddw.aSegments[ddw.SEG_DEBUG_LINE],    SHN_DEBUG_LINE,    SHN_RELOCATION_DEBUG_LINE);
--&&&          wr_dbg_sec(ddw.aSegments[ddw.SEG_DEBUG_ARANGES], SHN_DEBUG_ARANGES, SHN_RELOCATION_DEBUG_ARANGES);
--&&& ассоциированныя секция с debug_line
          correct_offset;
        END;
        print ("<<< WriteLineNumSections\n");
      END WriteLineNumSections;


      PROCEDURE WriteDebugInfoSections;
        CONST
          OBJ_EXT = ".obj";

        VAR
          name: ARRAY 256 OF CHAR;
          ext : ARRAY 16 OF CHAR;
          s   : pc.STRING;
      BEGIN
        print (">>> WriteDebugInfoSections\n");
        IF GENDEBUG THEN
          -- определим name и ext объектного фалйа
          COPY(at.curr_mod.name^,name);
          env.config.Equation("OBJEXT", s);
          IF s = NIL THEN
            COPY(OBJ_EXT, ext);
          ELSE
            IF s[0] = '.' THEN
              ext[0] := 0X;
            ELSE
              ext[0] := '.';
              ext[1] := 0X;
            END;
            str.Append(s^, ext);
          END;
          IF dbg.generate (name, ext, TRUE, at.DbgNestedProc IN at.COMP_MODE) THEN END;
          wr_dbg_sec(ddw.aSegments[ddw.SEG_DEBUG_ABBREV],   SHN_DEBUG_ABBREV,   -1);
          wr_dbg_sec(ddw.aSegments[ddw.SEG_DEBUG_PUBNAMES], SHN_DEBUG_PUBNAMES, -1);
          wr_dbg_sec(ddw.aSegments[ddw.SEG_DEBUG_INFO],     SHN_DEBUG_INFO,     SHN_RELOCATION_DEBUG_INFO);
          correct_offset;
        END;
        print ("<<< WriteDebugInfoSections\n");
      END WriteDebugInfoSections;

    BEGIN
      print (">>> WriteDebug\n");
      WriteLineNumSections;
      WriteDebugInfoSections;
      print ("<<< WriteDebug\n");
    END WriteDebugInfo_DWARF;
   <* END *>

    --------->>>>>>>>>>> E N D   O F   D E B U G   I N F O <<<<<<<<<<<<<------------

    PROCEDURE Write();
      VAR s: ARRAY 2 OF CHAR;
    BEGIN
      s[0]:=0X;
      WriteHeader; AlignPara;
      -- always write next sections
      ASSERT(Sections[SHN_TEXT].sh_offset = VAL(CARD32, fc.get_pos()));
      IterateContext(WriteToSection,    FilterCode);               AlignPara;
      ASSERT(Sections[SHN_DATA].sh_offset = VAL(CARD32, fc.get_pos()));
      IterateContext(WriteToSection,    FilterData);               AlignPara;
      ASSERT(Sections[SHN_RODATA].sh_offset = VAL(CARD32, fc.get_pos()));
      IterateContext(WriteToSection,    FilterROData);             AlignPara;
      -- but no write BSS section
      ASSERT(Sections[SHN_COMMENT].sh_offset = VAL(CARD32, fc.get_pos()));
      WriteComment;                                                AlignPara;
      ASSERT(Sections[SHN_SYMTAB].sh_offset = VAL(CARD32, fc.get_pos()));
      WriteEmptySymbol; IterateContext(WriteToSymTab, NIL);        AlignPara;
      ASSERT(Sections[SHN_SHSTRTAB].sh_offset = VAL(CARD32, fc.get_pos()));
      WriteSHStrTab;                                               AlignPara;
      ASSERT(Sections[SHN_STRTAB].sh_offset = VAL(CARD32, fc.get_pos()));
      out(s,1); IterateContext(WriteToStrTab,NIL);                 AlignPara;
      ASSERT(Sections[SHN_RELOCATION_TEXT].sh_offset = VAL(CARD32, fc.get_pos()));
      IterateContext(WriteToRelSection, FilterCode);               AlignPara;
      IF isSectionLive(SHN_RELOCATION_DATA) THEN
        ASSERT(Sections[SHN_RELOCATION_DATA].sh_offset = VAL(CARD32, fc.get_pos()));
        IterateContext(WriteToRelSection, FilterData);               AlignPara;
      END;
      IF isSectionLive(SHN_RELOCATION_RODATA) THEN
        ASSERT(Sections[SHN_RELOCATION_RODATA].sh_offset = VAL(CARD32, fc.get_pos()));
        IterateContext(WriteToRelSection, FilterROData);             AlignPara;
      END;

      IF (dbgFMT = dbgFMT_STAB) AND GENDEBUG THEN
        -- write next sections only if debug info is STAB
        WriteDebugInfo_STAB;
      END;

     <* IF DBG_DWARF THEN *>
      IF (dbgFMT = dbgFMT_DWARF) THEN
        -- write next sections only if debug info is DWARF and LINENO
        -- write next sections only if debug info is DWARF and GENDEBUG
        WriteDebugInfo_DWARF;
      END;
     <* END *>

      ASSERT(Header.e_shoff = VAL(CARD32, fc.get_pos()));
      WriteSectionHeaderTable;
      PrintSectionHeaderTable;
    END Write;

PROCEDURE CorrectHeader;
  PROCEDURE CorrectOneFixupSectionEntry(i:CARD32);
  VAR j:INT32;
  BEGIN
     IF ~isSectionLive(i) THEN
        FOR j:=i+1 TO SHN_max DO
	    IF 	Sections[j].sh_link > i THEN
                Sections[j].sh_link      := Sections[j].sh_link-1;
	    END;	
	    IF 	Sections[j].sh_info > i THEN
                Sections[j].sh_info      := Sections[j].sh_info-1;
	    END;
	END;
        Header.e_shnum:=Header.e_shnum-1;
     END;
  END CorrectOneFixupSectionEntry;
BEGIN
  CorrectOneFixupSectionEntry(SHN_RELOCATION_STAB);
  CorrectOneFixupSectionEntry(SHN_RELOCATION_RODATA);
  CorrectOneFixupSectionEntry(SHN_RELOCATION_DATA);
END CorrectHeader;

TYPE
  FORM_ELF *= POINTER TO formELF_rec;
  formELF_rec *= RECORD (fc.formobj_rec)
                  END;

PROCEDURE (this: FORM_ELF) generate*;
VAR
      item: reg.ITEM;
      dbg_is_set: BOOLEAN;
BEGIN
      print_dbg_info:=env.config.Option("PRINT_DBG_INFO");
      dbg_is_set := this.SetDebugFormat();
      GENDEBUG := fc.GenDebug() & dbg_is_set;
      LINENO := GENDEBUG OR fc.GenLineno();
      item := reg.GetActive(opt.dbgFormat);
      IF item = NIL THEN
        dbgFMT := dbgFMT_None;
      ELSIF item.name = opt.dbg_STAB THEN
        dbgFMT := dbgFMT_STAB;
      ELSIF item.name = opt.dbg_DWARF THEN
        dbgFMT := dbgFMT_DWARF;
      ELSE
        dbgFMT := dbgFMT_None;
      END;
      SHN_max := SHN_LAST_USED;
      CASE dbgFMT OF
      | dbgFMT_STAB:
        IF LINENO THEN
          SHN_max := SHN_LAST_USED_STAB;
        ELSE
          dbgFMT := dbgFMT_None;
        END;
      | dbgFMT_DWARF:
        IF GENDEBUG THEN
          SHN_max := SHN_LAST_USED_DWARF_DEBUG;
        ELSIF LINENO THEN
          SHN_max := SHN_LAST_USED_DWARF_LINENO;
        END;
      | dbgFMT_None:
      END;
      CountSections;
      LocalsNamed:=env.config.Option("LOCALSNAMED");
      DoRevert:= env.config.Option("TARGET_BIGENDIAN") # env.config.Option("HOST_BIGENDIAN");
<* IF TARGET_RISC THEN *> 
      IF NOT (at.ABI IN {at.eabi, at.v4abi}) THEN ASSERT(FALSE) END;
      IF NOT env.config.Option("TARGET_BIGENDIAN") THEN ASSERT(FALSE) END;
<* END *>
      InitHeader;
      InitSections;
      Allocate;
      CorrectHeader;  -- due to cut off zero-length reloc sections
      AllocSHStrTab;
      CountSectionOffsets;
      fc.BeginOutput;
      Write;
      fc.EndOutput;
END generate;

VAR
  formELF: FORM_ELF;
  item: reg.ITEM;

BEGIN
  NEW(formELF);
  NEW(item);
  formELF.AddItem(opt.dbg_STAB, item);
  formELF.defaultFormat := item;
  NEW(item);
  formELF.AddItem(opt.dbg_DWARF_1, item);
  NEW(item);
  formELF.AddItem(opt.dbg_DWARF_2, item);
  reg.Register(opt.objFormat, opt.objELF, formELF);
END formELF.



(*****************************************************
                        2DO
 -----------------------------------------------------
 1. Comments section - get ride of it ?
 2. Alignment - synchronize with "ALIGNMENT" equation, or at least
    think about weither we should do it or not.
 99. Rewrite totally. :-)
 -----------------------------------------------------
                       End 2DO.
 *****************************************************)
