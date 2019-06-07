

#ifndef _XELF_H_
#define _XELF_H_
/*
    "Executable and Linking Format (ELF)"

    Tool Interface Standards(TIS),
    Portable Formats Specification, Version 1.1
*/



/* Data Representation, 32-Bit Data Types */

typedef void *             Elf32_Addr;
typedef unsigned short int Elf32_Half;
typedef unsigned int       Elf32_Off;
typedef int                Elf32_Sword;
typedef unsigned int       Elf32_Word;


/* -------------- ELF Header -------------- */

#define EI_NIDENT       16

typedef struct {
          unsigned char e_ident[EI_NIDENT];
          Elf32_Half    e_type;
          Elf32_Half    e_machine;
          Elf32_Word    e_version;
          Elf32_Addr    e_entry;
          Elf32_Off     e_phoff;
          Elf32_Off     e_shoff;
          Elf32_Word    e_flags;
          Elf32_Half    e_ehsize;
          Elf32_Half    e_phentsize;
          Elf32_Half    e_phnum;
          Elf32_Half    e_shentsize;
          Elf32_Half    e_shnum;
          Elf32_Half    e_shstrndx;
} Elf32_Ehdr;


/* Type of object file (e_type) */

#define ET_NONE         0
#define ET_REL          1
#define ET_EXEC         2
#define ET_DYN          3
#define ET_CORE         4
#define ET_LOPROC       0xff00
#define ET_HIPROC       0xffff

/* Architecture (e_machine) */

#define EM_NONE         0
#define EM_M32          1
#define EM_SPARC        2
#define EM_386          3
#define EM_68K          4
#define EM_88K          5
#define EM_860          7
#define EM_MIPS         8
#define EM_MIPS_RS4_BE  10

/* Object file version (e_version) */

#define EV_NONE         0
#define EV_CURRENT      1

/* ELF Identification */

#define EI_MAG0         0
#define EI_MAG1         1
#define EI_MAG2         2
#define EI_MAG3         3
#define EI_CLASS        4
#define EI_DATA         5
#define EI_VERSION      6
#define EI_PAD          7

#define ELFMAG0         0x7F
#define ELFMAG1         'E'
#define ELFMAG2         'L'
#define ELFMAG3         'F'

#define ELFCLASSNONE    0
#define ELFCLASS32      1
#define ELFCLASS64      2

#define ELFDATANONE     0
#define ELFDATA2LSB     1
#define ELFDATA2MSB     2


/* -------------- Sections -------------- */

/* Special Section Indexes */

#define SHN_UNDEF       0
#define SHN_LORESERVE   0xff00
#define SHN_LOPROC      0xff00
#define SHN_HIPROC      0xff1f
#define SHN_ABS         0xfff1
#define SHN_COMMON      0xfff2
#define SHN_HIRESERVE   0xffff

/* Section Header */

typedef struct {
          Elf32_Word  sh_name;
          Elf32_Word  sh_type;
          Elf32_Word  sh_flags;
          Elf32_Addr  sh_addr;
          Elf32_Off   sh_offset;
          Elf32_Word  sh_size;
          Elf32_Word  sh_link;
          Elf32_Word  sh_info;
          Elf32_Word  sh_addralign;
          Elf32_Word  sh_entsize;
} Elf32_Shdr;

/* Section Types (sh_type) */

#define SHT_NULL        0
#define SHT_PROGBITS    1
#define SHT_SYMTAB      2
#define SHT_STRTAB      3
#define SHT_RELA        4
#define SHT_HASH        5
#define SHT_DYNAMIC     6
#define SHT_NOTE        7
#define SHT_NOBITS      8
#define SHT_REL         9
#define SHT_SHLIB       10
#define SHT_DYNSYM      11
#define SHT_LOPROC      0x70000000
#define SHT_HIPROC      0x7fffffff
#define SHT_LOUSER      0x80000000
#define SHT_HIUSER      0xffffffff

/* Section Attribute Flags (sh_flags) */

#define SHF_WRITE       0x1
#define SHF_ALLOC       0x2
#define SHF_EXECINSTR   0x4
#define SHF_MASKPROC    0xf0000000

/* -------------- Symbol Table -------------- */

#define STN_UNDEF       0

/* Symbol Table Entry */

typedef struct {
          Elf32_Word    st_name;
          Elf32_Addr    st_value;
          Elf32_Word    st_size;
          unsigned char st_info;
          unsigned char st_other;
          Elf32_Half    st_shndx;
} Elf32_Sym;


#define ELF32_ST_BIND(i)        ((i) >> 4)
#define ELF32_ST_TYPE(i)        ((i) & 0xf)
#define ELF32_ST_INFO(b, t)     (((b) << 4) | ((t) & 0xf))


/* Symbol Binding, ELF32_ST_BIND */

#define STB_LOCAL       0
#define STB_GLOBAL      1
#define STB_WEAK        2
#define STB_LOPROC      13
#define STB_HIPROC      15

/* Symbol Types, ELF32_ST_TYPE */

#define STT_NOTYPE      0
#define STT_OBJECT      1
#define STT_FUNC        2
#define STT_SECTION     3
#define STT_FILE        4
#define STT_LOPROC      13
#define STT_HIPROC      15

/* -------------- Relocation -------------- */

/* Relocation Entries */

typedef struct {
          Elf32_Addr   r_offset;
          Elf32_Word   r_info;
} Elf32_Rel;


typedef struct {
          Elf32_Addr   r_offset;
          Elf32_Word   r_info;
          Elf32_Sword  r_addend;
} Elf32_Rela;


#define ELF32_R_SYM(i)          ((i) >> 8)
#define ELF32_R_TYPE(i)         ((unsigned char)(i))
#define ELF32_R_INFO(s, t)      (((s) << 8) + (unsigned char)(t))

/* -------------- Program Header -------------- */

typedef struct {
          Elf32_Word   p_type;
          Elf32_Off    p_offset;
          Elf32_Addr   p_vaddr;
          Elf32_Addr   p_paddr;
          Elf32_Word   p_filesz;
          Elf32_Word   p_memsz;
          Elf32_Word   p_flags;
          Elf32_Word   p_align;
} Elf32_Phdr;

/* Segment Types (p_type) */

#define PT_NULL         0
#define PT_LOAD         1
#define PT_DYNAMIC      2
#define PT_INTERP       3
#define PT_NOTE         4
#define PT_SHLIB        5
#define PT_PHDR         6
#define PT_LOPROC       0x70000000
#define PT_HIPROC       0x7fffffff

/* ---------------- Processor Specific (Intel Architecture) ---------------- */

/* Relocation types */

#define R_386_NONE      0
#define R_386_32        1
#define R_386_PC32      2

/* ---------- Operating System Specific (UNIX System V Release 4) ---------- */

/* Segment Flag Bits (p_flags) */

#define PF_X            0x1
#define PF_W            0x2
#define PF_R            0x4
#define PF_MASKPROC     0xf0000000

/* Dynamic Structure */

typedef struct {
          Elf32_Sword  d_tag;
          union {
                 Elf32_Word  d_val;
                 Elf32_Addr  d_ptr;
          } d_un;
} Elf32_Dyn;

/* Dynamic Array Tags (d_tag) */

#define DT_NULL         0
#define DT_NEEDED       1
#define DT_PLTRELSZ     2
#define DT_PLTGOT       3
#define DT_HASH         4
#define DT_STRTAB       5
#define DT_SYMTAB       6
#define DT_RELA         7
#define DT_RELASZ       8
#define DT_RELAENT      9
#define DT_STRSZ        10
#define DT_SYMENT       11
#define DT_INIT         12
#define DT_FINI         13
#define DT_SONAME       14
#define DT_RPATH        15
#define DT_SYMBOLIC     16
#define DT_REL          17
#define DT_RELSZ        18
#define DT_RELENT       19
#define DT_PLTREL       20
#define DT_DEBUG        21
#define DT_TEXTREL      22
#define DT_JMPREL       23
#define DT_BIND_NOW     24
#define DT_LOPROC       0x70000000
#define DT_HIPROC       0x7fffffff

/* ------------- Intel Architecture + UNIX System V Release 4 ------------- */

/* Relocation Types */

#define R_386_GOT32     3
#define R_386_PLT32     4
#define R_386_COPY      5
#define R_386_GLOB_DAT  6
#define R_386_JMP_SLOT  7
#define R_386_RELATIVE  8
#define R_386_GOTOFF    9
#define R_386_GOTPC     10

/* GNU extensions, defined in Linux Standard Base Specification */

#define DT_VERSYM         0x6ffffff0
#define DT_RELCOUNT       0x6ffffffa

#define DT_VERDEF         0x6ffffffc    /* Address of version definition table */
#define DT_VERDEFNUM      0x6ffffffd    /* Number of version definitions */
#define DT_VERNEED        0x6ffffffe    /* Address of table with needed versions */
#define DT_VERNEEDNUM     0x6fffffff    /* Number of needed versions */

#define SHT_GNU_verdef    0x6ffffffd    /* Version definition section.  */
#define SHT_GNU_verneed   0x6ffffffe    /* Version needs section.  */
#define SHT_GNU_versym    0x6fffffff    /* Version symbol table.  */

/* Version definition sections.  */

typedef struct
{
  Elf32_Half    vd_version;             /* Version revision */
  Elf32_Half    vd_flags;               /* Version information */
  Elf32_Half    vd_ndx;                 /* Version Index */
  Elf32_Half    vd_cnt;                 /* Number of associated aux entries */
  Elf32_Word    vd_hash;                /* Version name hash value */
  Elf32_Word    vd_aux;                 /* Offset in bytes to verdaux array */
  Elf32_Word    vd_next;                /* Offset in bytes to next verdef
                                           entry */
} Elf32_Verdef;


/* Legal values for vd_version (version revision).  */
#define VER_DEF_NONE    0               /* No version */
#define VER_DEF_CURRENT 1               /* Current version */
#define VER_DEF_NUM     2               /* Given version number */

/* Legal values for vd_flags (version information flags).  */
#define VER_FLG_BASE    0x1             /* Version definition of file itself */
#define VER_FLG_WEAK    0x2             /* Weak version identifier */

/* Versym symbol index values.  */
#define VER_NDX_LOCAL           0       /* Symbol is local.  */
#define VER_NDX_GLOBAL          1       /* Symbol is global.  */
#define VER_NDX_LORESERVE       0xff00  /* Beginning of reserved entries.  */
#define VER_NDX_ELIMINATE       0xff01  /* Symbol is to be eliminated.  */

/* Auxialiary version information.  */

typedef struct
{
  Elf32_Word    vda_name;               /* Version or dependency names */
  Elf32_Word    vda_next;               /* Offset in bytes to next verdaux
                                           entry */
} Elf32_Verdaux;

/* Version dependency section.  */

typedef struct
{
  Elf32_Half    vn_version;             /* Version of structure */
  Elf32_Half    vn_cnt;                 /* Number of associated aux entries */
  Elf32_Word    vn_file;                /* Offset of filename for this
                                           dependency */
  Elf32_Word    vn_aux;                 /* Offset in bytes to vernaux array */
  Elf32_Word    vn_next;                /* Offset in bytes to next verneed
                                           entry */
} Elf32_Verneed;


/* Legal values for vn_version (version revision).  */
#define VER_NEED_NONE    0              /* No version */
#define VER_NEED_CURRENT 1              /* Current version */
#define VER_NEED_NUM     2              /* Given version number */

/* Auxiliary needed version information.  */

typedef struct
{
  Elf32_Word    vna_hash;               /* Hash value of dependency name */
  Elf32_Half    vna_flags;              /* Dependency specific information */
  Elf32_Half    vna_other;              /* Unused */
  Elf32_Word    vna_name;               /* Dependency name string offset */
  Elf32_Word    vna_next;               /* Offset in bytes to next vernaux
                                           entry */
} Elf32_Vernaux;


/* Legal values for vna_flags.  */
#define VER_FLG_WEAK    0x2             /* Weak version identifier */

#endif
