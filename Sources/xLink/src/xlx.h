
#ifndef _XLX_H_
#define _XLX_H_

typedef unsigned long       DWORD;
typedef unsigned char       BYTE;
typedef unsigned short      WORD;
typedef float               FLOAT;
typedef FLOAT             * PFLOAT;
typedef BYTE far          * LPBYTE;
typedef int far           * LPINT;
typedef WORD near         * PWORD;
typedef WORD far          * LPWORD;
typedef long far          * LPLONG;
typedef DWORD near        * PDWORD;
typedef DWORD far         * LPDWORD;
typedef void far          * LPVOID;
typedef const void far    * LPCVOID;
typedef BYTE                BOOLEAN;
typedef unsigned short      WCHAR;
#define VOID                void
#define CHAR                char
#define SHORT               short
#define LONG                long
typedef CHAR                *PSTR;
typedef CHAR                *LPSTR;

#define FOR_EXEHDR 0
#undef  FALSE
#define FALSE 1

#ifdef __cplusplus
      extern "C" {
#endif

#pragma pack(1)    /* Force byte alignment */

    /*_________________________________________________________________*
     |                                                                 |
     |                                                                 |
     |  OS/2 .EXE FILE HEADER DEFINITION - 386 version 0:32            |
     |                                                                 |
     |_________________________________________________________________|
     *                                                                 */


#define BITPERWORD      16
#define BITPERBYTE      8
#define OBJPAGELEN      4096
#define E32MAGIC1       'L'        /* New magic number  "LX" */
#define E32MAGIC2       'X'        /* New magic number  "LX" */
#define E32MAGIC        0x584c     /* New magic number  "LX" */
#define E32RESBYTES1    0          /* First bytes reserved */
#define E32RESBYTES2    0          /* Second bytes reserved */
#define E32RESBYTES3    20         /* Third bytes reserved */
#define E32LEBO         0x00       /* Little Endian Byte Order */
#define E32BEBO         0x01       /* Big Endian Byte Order */
#define E32LEWO         0x00       /* Little Endian Word Order */
#define E32BEWO         0x01       /* Big Endian Word Order */
#define E32LEVEL        0L         /* 32-bit EXE format level */
#define E32CPU286       0x001      /* Intel 80286 or upwardly compatibile */
#define E32CPU386       0x002      /* Intel 80386 or upwardly compatibile */
#define E32CPU486       0x003      /* Intel 80486 or upwardly compatibile */

struct e32_exe                          /* New 32-bit .EXE header */
{
    unsigned char       e32_magic[2];   /* Magic number E32_MAGIC */
    unsigned char       e32_border;     /* The byte ordering for the .EXE */
    unsigned char       e32_worder;     /* The word ordering for the .EXE */
    unsigned long       e32_level;      /* The EXE format level for now = 0 */
    unsigned short      e32_cpu;        /* The CPU type */
    unsigned short      e32_os;         /* The OS type */
    unsigned long       e32_ver;        /* Module version */
    unsigned long       e32_mflags;     /* Module flags */
    unsigned long       e32_mpages;     /* Module # pages */
    unsigned long       e32_startobj;   /* Object # for instruction pointer */
    unsigned long       e32_eip;        /* Extended instruction pointer */
    unsigned long       e32_stackobj;   /* Object # for stack pointer */
    unsigned long       e32_esp;        /* Extended stack pointer */
    unsigned long       e32_pagesize;   /* .EXE page size */
    unsigned long       e32_pageshift;  /* Page alignment shift in .EXE */
    unsigned long       e32_fixupsize;  /* Fixup section size */
    unsigned long       e32_fixupsum;   /* Fixup section checksum */
    unsigned long       e32_ldrsize;    /* Loader section size */
    unsigned long       e32_ldrsum;     /* Loader section checksum */
    unsigned long       e32_objtab;     /* Object table offset */
    unsigned long       e32_objcnt;     /* Number of objects in module */
    unsigned long       e32_objmap;     /* Object page map offset */
    unsigned long       e32_itermap;    /* Object iterated data map offset */
    unsigned long       e32_rsrctab;    /* Offset of Resource Table */
    unsigned long       e32_rsrccnt;    /* Number of resource entries */
    unsigned long       e32_restab;     /* Offset of resident name table */
    unsigned long       e32_enttab;     /* Offset of Entry Table */
    unsigned long       e32_dirtab;     /* Offset of Module Directive Table */
    unsigned long       e32_dircnt;     /* Number of module directives */
    unsigned long       e32_fpagetab;   /* Offset of Fixup Page Table */
    unsigned long       e32_frectab;    /* Offset of Fixup Record Table */
    unsigned long       e32_impmod;     /* Offset of Import Module Name Table */
    unsigned long       e32_impmodcnt;  /* Number of entries in Import Module Name Table */
    unsigned long       e32_impproc;    /* Offset of Import Procedure Name Table */
    unsigned long       e32_pagesum;    /* Offset of Per-Page Checksum Table */
    unsigned long       e32_datapage;   /* Offset of Enumerated Data Pages */
    unsigned long       e32_preload;    /* Number of preload pages */
    unsigned long       e32_nrestab;    /* Offset of Non-resident Names Table */
    unsigned long       e32_cbnrestab;  /* Size of Non-resident Name Table */
    unsigned long       e32_nressum;    /* Non-resident Name Table Checksum */
    unsigned long       e32_autodata;   /* Object # for automatic data object */
    unsigned long       e32_debuginfo;  /* Offset of the debugging information */
    unsigned long       e32_debuglen;   /* The length of the debugging info. in bytes */
    unsigned long       e32_instpreload;/* Number of instance pages in preload section of .EXE file */
    unsigned long       e32_instdemand; /* Number of instance pages in demand load section of .EXE file */
    unsigned long       e32_heapsize;   /* Size of heap - for 16-bit apps */
    unsigned long       e32_stacksize;  /* Size of stack */
    unsigned char       e32_res3[E32RESBYTES3];
                                        /* Pad structure to 196 bytes */
  };



#define E32_MAGIC1(x)       (x).e32_magic[0]
#define E32_MAGIC2(x)       (x).e32_magic[1]
#define E32_BORDER(x)       (x).e32_border
#define E32_WORDER(x)       (x).e32_worder
#define E32_LEVEL(x)        (x).e32_level
#define E32_CPU(x)          (x).e32_cpu
#define E32_OS(x)           (x).e32_os
#define E32_VER(x)          (x).e32_ver
#define E32_MFLAGS(x)       (x).e32_mflags
#define E32_MPAGES(x)       (x).e32_mpages
#define E32_STARTOBJ(x)     (x).e32_startobj
#define E32_EIP(x)          (x).e32_eip
#define E32_STACKOBJ(x)     (x).e32_stackobj
#define E32_ESP(x)          (x).e32_esp
#define E32_PAGESIZE(x)     (x).e32_pagesize
#define E32_PAGESHIFT(x)    (x).e32_pageshift
#define E32_FIXUPSIZE(x)    (x).e32_fixupsize
#define E32_FIXUPSUM(x)     (x).e32_fixupsum
#define E32_LDRSIZE(x)      (x).e32_ldrsize
#define E32_LDRSUM(x)       (x).e32_ldrsum
#define E32_OBJTAB(x)       (x).e32_objtab
#define E32_OBJCNT(x)       (x).e32_objcnt
#define E32_OBJMAP(x)       (x).e32_objmap
#define E32_ITERMAP(x)      (x).e32_itermap
#define E32_RSRCTAB(x)      (x).e32_rsrctab
#define E32_RSRCCNT(x)      (x).e32_rsrccnt
#define E32_RESTAB(x)       (x).e32_restab
#define E32_ENTTAB(x)       (x).e32_enttab
#define E32_DIRTAB(x)       (x).e32_dirtab
#define E32_DIRCNT(x)       (x).e32_dircnt
#define E32_FPAGETAB(x)     (x).e32_fpagetab
#define E32_FRECTAB(x)      (x).e32_frectab
#define E32_IMPMOD(x)       (x).e32_impmod
#define E32_IMPMODCNT(x)    (x).e32_impmodcnt
#define E32_IMPPROC(x)      (x).e32_impproc
#define E32_PAGESUM(x)      (x).e32_pagesum
#define E32_DATAPAGE(x)     (x).e32_datapage
#define E32_PRELOAD(x)      (x).e32_preload
#define E32_NRESTAB(x)      (x).e32_nrestab
#define E32_CBNRESTAB(x)    (x).e32_cbnrestab
#define E32_NRESSUM(x)      (x).e32_nressum
#define E32_AUTODATA(x)     (x).e32_autodata
#define E32_DEBUGINFO(x)    (x).e32_debuginfo
#define E32_DEBUGLEN(x)     (x).e32_debuglen
#define E32_INSTPRELOAD(x)  (x).e32_instpreload
#define E32_INSTDEMAND(x)   (x).e32_instdemand
#define E32_HEAPSIZE(x)     (x).e32_heapsize
#define E32_STACKSIZE(x)    (x).e32_stacksize



/*
 *  Format of E32_MFLAGS(x):
 *
 *  Low word has the following format:
 *
 *  15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0  - bit no
 *   |     |          | |     | | | |
 *   |     |          | |     | | | +------- Per-Process Library Initialization
 *   |     |          | |     | | +--------- SystemDLL (internal fixups discarded)
 *   |     |          | |     | +----------- No Internal Fixups for Module in .EXE
 *   |     |          | |     +------------- No External Fixups for Module in .EXE
 *   |     |          | +------------------- Incompatible with PM Windowing
 *   |     |          +--------------------- Compatible with PM Windowing
 *   |     |                                 Uses PM Windowing API
 *   |     +-------------------------------- Module not Loadable
 *   +-------------------------------------- Library Module
 */


#define E32NOTP          0x8000L        /* Library Module - used as NENOTP */
#define E32NOLOAD        0x2000L        /* Module not Loadable */
#define E32PMAPI         0x0300L        /* Uses PM Windowing API */
#define E32PMW           0x0200L        /* Compatible with PM Windowing */
#define E32NOPMW         0x0100L        /* Incompatible with PM Windowing */
#define E32NOEXTFIX      0x0020L        /* NO External Fixups in .EXE */
#define E32NOINTFIX      0x0010L        /* NO Internal Fixups in .EXE */
#define E32SYSDLL        0x0008L        /* System DLL, Internal Fixups discarded*/
#define E32LIBINIT       0x0004L        /* Per-Process Library Initialization */
#define E32LIBTERM       0x40000000L    /* Per-Process Library Termination */
#define E32APPMASK       0x0300L        /* Application Type Mask */


/*
 *  Format of E32_MFLAGS(x):
 *
 *  High word has the following format:
 *
 *  15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0  - bit no
 *                                    | |
 *                                    | +--- Protected memory library module
 *                                    +----- Device driver
 */

#define E32PROTDLL       0x10000L       /* Protected memory library module */
#define E32DEVICE        0x20000L       /* Device driver                   */
#define E32MODEXE        0x00000L       /* .EXE module                     */
#define E32MODDLL        0x08000L       /* .DLL module                     */
#define E32MODPROTDLL    0x18000L       /* Protected memory library module */
#define E32MODPDEV       0x20000L       /* Physical device driver          */
#define E32MODVDEV       0x28000L       /* Virtual device driver           */
#define E32MODMASK       0x38000L       /* Module type mask                */

/*
 *  RELOCATION DEFINITIONS - RUN-TIME FIXUPS
 */




typedef union _offset
{
    unsigned short offset16;
    unsigned long  offset32;
}
    offset;                             /* 16-bit or 32-bit offset */


/***ET+ r32_rlc - Relocation item */

struct r32_rlc                          /* Relocation item */
{
    unsigned char       nr_stype;       /* Source type - field shared with new_rlc */
    unsigned char       nr_flags;       /* Flag byte - field shared with new_rlc */
    short               r32_soff;       /* Source offset */
    unsigned short      r32_objmod;     /* Target object number or Module ordinal */

    union targetid
    {
        offset             intref;      /* Internal fixup */

        union extfixup
        {
            offset         proc;        /* Procedure name offset */
            unsigned long  ord;         /* Procedure odrinal */
        }
                           extref;      /* External fixup */

        struct addfixup
        {
            unsigned short entry;       /* Entry ordinal */
            offset         addval;      /* Value added to the address */
        }
                           addfix;      /* Additive fixup */
    }
                        r32_target;     /* Target data */
    unsigned short      r32_srccount;   /* Number of chained fixup records */
    unsigned short      r32_chain;      /* Chain head */
};



/*
 *  In 32-bit .EXE file run-time relocations are written as varying size
 *  records, so we need many size definitions.
 */

#define RINTSIZE16      8
#define RINTSIZE32      10
#define RORDSIZE        8
#define RNAMSIZE16      8
#define RNAMSIZE32      10
#define RADDSIZE16      10
#define RADDSIZE32      12



#if FALSE
/*
 *  Access macros defined in NEWEXE.H !!!
 */
#define NR_STYPE(x)      (x).nr_stype
#define NR_FLAGS(x)      (x).nr_flags
#endif

#define R32_SOFF(x)      (x).r32_soff
#define R32_OBJNO(x)     (x).r32_objmod
#define R32_MODORD(x)    (x).r32_objmod
#define R32_OFFSET16(x)  (x).r32_target.intref.offset16
#define R32_OFFSET32(x)  (x).r32_target.intref.offset32
#define R32_PROCOFF16(x) (x).r32_target.extref.proc.offset16
#define R32_PROCOFF32(x) (x).r32_target.extref.proc.offset32
#define R32_PROCORD(x)   (x).r32_target.extref.ord
#define R32_ENTRY(x)     (x).r32_target.addfix.entry
#define R32_ADDVAL16(x)  (x).r32_target.addfix.addval.offset16
#define R32_ADDVAL32(x)  (x).r32_target.addfix.addval.offset32
#define R32_SRCCNT(x)    (x).r32_srccount
#define R32_CHAIN(x)     (x).r32_chain



/*
 *  Format of NR_STYPE(x)
 *
 *       7 6 5 4 3 2 1 0  - bit no
 *           | | | | | |
 *           | | +-+-+-+--- Source type
 *           | +----------- Fixup to 16:16 alias
 *           +------------- List of source offset follows fixup record
 */

#if FALSE

            /* DEFINED in newexe.h !!! */

#define NRSTYP          0x0f            /* Source type mask */
#define NRSBYT          0x00            /* lo byte (8-bits)*/
#define NRSSEG          0x02            /* 16-bit segment (16-bits) */
#define NRSPTR          0x03            /* 16:16 pointer (32-bits) */
#define NRSOFF          0x05            /* 16-bit offset (16-bits) */
#define NRPTR48         0x06            /* 16:32 pointer (48-bits) */
#define NROFF32         0x07            /* 32-bit offset (32-bits) */
#define NRSOFF32        0x08            /* 32-bit self-relative offset (32-bits) */
#endif


#define NRSRCMASK       0x0f            /* Source type mask */
#define NRALIAS         0x10            /* Fixup to alias */
#define NRCHAIN         0x20            /* List of source offset follows */
                                        /* fixup record, source offset field */
                                        /* in fixup record contains number */
                                        /* of elements in list */

/*
 *  Format of NR_FLAGS(x) and R32_FLAGS(x):
 *
 *       7 6 5 4 3 2 1 0  - bit no
 *       | | | |   | | |
 *       | | | |   | +-+--- Reference type
 *       | | | |   +------- Additive fixup
 *       | | | +----------- 32-bit Target Offset Flag (1 - 32-bit; 0 - 16-bit)
 *       | | +------------- 32-bit Additive Flag (1 - 32-bit; 0 - 16-bit)
 *       | +--------------- 16-bit Object/Module ordinal (1 - 16-bit; 0 - 8-bit)
 *       +----------------- 8-bit import ordinal (1 - 8-bit;
 *                                                0 - NR32BITOFF toggles
 *                                                    between 16 and 32 bit
 *                                                    ordinal)
 */

#if FALSE

            /* DEFINED in newexe.h !!! */

#define NRRTYP          0x03            /* Reference type mask */
#define NRRINT          0x00            /* Internal reference */
#define NRRORD          0x01            /* Import by ordinal */
#define NRRNAM          0x02            /* Import by name */
#define NRADD           0x04            /* Additive fixup */
#endif

#define NRRENT          0x03            /* Internal entry table fixup */

#define NR32BITOFF      0x10            /* 32-bit Target Offset */
#define NR32BITADD      0x20            /* 32-bit Additive fixup */
#define NR16OBJMOD      0x40            /* 16-bit Object/Module ordinal */
#define NR8BITORD       0x80            /* 8-bit import ordinal */
/*end*/

/*
 *  Data structures for storing run-time fixups in linker virtual memory.
 *
 *  Each object has a list of Object Page Directories which specify
 *  fixups for given page. Each page has its own hash table which is
 *  used to detect fixups to the same target.
 */

#define PAGEPERDIR      62
#define LG2DIR          7


typedef struct _OBJPAGEDIR
{
    DWORD   next;                       /* Virtual pointer to next dir on list */
    WORD    ht[PAGEPERDIR];             /* Pointers to individual hash tables */
}
    OBJPAGEDIR;



/*
 *  OBJECT TABLE
 */

/***ET+ o32_obj Object Table Entry */

struct o32_obj                          /* Flat .EXE object table entry */
{
    unsigned long       o32_size;       /* Object virtual size */
    unsigned long       o32_base;       /* Object base virtual address */
    unsigned long       o32_flags;      /* Attribute flags */
    unsigned long       o32_pagemap;    /* Object page map index */
    unsigned long       o32_mapsize;    /* Number of entries in object page map */
    unsigned long       o32_reserved;   /* Reserved */
};


#define O32_SIZE(x)     (x).o32_size
#define O32_BASE(x)     (x).o32_base
#define O32_FLAGS(x)    (x).o32_flags
#define O32_PAGEMAP(x)  (x).o32_pagemap
#define O32_MAPSIZE(x)  (x).o32_mapsize
#define O32_RESERVED(x) (x).o32_reserved



/*
 *  Format of O32_FLAGS(x)
 *
 *  High word of dword flag field is not used for now.
 *  Low word has the following format:
 *
 *  15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0  - bit no
 *   |  |  |  |     | | | | | | | | | | |
 *   |  |  |  |     | | | | | | | | | | +--- Readable Object
 *   |  |  |  |     | | | | | | | | | +----- Writeable Object
 *   |  |  |  |     | | | | | | | | +------- Executable Object
 *   |  |  |  |     | | | | | | | +--------- Resource Object
 *   |  |  |  |     | | | | | | +----------- Object is Discardable
 *   |  |  |  |     | | | | | +------------- Object is Shared
 *   |  |  |  |     | | | | +--------------- Object has preload pages
 *   |  |  |  |     | | | +----------------- Object has invalid pages
 *   |  |  |  |     | | +------------------- Object is permanent and swappable
 *   |  |  |  |     | +--------------------- Object is permanent and resident
 *   |  |  |  |     +----------------------- Object is permanent and long lockable
 *   |  |  |  +----------------------------- 16:16 alias required (80x86 specific)
 *   |  |  +-------------------------------- Big/Default bit setting (80x86 specific)
 *   |  +----------------------------------- Object is conforming for code (80x86 specific)
 *   +-------------------------------------- Object I/O privilege level (80x86 specific)
 *
 */

#define OBJREAD         0x0001L             /* Readable Object   */
#define OBJWRITE        0x0002L             /* Writeable Object  */
#define OBJRSRC         0x0008L             /* Resource Object   */
#define OBJINVALID      0x0080L             /* Object has invalid pages  */
#define LNKNONPERM      0x0600L             /* Object is nonpermanent - should be */
#define OBJNONPERM      0x0000L             /* zero in the .EXE but LINK386 uses 6 */
#define OBJPERM         0x0100L             /* Object is permanent and swappable */
#define OBJRESIDENT     0x0200L             /* Object is permanent and resident */
#define OBJCONTIG       0x0300L             /* Object is resident and contiguous */
#define OBJDYNAMIC      0x0400L             /* Object is permanent and long locable */
#define OBJTYPEMASK     0x0700L             /* Object type mask */
#define OBJALIAS16      0x1000L             /* 16:16 alias required (80x86 specific)           */
#define OBJBIGDEF       0x2000L             /* Big/Default bit setting (80x86 specific)        */
#define OBJIOPL         0x8000L             /* Object I/O privilege level (80x86 specific)     */
#if FOR_EXEHDR
/*
 *  Name these flags differently for EXEHDR.EXE - avoid conflicts with 286 version
 */
#define OBJDISCARD       0x0010L            /* Object is Discardable */
#define OBJSHARED        0x0020L            /* Object is Shared */
#define OBJPRELOAD       0x0040L            /* Object has preload pages  */
#define OBJEXEC          0x0004L            /* Executable Object */
#define OBJCONFORM       0x4000L            /* Object is conforming for code (80x86 specific)  */
#else
/*
 *  Life will be easier, if we keep the same names for the following flags:
 */
#define NSDISCARD       0x0010L             /* Object is Discardable */
#define NSMOVE          NSDISCARD           /* Moveable object is for sure Discardable */
#define NSSHARED        0x0020L             /* Object is Shared */
#define NSPRELOAD       0x0040L             /* Object has preload pages  */
#define NSEXRD          0x0004L             /* Executable Object */
#define NSCONFORM       0x4000L             /* Object is conforming for code (80x86 specific)  */
#endif
/*end*/

/***ET+ o32_map - Object Page Map entry */

struct o32_map                              /* Object Page Table entry */
{
    unsigned long   o32_pagedataoffset;     /* file offset of page */
    unsigned short  o32_pagesize;           /* # bytes of page data */
    unsigned short  o32_pageflags;          /* Per-Page attributes */
};


#define GETPAGEIDX(x)    ((x).o32_pagedataoffset)

#define PUTPAGEIDX(x,i)  ((x).o32_pagedataoffset = ((unsigned long)(i)))

#define PUTPAGESIZ(x,i)  ((x).o32_pagesize = ((unsigned int)(i)))

#define GETPAGESIZ(x)    ((x).o32_pagesize)

#define PAGEFLAGS(x)    (x).o32_pageflags


#define VALID           0x0000                /* Valid Physical Page in .EXE */
#define ITERDATA        0x0001                /* Iterated Data Page */
#define INVALID         0x0002                /* Invalid Page */
#define ZEROED          0x0003                /* Zero Filled Page */
#define RANGE           0x0004                /* Range of pages */
#define ITERDATA2       0x0005                /* Iterated Data Page Type II */
/*end*/

/*
 *  RESOURCE TABLE
 */

/***ET+ rsrc32 - Resource Table Entry */

struct rsrc32                               /* Resource Table Entry */
{
    unsigned short      type;               /* Resource type */
    unsigned short      name;               /* Resource name */
    unsigned long       cb;                 /* Resource size */
    unsigned short      obj;                /* Object number */
    unsigned long       offset;             /* Offset within object */
};
/*end*/

/*
 *  ENTRY TABLE DEFINITIONS
 */

#define E32_EFLAGS(x)   (x).e32_flags
#define E32_OFFSET16(x) (x).e32_variant.e32_offset.offset16
#define E32_OFFSET32(x) (x).e32_variant.e32_offset.offset32
#define E32_GATEOFF(x)  (x).e32_variant.e32_callgate.offset
#define E32_GATE(x)     (x).e32_variant.e32_callgate.callgate
#define E32_MODORD(x)   (x).e32_variant.e32_fwd.modord
#define E32_VALUE(x)    (x).e32_variant.e32_fwd.value

#define FIXENT16        3
#define FIXENT32        5
#define GATEENT16       5
#define FWDENT          7

/*
 *  BUNDLE TYPES
 */

#define EMPTY        0x00               /* Empty bundle */
#define ENTRY16      0x01               /* 16-bit offset entry point */
#define GATE16       0x02               /* 286 call gate (16-bit IOPL) */
#define ENTRY32      0x03               /* 32-bit offset entry point */
#define ENTRYFWD     0x04               /* Forwarder entry point */
#define TYPEINFO     0x80               /* Typing information present flag */


/*
 *  Format for E32_EFLAGS(x)
 *
 *       7 6 5 4 3 2 1 0  - bit no
 *       | | | | | | | |
 *       | | | | | | | +--- exported entry
 *       | | | | | | +----- uses shared data
 *       +-+-+-+-+-+------- parameter word count
 */

#define E32EXPORT       0x01            /* Exported entry */
#define E32SHARED       0x02            /* Uses shared data */
#define E32PARAMS       0xf8            /* Parameter word count mask */

/*
 *  Flags for forwarders only:
 */

#define FWD_ORDINAL     0x01            /* Imported by ordinal */


#pragma pack()       /* Restore default alignment */

/*end*/

#ifdef __cplusplus
        }
#endif

#endif /* _XLX_H_ */
