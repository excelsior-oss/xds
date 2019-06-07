
#include <stdarg.h>
#include <setjmp.h>

#define xMESSAGE 0
#define xFATAL   1
#define xERROR   2
#define xWARNING 3

#define MAX_WARNING_LEVEL     3
#define DEFAULT_WARNING_LEVEL 2

#define  msgUNABLE_TO_OPEN_FILE         0
#define  msgUNABLE_TO_WRITE_FILE        1
#define  msgUNABLE_TO_READ_FILE         2
#define  msgINSUFFICIENT_MEMORY         3
#define  msgINVALID_SYSTEM              4
#define  msgINVALID_OS_VERSION          5
#define  msgINVALID_NUMBER              6
#define  msgINVALID_PARAMETER           7
#define  msgINVALID_ENTRY_OPTION        8
#define  msgINVALID_OPTION_VALUE        9
#define  msgINVALID_OPTION              10
#define  msgNO_FILE_SPECIFIED           11
#define  msgNO_DLL_ENTRY                12
#define  msgNO_PROG_ENTRY               13
#define  msgNAME_WAS_TWICE_DECLARED     14
#define  msgNAME_WAS_REDECLARED         15
#define  msgILLEGAL_RES_FILE            16
#define  msgILLEGAL_FILE_FORMAT         17
#define  msgEMPTY_FILE                  18
#define  msgFILE_TOO_LONG               19
#define  msgDUP_DEF_FOR_EXPORT          20
#define  msgIGNORED_ENTRY_POINT         21
#define  msgILLEGAL_CPU_TYPE            22
#define  msgCANNOT_INIT_BSS_SEG         23
#define  msgINVALID_WEAK_EXTERN         24
#define  msgILLEGAL_SYM_IDX             25
#define  msgUNSUPPORTED_FIXUP_TYPE      26
#define  msgINVALID_SECTION             27
#define  msgBAD_STORAGE_CLASS           28
#define  msgNOT_EXPECT_EOF              29
#define  msgIDENT_EXPECT                30
#define  msgSTR_NOT_CLOSED              31
#define  msgBAD_ORDINAL                 32
#define  msgORDINAL_EXPECT              33
#define  msgEOF_EXPECT                  34
#define  msgUNKNOWN_COMDAT              35
#define  msgINVALID_ENTRY_POINT         36
#define  msgLOCAL_GRP_NOT_SUPPORTED     37
#define  msgGRP_TYPE_NOT_SUPPORTED      38
#define  msgUNKNOWN_COMMON_TYPE         39
#define  msgTOO_MUCH_DATA_FOR_SEG       40
#define  msgNO_LINNUM_SEG               41
#define  msgFIXUP_IN_BSS_SEG            42
#define  msgFIXUP_WITHOUT_LEDATA        43
#define  msgUNSUPPORTED_OMF_RECORD_TYPE 44
#define  msgILLEGAL_RECORD_LENGTH       45
#define  msgRECORD_TOO_LONG             46
#define  msgUNKNOWN_RECORD_TYPE         47
#define  msgINTERNAL_NAME_NOT_FOUND     48
#define  msgDUP_DEF_FOR_EXPORT_IN_FILE  49
#define  msgUNRESOLVED_SEG              50
#define  msgINVALID_FIXUP_TARGET        51
#define  msgINVALID_FIXUP_FOR_FLAT_MOD  52
#define  msgNAME_NOT_FOUND              53
#define  msgREFERENCED_NAME_NOT_FOUND   54
#define  msgINVALID_ENTRY_POINT_TARGET  55
#define  msgILLEGAL_STUB_FILE           56
#define  msgINVALID_EXE_TYPE            57
#define  msgNO_EXPORT_SECTION           58
#define  msgOPTION_SKIPPED              59
#define  msgINVALID_IMAGE_FORMAT        60
#define  msgUNABLE_TO_MIX_DBG_INFO      61
#define  msgDUP_DBG_INFO                62
#define  msgDUP_RES_NAME                63
#define  msgNO_DEBUG_INFO               64
#define  msgINVALID_FILE_FORMAT         65
#define  msgEXPECT_RBRACKET             66
#define  msgFULNAM_WO_MODNAM            67
#define  msgTOO_MANY_EXPORTED           68
#define  msgINVALID_EXPORT_OPTION       69
#define  msgDUP_ORDINALS                70
#define  msgCANT_FIND_ARRAY_ELEMTYPE    71
#define  msgCODEVIEW_TOO_MANY_TYPES     72
#define  msgINCOMPATIBLE_VERSIONS       73
#define  msgFIXUP_IN_RDATA              74
#define  msgUNABLE_LOAD_RESOURCE        75
#define  msgREQUIRED_MEMBER_MISSING     76
#define  msgINVALID_COFF_LIBRARY        77
#define  msgINCORRECT_RECORD_FORMAT     78
#define  msgINVALID_ZERO_TERM_STRING    79
#define  msgFILE_FORMAT_OUTDATED        80
#define  msgEXPORTED_NAME_NOT_FOUND     81
#define  msgOBSOLETE_RECORD             82
#define  msgUNSUPPORTED_XOMF_VERSION    83
#define  msgFILE__EXPECTED              84
#define  msgUNABLE_DEBUG_INFO_IN_REORD  85
#define  msgUNABLE_ALLOC_VIRTUAL_SPACE  86
#define  msgTOO_BIG_IMAGE               87
#define  msgINVALID_ELF                 88
#define  msgPACKED_EFS_WRITTEN          89
#define  msgCOMMON_OBJ_FILES_NOT_MATCH  90
#define  msgELF_NOT_SUPPORT_RESOURCES   91
#define  msgUNABLE_TO_OPEN_FILE_MSG     92
#define  msgTOO_MANY_TYPES              93
#define  msgUNREFERENCED_TYPES_TABLE    94
#define  msgOPTION_REQUIRED_BEFORE_CONFIG 95

#define  MESSAGE_TYPES                  96

extern int       TotalErrors;
extern int       TotalWarnings;

extern void Message(int code, int number,...);

extern void VerboseMessage(const char *fmt_str,...);

#define INFO_SECTIONSIZE         0x00000001
#define INFO_RELOCSTAT           0x00000002
#define INFO_OMFREAD             0x00000004
#define INFO_SYMBOLDECLARATION   0x00000008
#define INFO_TYPEDESC            0x00000010
#define INFO_DEBUGINFO           0x00000020
#define INFO_EILREAD             0x00000040
#define INFO_MEMUSAGE            0x00000080
#define INFO_IMAGECREATION       0x00000100
#define INFO_JEXPORT             0x00000200
#define INFO_JAVASTRINGS         0x00000400
#define INFO_UNUSEDDLLIMPORT     0x00000800
#define INFO_STRINGOPT           0x00001000
#define INFO_STROPTPROGRESS      0x00002000
#define INFO_VIRTUALADDRESSES    0x00004000
#define INFO_READLINKINFO        0x00008000
#define INFO_XOMFREAD            0x00010000
#define INFO_NULLCHECKS          0x00020000
#define INFO_ELFREAD             0x00040000
#define INFO_ZIPREAD             0x00080000

extern void SetVerboseMask (char *);
extern void VerboseMessage (int infoKind, const char *fmt_str,...);
extern Bool IsPrintable    (int infoKind);
extern void VerboseHelp ();

extern char * ERROR_MESSAGE_BUF;

#define ERROR_EXIT  2
#define ASSERT_EXIT 3

extern jmp_buf ABORT_JMP_BUF;

