
#ifndef XOMF_H
#define XOMF_H

#include "omf.h"

/* Obsolete definitions */

#define OLD_XOMF_THEADR 0xD5
#define XOMF_THEADR 0xDD
#define XOMF_LIBHDR 0xF2
#define XOMF_JSTRINGDESC 0xD7

/* XOMF Signature */

#define XOMF_SIGNATURE          0x464D4F58  /* "XOMF"     */
#define XOMF_FORMAT_VERSION     13           /* 03.09.2007 */

/* XOMF Object Module Types */

#define XOMF_MODULE_NORMAL             0x01
#define XOMF_MODULE_STACKTRACEINFO     0x02

/* XOMF Debug Info Formats */

#define XOMF_DEBUG_FORMAT_NO    0
#define XOMF_DEBUG_FORMAT_CV    1
#define XOMF_DEBUG_FORMAT_HLL4  2
#define XOMF_DEBUG_FORMAT_NB99  3
#define XOMF_DEBUG_FORMAT_EDIF  4

/* XOMF Record Types */

#define XOMF_HEADER      0xD0
#define XOMF_SEGDEF      0xD1
#define XOMF_OBJEND      0xD2
#define XOMF_EXPDEF      0xD9
#define XOMF_IMPDEF      0xDB
#define XOMF_RAWDATA     0xDE
#define XOMF_FIXUP       0xDF

/* Data Kinds (XOMF_RAWDATA) */

#define DK_BYTESTR       0x01
#define DK_UNICODESTR    0x02

/* Public Symbol Types (PUBDEF) */

#define XOMF_TYPE_DATA   0x01
#define XOMF_TYPE_CODE   0x02
#define XOMF_TYPE_TD     0x05
#define XOMF_TYPE_ATD    0x09

/* Fixup Types (XOMF_FIXUP) */

#define XOMF_FX_ADDR32                     1
#define XOMF_FX_OFFS32                     2
#define XOMF_FX_FAR48                      3
#define XOMF_FX_TDINDEX16                  4
#define XOMF_FX_JSTR32                     5
#define XOMF_FX_BYTESTR32                  6
#define XOMF_FX_CONSTADDR32                7
#define XOMF_FX_CONSTADDR32_EXTRA_THUNK    8
#define XOMF_FX_TDINDEX32                  9

/* Fixup Target Kinds (XOMF_FIXUP) */

#define XOMF_TK_SEG             1
#define XOMF_TK_ID              2
#define XOMF_TK_RAWDATA         3

/* Segment Class Codes */

#define SEGCLASS_CODE           1
#define SEGCLASS_DATA           2
#define SEGCLASS_BSS            3
#define SEGCLASS_RODATA         4
#define SEGCLASS_DEBUG          5
#define SEGCLASS_NULLCHECKS     6
#define SEGCLASS_STACKTRACE     7

/* Segment Alignment */

#define SEGALIGN_1              0
#define SEGALIGN_2              1
#define SEGALIGN_4              2
#define SEGALIGN_8              3
#define SEGALIGN_16             4
#define SEGALIGN_4096           12


#endif /* XOMF_H */
