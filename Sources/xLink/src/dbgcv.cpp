/*
  Запись отладочной CV информации.
  Ограничения:
  1) Символы
     - не поддерживаются WITH, THUNK
     - плохо пишется информация о COMMON-блоках (много раз)
  2) Типы
     - не обрабатывается LF_SKIP
     - не считываются precompiled types
     - не строится LF_DERIVED
     - в LF_POINTER:
        - не обрабатываются segment based
        - плохо обрабатываются pointer to data & pointer to member
*/

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "xdefs.h"

open_namespace

#include "struct.h"
#include "idents.h"
#include "messages.h"
#include "writer.h"

#include "debug.h"
#include "dbgcv.h"
#include "xmem.h"

char * UNKNOWN_SRC_NAME = "???";

/*----------------------------------------------------------------------------*/
/*                                                                            */
/*                  Получение по полному имени файла имени модуля             */
/* Необходимо перенести в другой модуль, но пока не понятно в какой           */
/*----------------------------------------------------------------------------*/

const char *getpart_name(const char* name) {
    int l = strlen(name);
    const char *partname = name + l;
    while( (*(--partname) != '\\') && (partname != name) )
        ;
    return (partname == name) ? name : ++partname;
}

/*----------------------------------------------------------------------------*/
/*                                                                            */
/*                      Формирование таблицы модулей                          */
/*                                                                            */
/*----------------------------------------------------------------------------*/

static int NEntries = 0;

static word CodeSeg  = 0;
static word DataSeg  = 0;
static word BSSSeg   = 0;
static word RDataSeg = 0;

static dword CodeEnd, DataEnd, BSSEnd, RDataEnd;

/*----------------------------------------------------------------------------*/
/*                                                                            */
/*                      Запись отладочной информации                          */
/*                                                                            */
/*----------------------------------------------------------------------------*/

static void CalcSO (dword   addr,
                    dword * offset,
                    word  * section)
{
    if (addr >= CodeStart && addr < CodeEnd) {
        * offset  = addr - CodeStart;
        * section = (word) CodeSeg;
    }
    else if (addr >= DataStart && addr < DataEnd) {
        * offset  = addr - DataStart;
        * section = (word) DataSeg;
    }
    else if (addr >= BSSStart && addr < BSSEnd) {
        * offset  = addr - BSSStart;
        * section = (word) BSSSeg;
    }
    else if (addr >= RDataStart && addr < RDataEnd) {
        * offset  = addr - RDataStart;
        * section = (word) RDataSeg;
    } else
        ASSERT_FALSE ();
}

/*----------------------------------------------------------------------------*/
/*                                                                            */
/*                      Чтение и запись Codeview типов                        */
/*                                                                            */
/*----------------------------------------------------------------------------*/

#define LF_MODIFIER     0x0001
#define LF_POINTER      0x0002
#define LF_ARRAY        0x0003
#define LF_CLASS        0x0004
#define LF_STRUCTURE    0x0005
#define LF_UNION        0x0006
#define LF_ENUM         0x0007
#define LF_PROCEDURE    0x0008
#define LF_MFUNCTION    0x0009
#define LF_VTSHAPE      0x000A
#define LF_COBOL0       0x000B
#define LF_COBOL1       0x000C
#define LF_BARRAY       0x000D
#define LF_LABEL        0x000E
#define LF_NULL         0x000F
#define LF_NOTTRANS     0x0010
#define LF_DIMARRAY     0x0011
#define LF_VFTPATH      0x0012
#define LF_PRECOMP      0x0013
#define LF_ENDPRECOMP   0x0014
#define LF_OEM          0x0015

#define LF_SKIP         0x0200
#define LF_ARGLIST      0x0201
#define LF_DEFARG       0x0202
#define LF_LIST         0x0203
#define LF_FIELDLIST    0x0204
#define LF_DERIVED      0x0205
#define LF_BITFIELD     0x0206
#define LF_METHODLIST   0x0207
#define LF_DIMCONU      0x0208
#define LF_DIMCONLU     0x0209
#define LF_DIMVARU      0x020A
#define LF_DIMVARLU     0x020B
#define LF_REFSYM       0x020C

#define LF_BCLASS       0x0400
#define LF_VBCLASS      0x0401
#define LF_IVBCLASS     0x0402
#define LF_ENUMERATE    0x0403
#define LF_FRIENDFCN    0x0404
#define LF_INDEX        0x0405
#define LF_MEMBER       0x0406
#define LF_STMEMBER     0x0407
#define LF_METHOD       0x0408
#define LF_NESTTYPE     0x0409
#define LF_VFUNCTAB     0x040A
#define LF_FRIENDCLS    0x040B
#define LF_ONEMETHOD    0x040C
#define LF_VFUNCOFF     0x040D

#define LF_NUMERIC      0x8000
#define LF_CHAR         0x8000
#define LF_SHORT        0x8001
#define LF_USHORT       0x8002
#define LF_LONG         0x8003
#define LF_ULONG        0x8004
#define LF_REAL32       0x8005
#define LF_REAL64       0x8006
#define LF_REAL80       0x8007
#define LF_REAL128      0x8008
#define LF_QUADWORD     0x8009
#define LF_UQUADWORD    0x800A
#define LF_REAL48       0x800B
#define LF_COMPLEX32    0x800C
#define LF_COMPLEX64    0x800D
#define LF_COMPLEX80    0x800E
#define LF_COMPLEX128   0x800F
#define LF_VARSTRING    0x8010

struct type {
                word len;
                word tag;
                union {
                        struct {
                                word attribute;
                                word index;
                        } modifier;
                        struct {
                                word attribute;
                                word type;
                                word variant [65536];
                        } pointer;
                        struct {
                                word elemtype;
                                word idxtype;
                                byte filler [65536];
                        } array;
                        struct {
                                word count;
                                word field;
                                word property;
                                word dlist;
                                word vshape;
                                byte filler [65536];
                        } structure;
                        struct {
                                word count;
                                word field;
                                word property;
                                byte filler [65536];
                        } t_union;
                        struct {
                                word count;
                                word type;
                                word flist;
                                word property;
                                byte namelen;
                                byte name [255];
                        } t_enum;
                        struct {
                                word rvtype;
                                byte call;
                                byte reserved;
                                word nparams;
                                word arglist;
                        } procedure;
                        struct {
                                word  rvtype;
                                word  t_class;
                                word  t_this;
                                byte  call;
                                byte  reserved;
                                word  nparams;
                                word  arglist;
                                dword thisadjust;
                        } mfunction;
                        struct {
                                word parent;
                                byte data [65536];
                        } cobol0;
                        struct {
                                word type;
                        } barray;
                        struct {
                                word utype;
                                word diminfo;
                                byte namelen;
                                byte name [255];
                        } dimarray;
                        struct {
                                word count;
                                word bases [65535];
                        } vftpath;
                        struct {
                                word oem;
                                word recoem;
                                word count;
                                word indices [65535];
                        } oem;
                        struct {
                                word count;
                                word indices [65535];
                        } arglist;
                        struct {
                                word index;
                                byte expressionlen;
                                byte expression [255];
                        } defarg;
                        struct {
                                byte length;
                                byte position;
                                word type;
                        } bitfield;
                        struct {
                                word rank;
                                word index;
                                byte bounds [65536];
                        } dimconu;
                        struct {
                                word rank;
                                word index;
                                word var [65536];
                        } dimvaru;
                        struct {
                                word rank;
                                word index;
                                word var [65536];
                        } dimvarlu;
                } value;
};

#define MemberAttr(x)   (((x) >> 2) & 7)

#define MA_VANILLA                      0
#define MA_VIRTUAL                      1
#define MA_STATIC                       2
#define MA_FRIEND                       3
#define MA_INTODUCING_VIRTUAL           4
#define MA_PURE_VIRTUAL                 5
#define MA_PURE_INTRODUCING_VIRTUAL     6

static word ProcessType (word x);

/*----------------------------------------------------------------------------*/

#define MaxNTypes       65536

static dword         NOldTypes;                 /* # типов в текущем сегменте */
static struct type * OldTypes [MaxNTypes];      /* Ссылки на начала типов     */
static word          NewTypes [MaxNTypes];      /* Пересчитанные типы         */

/*----------------------------------------------------------------------------*/

static dword   NTypes;
static Storage * Types;
static dword   TypesDirectory [MaxNTypes];     /* Ссылки на начала типов */

/*----------------------------------------------------------------------------*/

static void TypesAlign (void)
{
        while (Types->Index & 3)
                Types->PutB( 0xF4 - (Types->Index & 3));
}

/*----------------------------------------------------------------------------*/

/*
  Поместить в Types ссылки на типы в данном сегменте; очистить NewTypes
*/

static void CollectTypes (Segment * s)
{
        struct type ** tptr;

        if (NOldTypes)
                memset (NewTypes, 0, NOldTypes * sizeof (word));
        for (tptr = OldTypes; s; s = s -> link)
                if ((s -> getLen() > 4) && (* (dword *) s -> getText() == 1)) {
                        dword len;
                        byte * ptr, * beg;

                        for (ptr = beg = s -> getText() + 4, len = s -> getLen() - 4; len;) {
                                dword rlen;

                                * tptr ++ = (struct type *) ptr;
                                rlen = ((struct type *) ptr) -> len + 2;
                                ptr += rlen;
                                len -= rlen;
                                if (len == 0)
                                        break;
                                if ((ptr - beg) & 3) {
                                        len -= 4 - ((ptr - beg) & 3);
                                        ptr += 4 - ((ptr - beg) & 3);
                                }
                        }
                }
        NOldTypes = tptr - OldTypes;
}

/*----------------------------------------------------------------------------*/

static void PreprocessSymbols (void * start, void * fin);

static int NGlobals = 0;
static int NLocals  = 0;

/*
  Обработать ссылку на символ из типов
*/

static void ProcessRef (void * start, void * fin)
{
        dword nl, ng;

        nl = NLocals;
        ng = NGlobals;
        PreprocessSymbols (start, fin);
        NLocals  = nl;
        NGlobals = ng;
}

/*----------------------------------------------------------------------------*/

/*
  Вычислить длину numeric-а
*/

static dword NumLen (void * p, dword offset)
{
        word * q = (word *) (offset + (byte *) p);

        switch (* q) {
        case LF_CHAR:           return 2 + 1;
        case LF_SHORT:
        case LF_USHORT:         return 2 + 2;
        case LF_LONG:
        case LF_ULONG:
        case LF_REAL32:         return 2 + 4;
        case LF_REAL48:         return 2 + 6;
        case LF_QUADWORD:
        case LF_UQUADWORD:
        case LF_REAL64:         return 2 + 8;
        case LF_REAL80:         return 2 + 10;
        case LF_REAL128:        return 2 + 16;
        case LF_COMPLEX32:      return 2 + 4 + 4;
        case LF_COMPLEX64:      return 2 + 8 + 8;
        case LF_COMPLEX80:      return 2 + 10 + 10;
        case LF_COMPLEX128:     return 2 + 16 + 16;
        case LF_VARSTRING:      return 2 + 2 + q [1];
        }
        return 2;
}

/*----------------------------------------------------------------------------*/

/*
  Пройтись по списку в типе и обработать его элементы
*/

static void ProcessSubfields (byte * p, dword plen)
{
#undef  Recalc
#define Recalc(offs)    {                                               \
                                word * q = (word *) (p + offs);         \
                                * q = ProcessType (* q);                \
                        }                                               \

        while (plen) {
                dword len;

                switch (* (word *) p) {
                case LF_BCLASS:
                        Recalc (2);
                        len = 6 + NumLen (p, 6);
                        break;
                case LF_VBCLASS:
                case LF_IVBCLASS:
                        Recalc (2);
                        Recalc (4);
                        len = 8 + NumLen (p, 8);
                        len += NumLen (p, len);
                        break;
                case LF_ENUMERATE:
                        len = 4 + NumLen (p, 4);
                        len += 1 + p [len];
                        break;
                case LF_FRIENDFCN:
                        Recalc (2);
                        len = 4 + 1 + p [4];
                        break;
                case LF_INDEX:
                case LF_VFUNCTAB:
                case LF_FRIENDCLS:
                        Recalc (2);
                        len = 4;
                        break;
                case LF_MEMBER:
                        Recalc (2);
                        len = 6 + NumLen (p, 6);
                        len += 1 + p [len];
                        break;
                case LF_STMEMBER:
                        Recalc (2);
                        len = 6 + 1 + p [6];
                        break;
                case LF_METHOD:
                        Recalc (4);
                        len = 6 + 1 + p [6];
                        break;
                case LF_NESTTYPE:
                        Recalc (2);
                        len = 4 + 1 + p [4];
                        break;
                case LF_ONEMETHOD:
                        Recalc (4);
                        switch (MemberAttr (* (word *) (p + 2))) {
                        case MA_VIRTUAL:
                        case MA_INTODUCING_VIRTUAL:
                        case MA_PURE_VIRTUAL:
                        case MA_PURE_INTRODUCING_VIRTUAL:
                                len = 10 + 1 + p [10];
                                break;
                        default:
                                len = 6 + 1 + p [6];
                                break;
                        }
                        break;
                case LF_VFUNCOFF:
                        Recalc (2);
                        len = 8;
                        break;
                }
                p    += len;
                plen -= len;
                if (plen == 0)
                        break;
                if (* p >= 0xF0) {
                        plen -= (* p) & 0x0F;
                        p    += (* p) & 0x0F;
                }
        }
}

/*----------------------------------------------------------------------------*/

#define HTypeMapSize   16384

struct HTEntry {
    unsigned hash;
    int typeIndex;
    struct HTEntry* next;
};

struct HTEntry* HTypeMap[HTypeMapSize];


static unsigned calcTypeHash(const struct type* t) {
    unsigned h = 0;
    const char* p = (const char*)t;
    for (int i = 0; i < t -> len+2; i++) {
        h = h*31 + *p;
        p++;
    }
    return h;
}

// HOT SPOT !!!!!!
static int compareTypes (const struct type* t1,
                         const struct type* t2)
{
    return (t1->len == t2->len) && !memcmp(t1, t2, t1->len + 2);
}

static void addType2HMap(int typeIndex) {
    const struct type* t = (const struct type*) (Types->Ptr + TypesDirectory [typeIndex - 0x1000]);
    unsigned hash  = calcTypeHash(t);
    unsigned index = hash % HTypeMapSize;

    struct HTEntry* entry = (struct HTEntry*) xalloc(sizeof(struct HTEntry));
    entry -> hash = hash;
    entry -> typeIndex = typeIndex;

    entry -> next = HTypeMap[index];
    HTypeMap[index] = entry;
}

// HOT SPOT !!!!!!
static int findEqualType(const struct type* t) {
    unsigned hash  = calcTypeHash(t);
    unsigned index = hash % HTypeMapSize;

    struct HTEntry* entry = HTypeMap[index];
    for (; entry != 0; entry = entry->next) {
        if ((entry->hash == hash) &&
            compareTypes(t, (const struct type*) (Types->Ptr + TypesDirectory [entry->typeIndex - 0x1000])))
        {
            return entry->typeIndex;
        }
    }
    return 0;
}


static void freeTypesHMap(void) {
    for (int i = 0; i < HTypeMapSize; i++) {
        struct HTEntry* entry = HTypeMap[i];
        while (entry != 0) {
            struct HTEntry* next = entry->next;
            xfree (entry);
            entry = next;
        }
        HTypeMap[i] = 0;
    }
}

/*----------------------------------------------------------------------------*/

static word ProcessType (word x)
{
        struct type * ptr;
        dword         i, full_len;
        dword         index, oldCnt;
        word          tp;

#undef  Recalc
#define Recalc(field)   tp = ProcessType (ptr -> field);                       \
                        ((struct type *) (Types->Ptr + index)) -> field = tp;

        if (x < 0x1000)
                return x;
        x -= 0x1000;
        if (NewTypes [x])
                return NewTypes [x];
        ASSERT (x < NOldTypes);
        ptr = OldTypes [x];
        full_len = ptr -> len + 2;
        index    = Types->Index;
        oldCnt   = NTypes;
        TypesDirectory [NTypes] = index;
        if (0x1000 + NTypes >= 65536)
           Message(xFATAL, msgCODEVIEW_TOO_MANY_TYPES);
        NewTypes [x] = (word) (0x1000 + NTypes);
        NTypes ++;
        Types->PutS(ptr, full_len);
        TypesAlign ();
        switch (ptr -> tag) {
                case LF_MODIFIER:
                        if (! ptr -> value.modifier.attribute) {
                                Types->Index = index;
                                NTypes --;
                                tp = ProcessType (ptr -> value.modifier.index);
                                NewTypes [x] = tp;
                                return tp;
                        }
                        Recalc (value.modifier.index);
                        break;
                case LF_POINTER:
                        Recalc (value.pointer.type);
                        switch (ptr -> value.pointer.attribute & 31) {
                        case 6:
                        case 7: memset (Types->Ptr + index + 8, 0, full_len - 8);
                                ProcessRef (8 + (byte *) ptr,
                                            full_len + (byte *) ptr);
                                memcpy (Types->Ptr + index + 8,
                                        8 + (byte *) ptr, full_len - 8);
                                break;
                        case 8: Recalc (value.pointer.variant [0]);
                                break;
                        }
                        switch ((ptr -> value.pointer.attribute >> 5) & 7) {
                        case 2:
                        case 3: Recalc (value.pointer.variant [0]);
                                break;
                        }
                        break;
                case LF_ARRAY:
                        Recalc (value.array.elemtype);
                        Recalc (value.array.idxtype);
                        break;
                case LF_CLASS:          /* Later: fill derivation list */
                case LF_STRUCTURE:
                        Recalc (value.structure.field);
                        Recalc (value.structure.vshape);
                        break;
                case LF_UNION:
                        Recalc (value.t_union.field);
                        break;
                case LF_ENUM:
                        Recalc (value.t_enum.type);
                        Recalc (value.t_enum.flist);
                        break;
                case LF_PROCEDURE:
                        Recalc (value.procedure.rvtype);
                        Recalc (value.procedure.arglist);
                        break;
                case LF_MFUNCTION:
                        Recalc (value.mfunction.rvtype);
                        Recalc (value.mfunction.t_class);
                        Recalc (value.mfunction.t_this);
                        Recalc (value.procedure.arglist);
                        break;
                case LF_COBOL0:
                        Recalc (value.cobol0.parent);
                        break;
                case LF_BARRAY:
                        Recalc (value.barray.type);
                        break;
                case LF_DIMARRAY:
                        Recalc (value.dimarray.utype);
                        Recalc (value.dimarray.diminfo);
                        break;
                case LF_VFTPATH:
                        for (i = 0; i < ptr -> value.vftpath.count; i ++) {
                                Recalc (value.vftpath.bases [i]);
                        }
                        break;
                case LF_OEM:
                        for (i = 0; i < ptr -> value.oem.count; i ++) {
                                Recalc (value.oem.indices [i]);
                        }
                        break;
                case LF_ARGLIST:
                        for (i = 0; i < ptr -> value.arglist.count; i ++) {
                                Recalc (value.arglist.indices [i]);
                        }
                        break;
                case LF_LIST:
                case LF_FIELDLIST:
                        ProcessSubfields (4 + (byte *) ptr, full_len - 4);
                        memcpy (Types->Ptr + index, ptr, full_len);
                        break;
                case LF_DEFARG:
                        Recalc (value.defarg.index);
                        break;
                case LF_BITFIELD:
                        Recalc (value.bitfield.type);
                        break;
                case LF_METHODLIST:
                        for (i = 2; i < full_len / 2;) {
                                tp = ProcessType (((word *) ptr) [i + 1]);
                                ((word *) (Types->Ptr + index)) [i + 1] = tp;
                                i += (MemberAttr (((word *) ptr) [i]) == MA_INTODUCING_VIRTUAL) ? 8 : 4;
                        }
                        break;
                case LF_DIMCONU:
                case LF_DIMCONLU:
                        Recalc (value.dimconu.index);
                        break;
                case LF_DIMVARU:
                        Recalc (value.dimvaru.index);
                        for (i = 0; i < ptr -> value.dimvaru.rank; i ++) {
                                Recalc (value.dimvaru.var [i]);
                        }
                        break;
                case LF_DIMVARLU:
                        Recalc (value.dimvarlu.index);
                        for (i = 0; i < ((dword)ptr->value.dimvaru.rank) * 2; i ++) {
                                Recalc (value.dimvaru.var [i]);
                                Recalc (value.dimvaru.var [i + 1]);
                        }
                        break;
                case LF_REFSYM:
                        ProcessRef (4 + (byte *) ptr, full_len + (byte *) ptr);
                        memcpy (Types->Ptr + index, ptr, full_len);
                        break;
                default:
                        Types->Index = index;
                        full_len = 4;
                        ptr -> len = 2;
                        ptr -> tag = LF_NULL;
                        Types->PutS(ptr, 4);
                        break;
        }
        if (oldCnt == NTypes - 1) {
            ptr = (struct type *) (Types->Ptr + index);
            int i = findEqualType (ptr);
            if (i != 0) {
                ASSERT(i < 65536);
                NTypes --;
                Types->Index = index;
                NewTypes [x] = (word) i;
                return (word) i;
            }
        }
        ASSERT (oldCnt + 0x1000 < 65536);
        addType2HMap (oldCnt + 0x1000);
        return (word) (oldCnt + 0x1000);
}

/*----------------------------------------------------------------------------*/

/*
  По прочитанному номеру типа в OBJ-файле пересчитать его глобальный номер
*/

static void ConvertType (word * tp)
{
        * tp = ProcessType (* tp);
}

/*----------------------------------------------------------------------------*/

static dword GlobalTypesOffset, GlobalTypesLen;

static void WriteGlobalTypes (void)
{
        GlobalTypesOffset = DebugInfo->Index;
        DebugInfo->Put4(1);              /* Signature */
        DebugInfo->Put4(NTypes);
        DebugInfo->PutS(TypesDirectory, NTypes * 4);
        DebugInfo->PutS(Types->Ptr, Types->Index);
        DebugInfo->Align4();
        GlobalTypesLen = DebugInfo->Index - GlobalTypesOffset;
        NEntries ++;
}

/*----------------------------------------------------------------------------*/
/*                                                                            */
/*                        Чтение и запись Codeview символов                   */
/*                                                                            */
/*----------------------------------------------------------------------------*/

#define S_COMPILE       0x0001
#define S_REGISTER      0x0002
#define S_CONSTANT      0x0003
#define S_UDT           0x0004
#define S_SSEARCH       0x0005
#define S_END           0x0006
#define S_OBJNAME       0x0009
#define S_ENDARG        0x000A
#define S_MANYREG       0x000C
#define S_RETURN        0x000D

#define S_BPREL32       0x0200
#define S_LDATA32       0x0201
#define S_GDATA32       0x0202
#define S_PUB32         0x0203
#define S_LPROC32       0x0204
#define S_GPROC32       0x0205
#define S_THUNK32       0x0206
#define S_BLOCK32       0x0207
#define S_WITH32        0x0208
#define S_REGREL32      0x020C

#define S_PROCREF       0x0400
#define S_DATAREF       0x0401
#define S_ALIGN         0x0402

struct symbol {
        word len;
        word tag;
        union {
                struct {
                        dword offset;
                        word  segment;
                } ssearch;
                struct {
                        word  type;
                        word  reg;
                        byte  name [1];
                } reg;
                struct {
                        word  type;
                        byte  count;
                        byte  filler [1];
                } many_regs;
                struct {
                        word  type;
                        byte  filler [1];
                } constant;
                struct {
                        word  type;
                        byte  name [1];
                } udt;
                struct {
                        dword offset;
                        word  type;
                        byte  name [1];
                } bprel32;
                struct {
                        dword offset;
                        word  segment;
                        word  type;
                        byte  name [1];
                } data32;
                struct {
                        dword pParent;
                        dword pEnd;
                        dword pNext;
                        dword length;
                        dword debug_start;
                        dword debug_end;
                        dword offset;
                        word  segment;
                        word  type;
                        byte  flags;
                        byte  name [1];
                } proc32;
                struct {
                        dword pParent;
                        dword pEnd;
                        dword pNext;
                        dword offset;
                        word  segment;
                        word  length;
                        byte  ordinal;
                        byte  name [1];
                } thunk32;
                struct {
                        dword pParent;
                        dword pEnd;
                        dword length;
                        dword offset;
                        word  segment;
                        byte  name [1];
                } block32;
                struct {
                        dword pParent;
                        dword pEnd;
                        dword length;
                        dword offset;
                        word  segment;
                        byte  expr [1];
                } with32;
                struct {
                        dword offset;
                        word  reg;
                        word  type;
                        byte  name [1];
                } regrel32;
        } value;
};

#define UNDEF_SEG       0

/*----------------------------------------------------------------------------*/

/*
  Пройтись по всем файлам и для тех из них, для которых хоть что-то
  попало в EXE-файл, собрать в список сегменты с символами и типами
*/

static void CollectDebugSegments (void)
{
        OBJFile * f;

        for (f = FileList; f; f = f -> next) {
                Segment ** types   = & (f -> types);
                Segment ** symbols = & (f -> symbols);
                Bool used = false;
                Segment * s;

                for (s = f -> segs; s; s = s -> next) {
                        if (s -> isProcessed ())
                                used = true;
                        else if (s -> name == SYMBOLS_OMF || s -> name == SYMBOLS_COFF)
                        {
                                * symbols = s;
                                symbols = & (s -> link);
                        }
                        else if (s -> name == TYPES_OMF || s -> name == TYPES_COFF)
                        {
                                * types = s;
                                types = & (s -> link);
                        }
                }
                if (! used)
                        f -> types = f -> symbols = NULL;
        }
}

/*----------------------------------------------------------------------------*/

/*
  Найти fixup по сегменту и смещению в нем; для ускорения поиска
  можно передать предыдущий найденный fixup (или NULL)
  Предполагается, что если передали,
  то от вызова к вызову смещения увеличиваются!
*/

static struct fixup * FindFixup (Segment * s, int offset, struct fixup * f)
{
        int n;

        if (f == NULL) {
                n = s -> nfixups;
                f = s -> fixups;
        }
        else
                n = s -> nfixups - (++ f - s -> fixups);
        for (; n; f ++, n --)
                if (f -> offset == offset)
                        return f;
        return NULL;
}

/*----------------------------------------------------------------------------*/

/*
  Игнорировать все не попавшие в EXE-файл символы
*/

static struct symbol * IgnoreSymbols (struct symbol * ptr)
{
        dword scope;

        scope = 1;
        for (;;) {
                dword tag;

                tag = ptr -> tag;
                ptr = (struct symbol *) (ptr -> len + 2 + (byte *) ptr);
                switch (tag) {
                        case S_END:     if (! -- scope)
                                                return ptr;
                                        break;
                        case S_WITH32:
                        case S_LPROC32:
                        case S_GPROC32:
                        case S_THUNK32:
                        case S_BLOCK32: scope ++;
                                        break;
                }
        }
}

/*----------------------------------------------------------------------------*/

static Segment * CurrentSegment;
static Bool         WasCode;            /* т.е. надо ли выдавать S_SEARCH */

/*
  Пройти по сегменту с символами:
  - пересчитать типы
  - посчитать адреса и смещения
  - для не попавших в EXE-файл объектов (верхнего scope)
    в поле segment положить UNDEF_SEG
  - посмотреть, есть ли процедура в сегменте кода (т.е. нужен ли SSEARCH)
  - посчитать количество локалов и глобалов
*/

static void PreprocessSymbols (void * start, void * fin)
{
        struct fixup  * f   = NULL;
        struct symbol * ptr = (struct symbol *) start;

        while ((void *)ptr < fin) {
                dword tag, len, addr;

                len = ptr -> len;
                tag = ptr -> tag;
                switch (tag) {
                case S_REGISTER:
                        ConvertType (& ptr -> value.reg.type);
                        break;
                case S_MANYREG:
                        ConvertType (& ptr -> value.many_regs.type);
                        break;
                case S_CONSTANT:
                        ConvertType (& ptr -> value.constant.type);
                        break;
                case S_UDT:
                        ConvertType (& ptr -> value.udt.type);
                        break;
                case S_BPREL32:
                        ConvertType (& ptr -> value.bprel32.type);
                        break;
                case S_REGREL32:
                        ConvertType (& ptr -> value.regrel32.type);
                        break;
                case S_LDATA32:
                case S_GDATA32:
                        f = FindFixup (CurrentSegment,
                                       ((byte *) ptr) - CurrentSegment->getText() + 4,
                                       f);
                        if (f && SymbolInExe (f, & addr)) {
                                CalcSO (addr + ptr -> value.data32.offset,
                                        & ptr -> value.data32.offset,
                                        & ptr -> value.data32.segment);
                                ConvertType (& ptr -> value.data32.type);
                                NGlobals += (tag == S_GDATA32);
                                NLocals  += (tag == S_LDATA32);
                        }
                        else
                                ptr -> value.data32.segment = UNDEF_SEG;
                        break;
                case S_LPROC32:
                case S_GPROC32:
                        f = FindFixup (CurrentSegment,
                                       ((byte *) ptr) - CurrentSegment->getText() + 28,
                                       f);
                        if (f && SymbolInExe (f, & addr)) {
                                CalcSO (addr + ptr -> value.proc32.offset,
                                        & ptr -> value.proc32.offset,
                                        & ptr -> value.proc32.segment);
                                WasCode |= ptr -> value.proc32.segment == CodeSeg;
                                ConvertType (& ptr -> value.proc32.type);
                                NGlobals += (tag == S_GPROC32);
                                NLocals  += (tag == S_LPROC32);
                        }
                        else {
                                ptr -> value.proc32.segment = UNDEF_SEG;
                                ptr = IgnoreSymbols ((struct symbol *) (len + 2 + (byte *) ptr));
                                continue;
                        }
                        break;
                case S_WITH32:
                case S_THUNK32:
                        ptr = IgnoreSymbols ((struct symbol *) (len + 2 + (byte *) ptr));
                        continue;
                case S_BLOCK32:
                        f = FindFixup (CurrentSegment,
                                       ((byte *) ptr) - CurrentSegment->getText() + 20,
                                       f);
                        if (f && SymbolInExe (f, & addr)) {
                                CalcSO (addr + ptr -> value.block32.offset,
                                        & ptr -> value.block32.offset,
                                        & ptr -> value.block32.segment);
                        }
                        else {
                                ptr -> value.block32.segment = UNDEF_SEG;
                                ptr = IgnoreSymbols ((struct symbol *) (len + 2 + (byte *) ptr));
                                continue;
                        }
                        break;
                }
                ptr = (struct symbol *) (len + 2 + (byte *) ptr);
        }
}

/*----------------------------------------------------------------------------*/

/*
  Начать запись символа; записать перед ним S_ALIGN, если надо
*/

static void StartSym (dword len, dword start)
{
        dword pos = DebugInfo->Index - start;

        if ((pos >> 12) != ((pos + len + 8) >> 12)) {
                dword align_len;

                align_len = 4096 - (pos & 4095);
                DebugInfo->Put2( align_len - 2);
                DebugInfo->Put2( S_ALIGN);
                DebugInfo->ZeroBytes(align_len - 4);
        }
        DebugInfo->Check(len);
}

/*----------------------------------------------------------------------------*/

static dword moduleStart;    /* Смещение начала символов модуля */
static dword FirstCode;      /* Смещение первой процедуры относительно moduleStart */

#define CurrentIndex    (DebugInfo->Index - moduleStart)

/*
  Записать все символы из одного сегмента, которые попали в EXE-файл.
  При этом для глобалов в поле offset в сегменте OBJ-файла пишется смещение
  его начала (чтобы потом сделать S_*REF). Если же кому-то после потребуется
  именно смещение символа, его можно получить из sstAlignSym этого модуля.
*/

static struct symbol * ProcessSymbols (void *   start,
                              void *   fin,
                              dword pParent,
                              dword pEnd     /* Procedure will fill it */
                             )
{
        struct symbol * ptr = (struct symbol *) start;
        dword pNext = 0;

        while ((void *)ptr < fin) {
                dword full_len;

                full_len = ptr -> len + 2;
                switch (ptr -> tag) {
                case S_COMPILE:
                case S_OBJNAME:
                case S_ENDARG:
                case S_RETURN:
                case S_REGISTER:
                case S_MANYREG:
                case S_CONSTANT:
                case S_UDT:
                case S_BPREL32:
                case S_REGREL32:
                        StartSym (full_len, moduleStart);
                        DebugInfo->PutS(ptr, full_len);
                        //DebugInfo->Align4();
                        break;
                case S_LDATA32:
                case S_GDATA32:
                        if (ptr -> value.data32.segment != UNDEF_SEG) {
                                dword start;

                                StartSym (full_len, moduleStart);
                                start = CurrentIndex;
                                DebugInfo->PutS(ptr, full_len);
                                //DebugInfo->Align4();
                                ptr -> value.data32.offset = start;
                        }
                        break;
                case S_LPROC32:
                case S_GPROC32:
                        if (ptr -> value.proc32.segment != UNDEF_SEG) {
                                dword start;

                                StartSym (full_len, moduleStart);
                                start = CurrentIndex;
                                if (! FirstCode && ptr -> value.proc32.segment == CodeSeg)
                                        FirstCode = start;
                                if (pNext)
                                        * (dword *) (DebugInfo->Ptr + pNext) = start;
                                pNext = DebugInfo->Index + 12;
                                ptr -> value.proc32.pParent = pParent;
                                DebugInfo->PutS(ptr, full_len);
                                //DebugInfo->Align4();
                                ptr -> value.proc32.offset = start;
                                ptr = ProcessSymbols (full_len + (byte *) ptr,
                                                      (void *) -1,
                                                      start,
                                                      start + moduleStart + 8);
                        }
                        else
                                ptr = IgnoreSymbols ((struct symbol *) (full_len + (byte *) ptr));
                        continue;
                case S_WITH32:
                case S_THUNK32:
                        ptr = IgnoreSymbols ((struct symbol *) (full_len + (byte *) ptr));
                        continue;
                case S_BLOCK32:
                        if (ptr -> value.block32.segment != UNDEF_SEG) {
                                dword start;

                                StartSym (full_len, moduleStart);
                                start = CurrentIndex;
                                ptr -> value.block32.pParent = pParent;
                                DebugInfo->PutS(ptr, full_len);
                                //DebugInfo->Align4();
                                ptr -> value.block32.offset = start;
                                ptr = ProcessSymbols (full_len + (byte *) ptr,
                                                      (void *) -1,
                                                      start,
                                                      start + moduleStart + 8);
                        }
                        else
                                ptr = IgnoreSymbols ((struct symbol *) (full_len + (byte *) ptr));
                        continue;
                case S_END:
                        StartSym (full_len, moduleStart);
                        * (dword *) (DebugInfo->Ptr + pEnd) = CurrentIndex;
                        DebugInfo->Put2( 2);
                        DebugInfo->Put2( S_END);
                        return (struct symbol *)(((byte *) ptr) + full_len);
                }
                ptr = (struct symbol *) (full_len + (byte *) ptr);
        }
        return ptr;
}

/*----------------------------------------------------------------------------*/

/*
  Записать все символы в секцию sstAlignSym
*/

static void AlignSymsPut (Segment * s)
{
        Segment * p;

        WasCode = false;
        for (p = s; p; p = p -> link) {
                CurrentSegment = p;
                if ((p -> getLen() > 4) && (* (dword *) (p -> getText()) == 1))
                        PreprocessSymbols (p -> getText() + 4,
                                           p -> getText() + p -> getLen());
        }
        moduleStart = DebugInfo->Index - 4;
        if (WasCode) {
                DebugInfo->Put2( 10);              /* Length */
                DebugInfo->Put2( S_SSEARCH);
                DebugInfo->Put4(0);
                DebugInfo->Put2( CodeSeg);
                DebugInfo->Put2( 0);               /* Align */
        }
        FirstCode = 0;
        do {
                CurrentSegment = s;
                if ((s -> getLen() > 4) && (* (dword *) (s -> getText()) == 1))
                        ProcessSymbols (s -> getText() + 4,
                                        s -> getText() + s -> getLen(), 0, 0);
                s = s -> link;
        } while (s != NULL);
        if (WasCode)
                ((struct symbol *) (DebugInfo->Ptr + moduleStart + 4)) -> value.ssearch.offset = FirstCode;
}

/*----------------------------------------------------------------------------*/

static void AlignSyms (struct DebugInfoModule * mod)
{
    OBJFile * f;
    Segment  * s;

    f = mod -> file;
    if (! f) return;

    CollectTypes (f -> types);
    s = f -> symbols;
    if (! s) return;

    mod -> align_offs = DebugInfo->Index;

    /* Signature */
    DebugInfo->Put4(1);          

    AlignSymsPut (s);

    mod -> align_len = DebugInfo->Index - mod -> align_offs;
    NEntries ++;
}

/*----------------------------------------------------------------------------*/
/*                                                                            */
/*                Запись sstGlobalSym, sstStaticSym и sstGlobalPub            */
/*                                                                            */
/*----------------------------------------------------------------------------*/

/*
  Счет хэш-суммы идентификатора
*/

#define byt_toupper(b)   ((b)  & 0xDF)
#define dwrd_toupper(dw) ((dw) & 0xDFDFDFDF)
#define _lrotl(x, n)     x = (((x) << (n)) | ((x) >> (32 - (n))))

static dword CalcHash (const char * lpbName, int cb)
{
        dword ulEnd, ulSum, cul, iul;

        ulEnd = 0;
        while (cb & 3) {
                ulEnd |= byt_toupper (lpbName [cb - 1]);
                ulEnd <<= 8;
                cb --;
        }
        const dword * lpulName = (const dword *) lpbName;
        ulSum = 0;
        cul = cb / 4;
        for (iul = 0; iul < cul; iul ++) {
                ulSum ^= dwrd_toupper (lpulName [iul]);
                _lrotl (ulSum, 4);
        }
        return ulSum ^ ulEnd;
}

/*----------------------------------------------------------------------------*/

struct sym {
        dword offs;          /* Смещение относительно начала sst*Sym */
        dword hash;          /* Хэш-сумма имени                      */
        dword addr;          /* На самом деле смещение в секции      */
        word  bucket;
        word  sect;
};

/*----------------------------------------------------------------------------*/

static dword CurrentModuleAlign;     /* Начало символов модуля   */
static dword CurrentModule;          /* Номер текущего модуля    */
static dword StartSstSym;            /* Смещение начала таблицы  */
static dword Nb;                     /* Сколько у нас buckets    */

/*
  Сбор информации о символах и одновременная формирование *ref в sst*Sym
*/

static struct sym * CollectSegment (word         proc,
                                    word         data,
                                    void *       start,
                                    void *       fin,
                                    struct sym * s
                                   )
{
        struct symbol * ptr = (struct symbol *) start;

        while ((void *)ptr < fin) {
                dword tag, len;

                len = ptr -> len;
                tag = ptr -> tag;
                if (tag == proc) {
                        if (ptr -> value.proc32.segment != UNDEF_SEG) {
                                s -> offs = DebugInfo->Index - StartSstSym;
                                s -> hash = CalcHash ( (char *)(ptr->value.proc32.name + 1),
                                                      ptr->value.proc32.name[0]);
                                ASSERT((s -> hash % Nb) < 65536);
                                s -> bucket = (word) (s -> hash % Nb);
                                s -> addr   = ((struct symbol *) (DebugInfo->Ptr + CurrentModuleAlign +
                                                                  ptr -> value.proc32.offset))
                                              -> value.proc32.offset;
                                s -> sect   = ptr -> value.proc32.segment;
                                StartSym (16, StartSstSym);
                                DebugInfo->Put2( 14);
                                DebugInfo->Put2( S_PROCREF);
                                DebugInfo->Put4(s -> hash);
                                DebugInfo->Put4(ptr -> value.proc32.offset);
                                DebugInfo->Put2( CurrentModule);
                                DebugInfo->Put2( 0);
                                s ++;
                        }
                }
                else if (tag == data) {
                        if (ptr -> value.data32.segment != UNDEF_SEG) {
                                s -> offs = DebugInfo->Index - StartSstSym;
                                s -> hash = CalcHash ((char *)(ptr->value.data32.name + 1),
                                                      ptr->value.data32.name[0]);
                                ASSERT((s -> hash % Nb) < 65536);
                                s -> bucket = (word) (s -> hash % Nb);
                                s -> sect   = ptr -> value.data32.segment;
                                s -> addr = ((struct symbol *) (DebugInfo->Ptr + CurrentModuleAlign +
                                                                ptr -> value.data32.offset))
                                                -> value.data32.offset;
                                StartSym (16, StartSstSym);
                                DebugInfo->Put2( 14);
                                DebugInfo->Put2( S_DATAREF);
                                DebugInfo->Put4(s -> hash);
                                DebugInfo->Put4(ptr -> value.data32.offset);
                                DebugInfo->Put2( CurrentModule);
                                DebugInfo->Put2( 0);
                                s ++;
                        }
                }
                else
                        switch (tag) {
                        case S_LPROC32:
                        case S_GPROC32:
                                if (ptr -> value.proc32.segment == UNDEF_SEG) {
                                        ptr = IgnoreSymbols ((struct symbol *) (len + 2 + (byte *) ptr));
                                        continue;
                                };
                                break;
                        case S_WITH32:
                        case S_THUNK32:
                                ptr = IgnoreSymbols ((struct symbol *) (len + 2 + (byte *) ptr));
                                continue;
                        case S_BLOCK32:
                                if (ptr -> value.block32.segment == UNDEF_SEG) {
                                        ptr = IgnoreSymbols ((struct symbol *) (len + 2 + (byte *) ptr));
                                        continue;
                                };
                                break;
                        }
                ptr = (struct symbol *) (len + 2 + (byte *) ptr);
        }
        return s;
}

/*----------------------------------------------------------------------------*/

/*
  Пройтись по всем модулям и собрать все нужные символы
*/

static void CollectSymbols (word         proc,
                            word         data,
                            struct sym * s
                           )
{
        struct DebugInfoModule * mod;

        for (mod = DebugInfoModules; mod; mod = mod -> next) {
                OBJFile * f;
                Segment  * q;

                f = mod -> file;
                if (! f)
                        continue;
                CurrentModule = mod -> no;
                CurrentModuleAlign = mod -> align_offs;
                for (q = f -> symbols; q; q = q -> link)
                        if ((q -> getLen() > 4) && (* (dword *) (q -> getText()) == 1))
                                s = CollectSegment (proc, data, q -> getText() + 4,
                                                    q -> getText() + q -> getLen(), s);
        }
}

/*----------------------------------------------------------------------------*/

/*
  Посчитать количество public-ов в EXE-файла
*/

int NPublics = 0;

void CalcSymbols (ident id, void *info) {
    nameInfo * n = (nameInfo *)info;
    if (n && n -> seg && (n  -> seg -> isProcessed ()) &&
       (NAMES.Index2StrLen (id) < 255))
    {
        NPublics++;
    }
}

/*----------------------------------------------------------------------------*/

/*
  Записать один S_PUB32
*/

static void WritePub (const char * p, dword offset, word seg, int type)
{
        dword full_len, plen;

        plen = strlen (p);
        full_len = (3 + 12 + 1 + plen) & ~3;
        StartSym (full_len, StartSstSym);
        DebugInfo->Put2( full_len - 2);
        DebugInfo->Put2( S_PUB32);
        DebugInfo->Put4(offset);
        DebugInfo->Put2( seg);
        DebugInfo->Put2( type);
        DebugInfo->PutB( plen);
        DebugInfo->PutS(p, plen);
        DebugInfo->Align4();
}

/*----------------------------------------------------------------------------*/

/*
  Собрать все public-и из нашего EXE-файла
*/

struct sym * Symbols = NULL;
int NSymbols = 0;
int curSym = 0;

void CollectPublics (ident id, void *info) {
    nameInfo * n = (nameInfo *)info;

    if (n && n -> seg && (n -> seg -> isProcessed ()) &&
       (NAMES.Index2StrLen(id) < 255))
    {
        ASSERT (curSym < NSymbols);
        Symbols [curSym].offs = DebugInfo->Index - StartSstSym;
        Symbols [curSym].hash = CalcHash (NAMES.Index2Str (id), NAMES.Index2StrLen(id));
        ASSERT((Symbols [curSym].hash % Nb) < 65536);

        Symbols [curSym].bucket = (word) (Symbols [curSym].hash % Nb);
        CalcSO (n -> offset, & (Symbols [curSym].addr), & (Symbols [curSym].sect));
        WritePub (NAMES.Index2Str(id), Symbols [curSym].addr, Symbols [curSym].sect, 0);
        curSym ++;
    }
}

#define ImportJumpsName   "IMPORT JUMPS"

void AddImportJumpsCodeviewPublic ()
{
    ASSERT (curSym < NSymbols);
    Symbols [curSym].offs = DebugInfo->Index - StartSstSym;
    Symbols [curSym].hash = CalcHash (ImportJumpsName, strlen(ImportJumpsName));
    ASSERT((Symbols [curSym].hash % Nb) < 65536);

    Symbols [curSym].bucket = (word) (Symbols [curSym].hash % Nb);

    Symbols [curSym].addr = ImportStart - CodeStart;
    Symbols [curSym].sect = (word) CodeSeg;
    WritePub (ImportJumpsName, Symbols [curSym].addr, Symbols [curSym].sect, 0);
    curSym ++;
}

/*----------------------------------------------------------------------------*/

#define NBuckets(n)     ((n) <   50 ?   7:      \
                         (n) <  100 ?  19:      \
                         (n) <  250 ?  37:      \
                         (n) <  500 ?  79:      \
                         (n) < 1000 ? 137:      \
                         (n) < 2500 ? 293:      \
                                      499)      \

/*----------------------------------------------------------------------------*/

static int XCDECL compare_buckets (const void * p1, const void * p2)
{
        struct sym * s1, * s2;

        s1 = (struct sym *) p1;
        s2 = (struct sym *) p2;
        return (s1 -> bucket < s2 -> bucket) ? -1 :
               (s1 -> bucket > s2 -> bucket) ?  1 :
               (s1 -> offs   < s2 -> offs)   ? -1 :
               (s1 -> offs   > s2 -> offs)   ?  1 :
                                                0 ;
}

static int XCDECL compare_addrs (const void * p1, const void * p2)
{
        struct sym * s1, * s2;

        s1 = (struct sym *) p1;
        s2 = (struct sym *) p2;
        return (s1 -> sect < s2 -> sect) ? -1 :
               (s1 -> sect > s2 -> sect) ?  1 :
               (s1 -> addr < s2 -> addr) ? -1 :
               (s1 -> addr > s2 -> addr) ?  1 :
               (s1 -> offs < s2 -> offs) ? -1 :
               (s1 -> offs > s2 -> offs) ?  1 :
                                            0 ;
}

static dword GlobalSymOffset, GlobalSymLen;
static dword StaticSymOffset, StaticSymLen;
static dword GlobalPubOffset, GlobalPubLen;

/*
  Процедура записи sstGlobalSym/sstStaticSym/sstGlobalPub
*/

static void WriteSymbols (word    proc,
                          word    data,
                          dword * offset,
                          dword * length,
                          int     N
                         )
{
        dword ind, ind0;
        struct sym * u;
        dword * r;
        int i, j, nb;
        dword counts [500];

        NEntries ++;
        * offset = ind = DebugInfo->Index;
        DebugInfo->Check(16);
/*
  Do not write empty table
*/
        if (! N)
                DebugInfo->ZeroBytes(16);
        else {
/*
  Write header
*/
                if (data == S_LDATA32)
                        DebugInfo->Put4(10);
                else
                        DebugInfo->Put4((12 << 16) + 10);
                DebugInfo->ZeroBytes(12);
                StartSstSym = DebugInfo->Index;
/*
  Write symbols
*/
                Nb = nb = NBuckets (N);
                NSymbols = N;
                Symbols  = (struct sym *) xalloc (NSymbols * sizeof (struct sym));
                if (data == S_PUB32) {
                    NAMES.Iterate (CollectPublics);

                    if (xAddImportJumpsCodeviewPublic) {
                        AddImportJumpsCodeviewPublic ();
                    }
                } else
                    CollectSymbols (proc, data, Symbols);
                if (data != S_LDATA32) {
                        DebugInfo->Put2( 6);
                        DebugInfo->Put2( S_ALIGN);
                        DebugInfo->Put4((dword)(-1));
                }
                * (dword *) (DebugInfo->Ptr + ind + 4) = DebugInfo->Index - StartSstSym;
/*
  Write hash table
*/
                qsort (Symbols, N, sizeof (struct sym), compare_buckets);
                ind0 = DebugInfo->Index;
                * (dword *) (DebugInfo->Ptr + ind + 8) = j = 4 + 8 * nb + 8 * N;
                DebugInfo->Check(j);
                DebugInfo->Put2(nb);
                DebugInfo->Put2(0);
                DebugInfo->ZeroBytes(8 * nb);
                memset (counts, 0, nb * sizeof (dword));
                for (i = 0, u = Symbols; i < N; i ++, u ++) {
                        counts [u -> bucket] ++;
                        DebugInfo->Put4(u -> offs);
                        DebugInfo->Put4(u -> hash);
                }
                memcpy (DebugInfo->Ptr + ind0 + 4 + 4 * nb, counts, 4 * nb);
                for (i = j = 0, r = (dword *) (DebugInfo->Ptr + ind0 + 4);
                     i < nb;        
                     j += 8 * counts [i], i ++, r ++)
                        * r = counts [i] ? j : 0;
/*
  Write sort table
*/
                if (data != S_LDATA32) {
                        qsort (Symbols, N, sizeof (struct sym), compare_addrs);
                        ind0 = DebugInfo->Index;
                        nb = (CodeSeg != 0) + (DataSeg != 0) + (BSSSeg != 0) + (RDataSeg != 0);
                        * (dword *) (DebugInfo->Ptr + ind + 12) = j = 4 + 8 * nb + 8 * N;
                        DebugInfo->Check(j);
                        DebugInfo->Put2(nb);
                        DebugInfo->Put2(0);
                        DebugInfo->ZeroBytes(8 * nb);
                        memset (counts, 0, nb * sizeof (dword));
                        for (i = 0, u = Symbols; i < N; i ++, u ++) {
                                counts [u -> sect - 1] ++;
                                DebugInfo->Put4(u -> offs);
                                DebugInfo->Put4(u -> addr);
                        }
                        memcpy (DebugInfo->Ptr + ind0 + 4 + 4 * nb, counts, 4 * nb);
                        for (i = j = 0, r = (dword *) (DebugInfo->Ptr + ind0 + 4);
                             i < nb;
                             j += 8 * counts [i], i ++, r ++)
                                * r = counts [i] ? j : 0;
                }
                NSymbols = 0;
                xfree (Symbols);
        }
/*
  Ok, we've wrote it all
*/
        * length = DebugInfo->Index - ind;
}

/*----------------------------------------------------------------------------*/
/*                                                                            */
/*                    Запись таблицы модулей, номеров строк, ...              */
/*                                                                            */
/*----------------------------------------------------------------------------*/

static void WriteModules (void)
{
    struct DebugInfoModule * mod;

    for (mod = DebugInfoModules; mod; mod = mod -> next) {
        mod -> name_offs = DebugInfo->Index;
        DebugInfo->Put4(0);
        DebugInfo->Put2(1);
        DebugInfo->PutS("CV", 2);

        /* SegInfo */
        DebugInfo->Put2(CodeSeg);
        DebugInfo->Put2(0);
        DebugInfo->Put4(mod -> first_seg -> address - CodeStart);
        DebugInfo->Put4(mod -> last_seg  -> address + mod -> last_seg -> getLen() -
                        mod -> first_seg -> address);

        DebugInfo->PutName(mod -> name);

        DebugInfo->Align4();
        mod -> name_len = DebugInfo->Index - mod -> name_offs;
        NEntries ++;
    }
}

/*----------------------------------------------------------------------------*/

static int LineNums (struct DebugInfoModule * mod)
{
    Segment * seg;
    int n;

    for (n = 0, seg = mod -> first_seg;; seg = seg -> link) {
        n += seg -> nlines;
        if (seg == mod->last_seg) return n;
    }
}

static void ModuleLineNums (struct DebugInfoModule * mod, int n)
{
    dword srcBase = mod -> linenums_offs = DebugInfo->Index;
    dword start   = mod -> first_seg -> address - CodeStart;
    dword end     = mod -> last_seg  -> address + mod -> last_seg -> getLen() - CodeStart - 1;

    /* Module header */
    DebugInfo->Put2 (1);
    DebugInfo->Put2 (1);
    DebugInfo->Put4 (20);
    DebugInfo->Put4 (start);
    DebugInfo->Put4 (end);
    DebugInfo->Put2 (1);
    DebugInfo->Put2 (0);

    /* File table */
    DebugInfo->Put2 (1);
    DebugInfo->Put2 (0);
    dword ind = DebugInfo->Index;
    DebugInfo->Put4 (0);
    DebugInfo->Put4 (start);
    DebugInfo->Put4 (end);
    if (mod->file->getSource() != NULL)
        DebugInfo->PutName (mod -> file -> getSource());
    else
        DebugInfo->PutName (UNKNOWN_SRC_NAME);
    * (dword *) (DebugInfo->Ptr + ind) = DebugInfo->Index - srcBase;

    /* Line numbers header */
    DebugInfo->Put2 (1);
    DebugInfo->Put2 (n);

    /* Fill arrays */
    ind = DebugInfo->Index;
    DebugInfo->ZeroBytes (4 * n);
    dword ind1 = DebugInfo->Index;
    DebugInfo->ZeroBytes (2 * n);

    n = 0;
    for (Segment * seg = mod -> first_seg;; seg = seg -> link) {
        if (seg -> nlines) {
            start = seg -> address - CodeStart;
            struct Line * line = seg -> lines;
            for (dword len = seg -> nlines; len; line++, n++, len--) {
                ((dword *) (DebugInfo->Ptr + ind )) [n] = line -> offset + start;
                ((short *) (DebugInfo->Ptr + ind1)) [n] = line -> line;
            }
        }
        if (seg == mod->last_seg) break;
    }
    DebugInfo->Align4 ();
    mod -> linenums_len = DebugInfo->Index - srcBase;
    NEntries ++;
}

/*----------------------------------------------------------------------------*/

static void WriteAlignSymAndSrcModules (void)
{
    int n;
    struct DebugInfoModule * mod;

    for (mod = DebugInfoModules; mod; mod = mod -> next) {
        AlignSyms (mod);
        n = LineNums (mod);
        if (n)
            ModuleLineNums (mod, n);
    }
}

/*----------------------------------------------------------------------------*/

static int SegTableOffset, SegTableLen;

static void WriteSegMap (void)
{
        int nsegs;

        SegTableOffset = DebugInfo->Index;
        nsegs = 1 + (DataLen != 0) + (BSSLen != 0);

        DebugInfo->Put2(nsegs);         /* Header */
        DebugInfo->Put2(nsegs);

        DebugInfo->Put2(0x0D);          /* code segment */
        DebugInfo->Put4(0);
        DebugInfo->Put2(CodeSeg);
        DebugInfo->Put4((dword) -1);
        DebugInfo->Put4(0);
        DebugInfo->Put4(CodeLen);

        if (DataLen) {                  /* data segment */
                DebugInfo->Put2(0x0B);
                DebugInfo->Put4(0);
                DebugInfo->Put2(DataSeg);
                DebugInfo->Put4((dword) -1);
                DebugInfo->Put4(0);
                DebugInfo->Put4(DataLen);
        }
        if (BSSLen) {                   /* BSS segment */
                DebugInfo->Put2(0x0B);
                DebugInfo->Put4(0);
                DebugInfo->Put2(BSSSeg);
                DebugInfo->Put4((dword) -1);
                DebugInfo->Put4(0);
                DebugInfo->Put4(BSSLen);
        }

        SegTableLen = DebugInfo->Index - SegTableOffset;
        NEntries ++;
}

static int FileIndexOffset, FileIndexLen; 

static void WriteFileIndex (void) {
    int cRef = 0, nRef = 0;
    short RefsPassed = 0;
    dword ind;
    FileIndexOffset = DebugInfo->Index;

    struct DebugInfoModule * mod;
    for (mod = DebugInfoModules; mod; mod = mod -> next)
        if (mod -> linenums_offs)
            nRef++;

    DebugInfo->Put2 (NDebugInfoModules);
    DebugInfo->Put2 (nRef);
    ind = DebugInfo->Index;

    DebugInfo->Check (NDebugInfoModules*(2 + 2 + 4));
    DebugInfo->ZeroBytes (NDebugInfoModules*4 + nRef*4);

    for (mod = DebugInfoModules; mod; mod = mod -> next) {
        if (mod -> linenums_offs) {
            *( (short*) (DebugInfo->Ptr+ind + cRef*2)) = RefsPassed;
            *( (short*) (DebugInfo->Ptr+ind + NDebugInfoModules*2 + cRef*2)) = 1;
            *( (dword*) (DebugInfo->Ptr+ind + NDebugInfoModules*4 + (RefsPassed++)*4)) = DebugInfo->Index - ind - NDebugInfoModules*4 - nRef*4;
            if (mod->file->getSource() != NULL)
                DebugInfo->PutName (getpart_name (mod->file->getSource()));
            else
                DebugInfo->PutName (UNKNOWN_SRC_NAME);
        }
        cRef++;
    }
    FileIndexLen = DebugInfo->Index - FileIndexOffset;                  
    NEntries ++;
}
/*----------------------------------------------------------------------------*/
/*                                                                            */
/*                      Запись всего этого хозяйства                          */
/*                                                                            */
/*----------------------------------------------------------------------------*/

static void DirEntry (word tag, word mod, int offset, int length)
{
        DebugInfo->Put2(tag);
        DebugInfo->Put2(mod);
        DebugInfo->Put4(offset);
        DebugInfo->Put4(length);
}

/*----------------------------------------------------------------------------*/

void CreateCVDbg ()
{
        memset (HTypeMap, 0, sizeof(HTypeMap));

        if (CodeLen)  CodeSeg  = getObjectNumber(CodeStart);
        if (DataLen)  DataSeg  = getObjectNumber(DataStart);
        if (BSSLen)   BSSSeg   = getObjectNumber(BSSStart);
        if (RDataLen) RDataSeg = getObjectNumber(RDataStart);

        CodeEnd  = CodeStart  + CodeLen;
        DataEnd  = DataStart  + DataLen;
        BSSEnd   = BSSStart   + BSSLen;
        RDataEnd = RDataStart + RDataLen;

        CollectDebugSegments ();

        NAMES.Iterate (CalcSymbols);

        if (xAddImportJumpsCodeviewPublic) {
            NPublics++;
        }

        Types = newStorage(65536);
        Types->Put4(1);

        DebugInfo = newStorage(65536);
        DebugInfo->PutS("NB09", 4);                               /* Signature */
        DebugInfo->Put4(0);                                       /* DirOffset */

        DebugInfo->Align4(); WriteModules               ();
        DebugInfo->Align4(); WriteAlignSymAndSrcModules ();
        DebugInfo->Align4(); WriteSymbols               (S_PUB32, S_PUB32,
                                          & GlobalPubOffset, & GlobalPubLen,
                                          NPublics);
        DebugInfo->Align4(); WriteSymbols               (S_GPROC32, S_GDATA32,
                                          & GlobalSymOffset, & GlobalSymLen,
                                          NGlobals);
     /* DebugInfo->Align4(); WriteLibraries             (); */
        DebugInfo->Align4(); WriteSymbols               (S_LPROC32, S_LDATA32,
                                          & StaticSymOffset, & StaticSymLen,
                                          NLocals);
        DebugInfo->Align4(); WriteGlobalTypes           ();
        DebugInfo->Align4(); WriteSegMap                ();
     /* DebugInfo->Align4(); WriteSegName               (); */
        DebugInfo->Align4(); WriteFileIndex             (); 
        DebugInfo->Align4();

        * (dword *) (DebugInfo->Ptr + 4) = DebugInfo->Index; /* DirOffset */

        /* Directory header */
        DebugInfo->Put2(16);
        DebugInfo->Put2(12);
        DebugInfo->Put4(NEntries);
        DebugInfo->ZeroBytes(8);

        /* Directory */
        struct DebugInfoModule * mod;
        for (mod = DebugInfoModules; mod; mod = mod -> next)
            DirEntry (0x120, mod -> no, mod -> name_offs, mod -> name_len);
        for (mod = DebugInfoModules; mod; mod = mod -> next) {
            if (mod -> align_len)
                DirEntry (0x125, mod -> no, mod -> align_offs, mod -> align_len);
            if (mod -> linenums_offs)
                DirEntry (0x127, mod -> no, mod -> linenums_offs, mod -> linenums_len);
        }
        DirEntry (0x12A, 0xFFFF, GlobalPubOffset,   GlobalPubLen);
        DirEntry (0x129, 0xFFFF, GlobalSymOffset,   GlobalSymLen);
     /* DirEntry (0x128, 0xFFFF, LibrariesOffset,   LibrariesLen); */
        DirEntry (0x134, 0xFFFF, StaticSymOffset,   StaticSymLen);
        DirEntry (0x12B, 0xFFFF, GlobalTypesOffset, GlobalTypesLen);
        DirEntry (0x12D, 0xFFFF, SegTableOffset,    SegTableLen);
     /* DirEntry (0x12E, 0xFFFF, SegNamesOffset,    SegNamesLen); */
        DirEntry (0x133, 0xFFFF, FileIndexOffset,   FileIndexLen); 

        DebugInfo->PutS("NB09", 4);                      /* Signature  */
        DebugInfo->Put4(DebugInfo->Index + 4);           /* BaseOffset */

        delete Types;
        freeTypesHMap();
}

close_namespace

