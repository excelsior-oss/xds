
#ifndef IDENTS_H
#define IDENTS_H

#include "xdefs.h"

typedef int ident;

#define INVALID_ID (ident)(-1)

struct id {
   int           index;
   struct id *   next;
   void *        info;
   int           length;
   char          name [1];
};

#define HashSize        65536    /* Hash table dictionary size */
#define N_IDS           65536    /* Linear index table delta */

typedef void (* iteratorCallback) (ident, void *);

class StringTable {
private:
    struct id *  idtable [HashSize];
    struct id ** ids;
    int          nIdents, MaxN;

public:
    StringTable ();
    ident Str2Index (const char *str);
    ident Str2Index (const char *str, int len);

    inline const char *Index2Str (ident index) const {
        return (index == INVALID_ID) ? 0 : ids[(int)index] -> name;
     }

    inline int Index2StrLen (ident index) const {
        return ids[(int)index] -> length;
    }

    inline void *getInfo (ident index) const {
        return ids[(int)index] -> info;
    }

    inline void setInfo (ident index, void *info) {
        ids[(int)index] -> info = info;
    }

    inline int getTableSize () const {
        return nIdents;
    }

    inline void Iterate (iteratorCallback callback) {
       for (int i = 1; i <= nIdents; i++)
           callback (i, ids[i]->info);
    }

    inline int Compare (ident id1, ident id2) const {
       return ((int)id1) - ((int)id2);
    }
};

extern StringTable NAMES;
extern void InitIds (void);

#undef CONST

/* Номера соответствующих имен */

extern ident CODE,
             CONST,
#if defined (STACK_SEG)
             STACK,
#endif
             DATA,
             IDATA,
             BSS,
             DEBUG,
             SYMBOLS_OMF,
             SYMBOLS_COFF,
             TYPES_OMF,
             TYPES_COFF,
             COMPONENT_KEY,
             NULLCHECKS,
             STACKTRACE,
             LOCAL_TYPES_TABLE,
             TD_DATA,
             EMPTY_ID;

extern Bool check_imp_var_suffix (char * name, Bool drop_suffix = true);


extern Bool HasExtension (const char * name);
extern char * MakeExtension (const char * name, const char * ext);
extern char * ConvDLLName (char * buf);

#define IsIDATA(p)    (p [0] == '.' && p [1] == 'i' && p [2] == 'd' && \
                       p [3] == 'a' && p [4] == 't' && p [5] == 'a')

#endif

/* Compare str1 and str2, ignoring case */
extern int strcmpIC (const char * str1, const char * str2);

/* Convert specified string to upper case */
extern void str2UpperCase (char * str);

/* Counts the length of unicode string */
extern int wcsLen (const unichar * str);
