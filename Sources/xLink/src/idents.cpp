#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "xdefs.h"

open_namespace

#include "idents.h"
#include "xmem.h"
#include "xdebug.h"
#include "struct.h"

/*-------------------------------------------------------------------------*/


StringTable NAMES;

static struct id empty = { 0, NULL, NULL, 0, '\0' };

/*----------------------------------------------------------------------------*/

StringTable::StringTable () {
    memset (idtable, (char) 0, sizeof (idtable));
    MaxN    = N_IDS;
    ids     = (struct id **) xalloc (N_IDS * sizeof (struct id *));
    * ids   = & empty;
    nIdents = 0;
}

static int hashCode (const char * p, int len) {
    int hash = 0;

    for (int i = 0; i < len; i++) {
        hash = hash*37 + p[i];
    }

    return hash;
}

ident StringTable::Str2Index (const char *p, int len) {
    while (len && p[len - 1] == ' ') {
        len --;
    }

    if (len == 0) {
        return 0;
    }

    ASSERT (len > 0);

    int hash  = hashCode (p, len);
    int index = hash & (HashSize-1);

    struct id * q, ** r;

    for (r = idtable + index; * r; r = & ((* r) -> next))
        if (len == (* r) -> length && !memcmp (p, (* r) -> name, len))
            return /*FIXME*/(ident)( (* r) -> index );

    * r = q = (struct id *) allocateForever (sizeof (struct id) + len);
    q -> next = NULL;
    q -> info = NULL;
    q -> index = ++ nIdents;
    q -> length = len;
    memcpy (q -> name, p, len);
    q -> name [len] = '\0';

    if (nIdents == MaxN) {
        MaxN += MaxN;
        ids = (struct id * *) xrealloc (ids,
                                        nIdents * sizeof (struct id *),
                                        MaxN    * sizeof (struct id *));
    }
    ids [nIdents] = q;
    return  /*FIXME*/(ident)nIdents;
}

ident StringTable::Str2Index (const char *str) {
    return Str2Index (str, strlen (str));
}

/*-------------------------------------------------------------------------*/

ident CODE,
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

/*-------------------------------------------------------------------------*/

void InitIds() {
    CODE  = NAMES.Str2Index ("CODE");
    CONST = NAMES.Str2Index ("CONST");
    DATA  = NAMES.Str2Index ("DATA");
    IDATA = NAMES.Str2Index ("IDATA");
    BSS   = NAMES.Str2Index ("BSS");
#if defined (STACK_SEG)
    STACK = NAMES.Str2Index ("STACK");
#endif
    DEBUG = NAMES.Str2Index ("DEBUG");
    SYMBOLS_OMF  = NAMES.Str2Index ("$$SYMBOLS");
    SYMBOLS_COFF = NAMES.Str2Index (".debug$S");
    TYPES_OMF    = NAMES.Str2Index ("$$TYPES");
    TYPES_COFF   = NAMES.Str2Index (".debug$T");

    COMPONENT_KEY = NAMES.Str2Index ("COMPONENT.KEY");

    NULLCHECKS    = NAMES.Str2Index ("NULLCHECKS");
    STACKTRACE    = NAMES.Str2Index ("STACKTRACE");
    TD_DATA       = NAMES.Str2Index ("TD_DATA");

    LOCAL_TYPES_TABLE = NAMES.Str2Index ("LINK_LocalTypesTable");

    EMPTY_ID = NAMES.Str2Index ("");
}

/*----------------------------------------------------------------------------*/

static Bool check_suffix(char * name, char * suffix, Bool drop_suffix) {
  char * pattr, * nm;

  nm    = name   + strlen(name);
  pattr = suffix + strlen(suffix);

  for(; *nm == *pattr  && nm != name && pattr != suffix; pattr --, nm --);
  if(*nm == *pattr && pattr == suffix) {
    if (drop_suffix) *nm = 0;
    return true;
  }
  return false;
}

/* Import variable name suffix */
static char imp_var_suffix[] = "@var153";

Bool check_imp_var_suffix(char * name, Bool drop_suffix) {
  return check_suffix(name, imp_var_suffix, drop_suffix);
}

/*----------------------------------------------------------------------------*/

Bool HasExtension (const char * name)
{
        const char* p = strrchr (name, '.');
        return (p && p [1] != '\\' && p [1] != '/') ? true : false;
}

/*----------------------------------------------------------------------------*/

char * MakeExtension (const char * name, const char * ext)
{
    char * nm = NULL;
    int len = 0;
    char * extpos = NULL;

    const char * point = strrchr (name, '.');

    if (point &&
        (strchr (point, '\\') == NULL) &&
        (strchr (point, '/')  == NULL))
    {
        len = point - name;
    } else {
        len = strlen (name);
    }

    nm = (char *) xalloc (len + 1 + strlen(ext) + 1);
    strncpy (nm, name, len);
    strcpy  (nm + len,     ".");
    strcpy  (nm + len + 1, ext);

    return nm;
}

/*----------------------------------------------------------------------------*/

char * ConvDLLName (char * buf)
{
    if ((xIMAGE_FORMAT == xPE_IMAGE_FORMAT) || (xIMAGE_FORMAT == xLX_IMAGE_FORMAT)) {
        char * r;
        if(!strchr(buf, '.'))
            strcat(buf, ".dll");
        for (r = buf; * r; r ++)
            * r = (char) toupper (* r);
    }
    return buf;
}

/*----------------------------------------------------------------------------*/

/* Compare str1 and str2, ignoring case */

int strcmpIC (const char * str1, const char * str2)
{
    for (;;) {
        char u1 = (char) toupper (*str1);
        char u2 = (char) toupper (*str2);

        if ((u1 == 0) || (u1 != u2))
            return u1 - u2;

        str1 ++;
        str2 ++;
    }
}

/* Convert specified string to upper case */
void str2UpperCase (char * str)
{
    char ch;
    do {
        ch = (char) toupper (*str);
        *str = ch;
    } while (ch != 0);
}


/* Counts the length of unicode string */
int wcsLen (const unichar * str)
{
    int len = 0;
    while (str [len] != 0)
        len++;

    return len;
}

close_namespace

