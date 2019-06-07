#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stddef.h>
#include <ctype.h>

// for typestable.h
#include <set>

#include "xdefs.h"

open_namespace

#include "struct.h"
#include "idents.h"
#include "messages.h"
#include "xdebug.h"
#include "xmem.h"
#include "cpb.h"
#include "parser.h"
#include "readdll.h"
#include "typestable.h"
#include "jet.h"
#include "efs.h"

/*----------------------------------------------------------------------------*/
/*                           PUBLICS                                          */
/*----------------------------------------------------------------------------*/

void TwiceDeclared (ident name, nameInfo * n, int is_warning)
{
    char buf [1024], tmp [1024];

    is_warning = is_warning && 
                 !(n -> seg && n -> seg -> file && n -> seg -> file -> lib &&
                   CurrentFile -> lib &&
                   !strcmp(CurrentFile -> lib, n-> seg -> file ->lib));

    PrintFile (tmp, CurrentFile);
    if (n -> seg && n -> seg -> file) {
        PrintFileBySeg (buf, n -> seg);
        Message (is_warning ? xWARNING : xERROR, msgNAME_WAS_TWICE_DECLARED,
                 NAMES.Index2Str (name), buf, tmp);
    } else {
        Message(is_warning ? xWARNING : xERROR, msgNAME_WAS_REDECLARED,
                NAMES.Index2Str (name), tmp);
    }
}


/*----------------------------------------------------------------------------*/

void NewPublicName (ident name, Segment * s, int offset, int type)
{
    nameInfo * info = (nameInfo *) (NAMES.getInfo (name));
    if (info != NULL) {
        if ((info -> kind != K_WEAKDEF) && (info -> kind != K_COMDEF)) {
            TwiceDeclared (name, info, CurrentFile -> lib != NULL);
            return;
        }
        // delete info;
    }
    NAMES.setInfo (name, new publicNameInfo (s, offset, type));

    if (s != NULL)
        VerboseMessage (INFO_SYMBOLDECLARATION, "SYMDECL: New public: %s SEG %s OFFS %d TYPE %d\n", NAMES.Index2Str(name), NAMES.Index2Str(s->getName()), offset, type);
    else
        VerboseMessage (INFO_SYMBOLDECLARATION, "SYMDECL: New public: %s SEG NULL OFFS %d TYPE %d\n", NAMES.Index2Str(name), offset, type);
}

/*----------------------------------------------------------------------------*/

void NewCommonName (ident name, int length)
{
    nameInfo * info = (nameInfo *) (NAMES.getInfo (name));
    if (info != NULL) {
        if ((info -> kind != K_WEAKDEF) && (info -> kind != K_COMDEF)) {
            TwiceDeclared (name, info, CurrentFile -> lib != NULL);
            return;
        }
        if (info -> kind == K_COMDEF) {
            commonNameInfo * cinfo = (commonNameInfo *)info;
            cinfo -> mergeCommon (length);
            return;
        }
        // delete info;
    }
    NAMES.setInfo (name, new commonNameInfo (length));
}

/*----------------------------------------------------------------------------*/

int NumberOfImportedNames    = 0;
int SizeOfImportedNamesTable = 1024;
ident * ImportedNames = (ident *) xalloc (SizeOfImportedNamesTable*sizeof (ident));

importNameInfo * NewImportName (ident name, int kind, ident modulename, int entry, ident version)
{
    ASSERT ((kind & K_MASK) == K_IMPORT);

    nameInfo * info = (nameInfo *) (NAMES.getInfo (name));
    if (info != NULL) {
        if (! ((importNameInfo *)info) -> equalTo (kind, modulename, entry)) {
            TwiceDeclared (name, info, true);
            return NULL;
        }
        return (importNameInfo *)info;
    }
    importNameInfo *iinfo = new importNameInfo (kind, modulename, entry, version);
    NAMES.setInfo (name, iinfo);

    if (NumberOfImportedNames == SizeOfImportedNamesTable) {
        SizeOfImportedNamesTable += SizeOfImportedNamesTable;
        ImportedNames = (ident *) xrealloc (ImportedNames, NumberOfImportedNames*sizeof(ident), SizeOfImportedNamesTable*sizeof(ident));
    }
    ASSERT (NumberOfImportedNames < SizeOfImportedNamesTable);
    ImportedNames [NumberOfImportedNames++] = name;

    if (kind & K_BY_ORDINAL)
        VerboseMessage (INFO_SYMBOLDECLARATION, "SYMDECL: New import: %s (%s | %d)\n", NAMES.Index2Str(name), NAMES.Index2Str(modulename), entry);
    else
        VerboseMessage (INFO_SYMBOLDECLARATION, "SYMDECL: New import: %s (%s | %s)\n", NAMES.Index2Str(name), NAMES.Index2Str(modulename), NAMES.Index2Str((ident)entry));

    if (version != INVALID_ID) {
        VerboseMessage (INFO_SYMBOLDECLARATION, "SYMDECL: import %s version: %s\n", NAMES.Index2Str(name), NAMES.Index2Str(version));
    }

    return iinfo;
}

/*----------------------------------------------------------------------------*/

void NewWeakName (ident name, ident defname)
{
    nameInfo * info = (nameInfo *) (NAMES.getInfo (name));
    if (info != NULL)
        return;

    info = new weakNameInfo (defname);

    nameInfo * definfo = (nameInfo *) (NAMES.getInfo (defname));
    if (definfo && definfo -> seg) {
        info -> seg    = definfo -> seg;
        info -> offset = definfo -> offset;
    }

    NAMES.setInfo (name, info);
}

/*-------------------------------------------------------------------------*/
/*                           GROUPS                                        */
/*-------------------------------------------------------------------------*/

struct group * GroupList = NULL;

/*-------------------------------------------------------------------------*/
/*                            SEGMENTS                                     */
/*-------------------------------------------------------------------------*/

Segment ** LastSeg;

//  Start new segment

Segment :: Segment (ident _name,
                    ident _grpname,
                    ident _clazz,
                    Bool  __16bit,
                    dword _length,
                    dword _alignment,
                    LocalTypesTable* localTypesTable)
{
    ASSERT (LastSeg != NULL);

    flags = 0;
    if (__16bit)
        flags |= sf_16bit;

    name        = _name;
    grpname     = _grpname;
    keyname     = INVALID_ID;
    file        = CurrentFile;
    length      = _length;
    clazz       = _clazz;
    overlay     = 0;
    address     = (dword) -1;
    if (xSparse)
      alignment = 4096;
    else
      alignment = _alignment;
#if defined (OBSOLETE)
    combination = 0;
    attributes  = 0;
#endif

    fixups     = NULL;
    nfixups    = 0;
    fixupssize = 0;

    lines     = NULL;
    nlines    = 0;
    linessize = 0;

    this->localTypesTable = localTypesTable;

    if ((name  != SYMBOLS_OMF) && (name  != SYMBOLS_COFF) &&
        (name  != TYPES_OMF)   && (name  != TYPES_COFF)   &&
        (clazz != NULLCHECKS)  && (clazz != DEBUG)        &&
        (clazz != STACKTRACE))
    {
        flags |= sf_linkable;
    }

    if (clazz != BSS) {
        text = (byte *) xalloc (length);
        memset (text, 0, length);
    } else {
        text = NULL;
    }
    next = NULL;
    link = NULL;

    * LastSeg = this;
    LastSeg = & (this -> next);
}


void Segment :: allocateFixups (int nfixups)
{
    ASSERT (fixupssize == 0);
    fixupssize = nfixups;
    fixups     = (struct fixup *) xalloc (nfixups * sizeof (struct fixup));
}


byte* Segment :: getText () {
    return text;
}


void Segment :: setText (byte* text, int len) {
    ASSERT (this->text == NULL);
    ASSERT (this->length == 0);
    this->text = text;
    this->length = len;
}


void Segment :: copyTextTo (byte* dest) {
    ASSERT (this->text != NULL);
    memcpy (dest, this->text, this->length);
}

void Segment :: freeText() {
    xfree (text);
    text = NULL;
}


ident Segment :: getKeyName ()
{
    if (keyname == INVALID_ID) {
        Bool duplicateName = false;
        ident myName = getName ();
        Segment * s = this -> file -> segs;
        for (;s ;s = s->next) {
            if ((s != this) && (s -> getName () == myName)) {
                duplicateName = true;
                break;
            }
        }
        if (duplicateName) {
            char keyName [1024 + 32];
            ASSERT (NAMES.Index2StrLen (myName) < 1024);
            // assign key names to all segments who's name == myName
            int index = 0;
            for (Segment * s = this -> file -> segs; s ; s = s->next) {
                if (s -> getName () == myName) {
                    sprintf (keyName, "%s^%d", NAMES.Index2Str (myName), index);
                    s -> keyname = NAMES.Str2Index (keyName);
                    index++;
                }
            }
            ASSERT (keyname != INVALID_ID);
        } else
            keyname = getName ();
    }
    return keyname;
}


byte* CustomDataSourceSegment :: getText () {
    // unsupported operation
    ASSERT_FALSE();
}

void CustomDataSourceSegment :: setText (byte* text, int len) {
    // unsupported operation
    ASSERT_FALSE();
}

void CustomDataSourceSegment :: copyTextTo (byte* dest) {
    ASSERT (dataSource != NULL);
    ASSERT (dataSource->getLen() == this->getLen());
    dataSource->copyDataTo(dest);
}

void CustomDataSourceSegment :: freeText() {
    delete dataSource;
    dataSource = NULL;
}


/*-------------------------------------------------------------------------*/
/*                              FIXUPS                                     */
/*-------------------------------------------------------------------------*/

void addFixup (Segment * seg, byte kind, dword offset, byte k_target, void * target, int fx_offset)
{
    struct fixup * f = NULL;
    Grow (seg -> nfixups, seg -> fixupssize, 16, seg -> fixups, struct fixup, f);

    ASSERT ((kind == FIXUP_ADDRESS32)     ||
            (kind == FIXUP_SELFRELOFFS32) ||
            (kind == FIXUP_OFFSET32NB)    ||
            (kind == FIXUP_FAR16_16)      ||
            (kind == FIXUP_FAR16_32)      ||
            (kind == FIXUP_TDINDEX16)     ||
            (kind == FIXUP_TDINDEX32)     ||
            (kind == FIXUP_JAVASTRING32)  ||
            (kind == FIXUP_BYTESTRING32)  ||
            (kind == FIXUP_CONSTADDRESS32));

    f -> kind      = kind;

    ASSERT ((k_target == TK_SEG)      ||
            (k_target == TK_GROUP)    ||
            (k_target == TK_ID)       ||
            (k_target == TK_FWD_SEG)  ||
            (k_target == TK_TARGET32));

    f -> k_target  = k_target;

    f -> target    = target;
    f -> offset    = offset;
    f -> fx_offset = fx_offset;

    if (k_target == TK_SEG) {
        ASSERT (target != NULL);
    }
}

struct fixupFAR16_xx * FAR16_xxFixups      = NULL;
struct fixupFAR16_xx * FAR16_xxFixups_Tail = NULL;

static void new_FAR16_xxFixup (dword source, dword target, byte kind) {
    if (FAR16_xxFixups) {
        FAR16_xxFixups_Tail -> next = (struct fixupFAR16_xx *) xalloc(sizeof(struct fixupFAR16_xx));
        FAR16_xxFixups_Tail = FAR16_xxFixups_Tail -> next;
    } else {
        FAR16_xxFixups_Tail = FAR16_xxFixups = (struct fixupFAR16_xx *) xalloc(sizeof(struct fixupFAR16_xx));
    }

    struct fixupFAR16_xx * far_fixup = FAR16_xxFixups_Tail;

    far_fixup -> next   = NULL;
    far_fixup -> source = source;
    far_fixup -> target = target;
    far_fixup -> kind   = kind;
}

/*-------------------------------------------------------------------------*/

struct impVarFixup * ImpVarFixups = NULL;

/*-------------------------------------------------------------------------*/

int NFixups = 0;

dword * Fix;
dword * FixTrg;
dword * FixRTrg;
dword   NFix = 0;

/*-------------------------------------------------------------------------*/

static void setFixupTarget (struct fixup * f, byte k_target, void * target) {
    f -> k_target = k_target;
    f -> target   = target;
}

static void Undefined (ident name, Segment * seg);

// resolveFixupTarget (s, f, mark)
// ASSERT ((f->k_target == TK_SEG) || (f->k_target == TK_TARGET32) || (f->k_target == TK_IMPORT_ID));

static void resolveFixupTarget (Segment * seg, struct fixup * f, int mark) {
    switch (f -> k_target) {
        case TK_SEG:
        case TK_TARGET32:
            return;

        case TK_FWD_SEG:
            {
                ident target_seg_name = (ident) (f -> target);
                ASSERT (seg != NULL);
                for (Segment * s = seg -> next; s != NULL; s = s -> next) {
                    if (s -> getName () == target_seg_name) {
                        setFixupTarget (f, TK_SEG, s);
                        return;
                    }
                }
                Message(xFATAL, msgUNRESOLVED_SEG, seg -> file -> getFilename(), NAMES.Index2Str (target_seg_name));
                return;
            }

        case TK_ID:
            {
                ident name = (ident) (f -> target);
                for (;;) {
                    nameInfo * p = (nameInfo *) (NAMES.getInfo (name));
                    if (p != NULL) {
                        p -> kind |= mark;
                        Segment * s = p -> seg;
                        if (s != NULL) {
                            setFixupTarget (f, TK_SEG, s);
                            f -> fx_offset += p -> offset;
                            return;
                        } else if ((p -> kind & K_MASK) == K_COMDEF) {
                            ((commonNameInfo *) p) -> createSegment ();
                            s = p -> seg;
                            ASSERT (s != NULL);
                            setFixupTarget (f, TK_SEG, s);
                            return;
                        } else if ((p -> kind & K_MASK) == K_WEAKDEF) {
                            name = ((weakNameInfo *) p) -> getForwardName ();
                            continue;
                        } else if ((p -> kind & K_MASK) == K_IMPORT) {
                            setFixupTarget (f, TK_IMPORT_ID, p);
                            return;
                        } else if ((p -> kind & K_MASK) == K_JIMPORT) {
                            setFixupTarget (f, TK_JIMPORT_ID, p);
                            return;
                        } else
                            ASSERT_FALSE ();
                    } else {
                        Undefined (name, seg);
                        setFixupTarget (f, TK_INVALID, NULL);
                        return;
                    }
                }
            }

        default:
            ASSERT (seg != NULL);
            Message (xFATAL, msgINVALID_FIXUP_TARGET, seg -> file -> getFilename (), f -> offset);
            return;
    }
}

/*-------------------------------------------------------------------------*/

struct fixup * EntryPoint = NULL;

void setEntryPoint (byte   kind,
                    byte   k_target,
                    void * target,
                    int    fx_offset,
                    const char * fileName)
{
    ASSERT (EntryPoint == NULL);

    EntryPoint = (struct fixup *) xalloc (sizeof (struct fixup));

    // only FAR16:32 entry point fixup kind supported
    ASSERT (kind == FIXUP_FAR16_32);  

    EntryPoint -> kind = kind;

    ASSERT ((k_target == TK_SEG)   ||
            (k_target == TK_GROUP) ||
            (k_target == TK_ID));

    EntryPoint -> k_target  = k_target;
    EntryPoint -> target    = target;
    EntryPoint -> offset    = 0;
    EntryPoint -> fx_offset = fx_offset;

    xWasEntryPoint  = true;
    xEntryPointFile = fileName;
}

/*-------------------------------------------------------------------------*/
/*                              FILES                                      */
/*-------------------------------------------------------------------------*/

StringTable * FileNames = new StringTable ();

OBJFile * FileList    = NULL;
OBJFile * CurrentFile = NULL;

static OBJFile * LastFile    = NULL;
static OBJFile * CommonsFile = NULL;


OBJFile :: OBJFile (const char * _filename, const char * _libname)
{
    filename   = _filename ? dup (_filename, strlen (_filename)) : NULL;
    filenameID = FileNames->Str2Index (filename);
    uidName    = filenameID;

    lib      = _libname ? dup (_libname, strlen(_libname)) : NULL;
    source   = NULL;
    segs     = NULL;
    nsegs    = 0;
    next     = NULL;
    nextrnls = 0;
    extrnls  = NULL;
    types    = NULL;
    symbols  = NULL;
    linnums  = NULL;
    publics  = NULL;
    processed= false;
    smart    = xSmart;
    idata_processed = false;
    dbgcomment = NULL;

    common     = false;
    commonData = NULL;
    commonLen  = 0;

    if (FileList) {
        ASSERT (LastFile != NULL);
        LastFile -> next = this;
    } else {
        FileList = this;
    }
    LastFile = this;

    LastSeg = & (this -> segs);

    CurrentFile = this;
}

void SetFile (char * filename, char * lib)
{
    OBJFile * r;

    if (CurrentFile && !strcmp(CurrentFile -> getFilename (), filename) && !strcmp(CurrentFile -> lib, lib))
        return;

    for(r = FileList; r; r = r -> next) {
        if (!strcmp(r -> getFilename (), filename) && !strcmp(r -> lib, lib))
            break;
    }
    if (!r) {
        new OBJFile (filename, lib);
    } else {
        if (r -> segs) {
            Segment * s = r -> segs;
            for(; s -> next; s = s -> next)
                ;
            LastSeg = &(s -> next);
        } else
            LastSeg = &(r -> segs);

        CurrentFile = r;
    }
}
/*-------------------------------------------------------------------------*/
/*                               DLLs                                      */
/*-------------------------------------------------------------------------*/

int Ndlls    = 0;
int dllsSize = 0;
struct dll * dlls = NULL;

struct dll * findDLL (ident dllname) {
    for (int i = 0; i < Ndlls; i++)
        if (dlls [i].name == dllname)
            return & (dlls [i]);

    return NULL;
}

struct dll * getDLL (ident dllname) {
    struct dll * p = findDLL (dllname);

    if (p == NULL) {
        Grow (Ndlls, dllsSize, 32, dlls, struct dll, p);
        p -> name     = dllname;
        p -> nentries = 0;
        p -> nameID   = 0;
    }
    return p;
}

/*-------------------------------------------------------------------------*/
/*                        Undefined symbols                                */
/*-------------------------------------------------------------------------*/

struct undef {
    ident           name;
    struct undef *  next;
};

struct undef * Undefs = NULL;

static Bool WasUndef (ident name)
{
    struct undef * u;

    for (u = Undefs; u; u = u -> next)
        if (u -> name == name)
            return true;

    u = (struct undef *) allocateForever (sizeof (struct undef));
    u -> name = name;
    u -> next = Undefs;
    Undefs = u;
    return false;
}

static void Undefined (ident name, Segment * seg)
{
    if (WasUndef (name))
        return;

    if (seg) {
        char tmp [1024];
        PrintFileBySeg (tmp, seg);
        Message(xERROR, msgREFERENCED_NAME_NOT_FOUND, NAMES.Index2Str (name), tmp);
    } else
        Message(xERROR, msgNAME_NOT_FOUND, NAMES.Index2Str (name));
}

static void Undefined (Export * exp)
{
    if (WasUndef (exp -> intname))
        return;

    Message(xERROR, msgEXPORTED_NAME_NOT_FOUND, NAMES.Index2Str (exp -> intname), NAMES.Index2Str (exp -> extname));
}

/*-------------------------------------------------------------------------*/

char  * Description = NULL;

/*-------------------------------------------------------------------------*/

void PrintFile (char * buf, OBJFile * s)
{
        if ((s == NULL) || (s -> getFilename () == NULL))
                sprintf (buf, "unknown file");
        else {
                buf += sprintf (buf, "file %s",
                                (s -> getSource() && strcmp (s->getSource(), s->getFilename ())) ?
                                s -> getSource() : s -> getFilename ());
                if (s -> lib)
                        sprintf (buf, " (library %s)", s -> lib);
        }
}

/*----------------------------------------------------------------------------*/

void PrintFileBySeg (char * buf, Segment * s)
{
        if (! s)
                sprintf (buf, "unknown file");
        else
                PrintFile (buf, s -> file);
}

/*----------------------------------------------------------------------------*/

int Alignment (int size)
{
        switch (size) {
                case 0:
                case 1:         return 1;
                case 2:         return 2;
                case 3:
                case 4:         return 4;
                case 5:
                case 6:
                case 7:
                case 8:         return 8;
                default:        return 16;
        }
}

/*----------------------------------------------------------------------------*/


//  Попал ли символ, на который ссылается fixup, в EXE-файл?
//  Если да, то выдать еще и его адрес.

Bool SymbolInExe (struct fixup * f, dword * address)
{
        nameInfo    * p;
        Segment  * s;
        ident         target_id;
        dword         target_addr;

        switch (f -> k_target) {
        case TK_SEG:
                s = (Segment *) f -> target;
                if (s == NULL || ! (s -> isProcessed ()))
                        return false;
                target_addr = s -> address;
                break;
        case TK_ID:
                target_id = (ident) (f -> target);
                p = (nameInfo *) (NAMES.getInfo(target_id));
                if (p -> seg == 0 && (p -> kind & K_MASK) == K_WEAKDEF)
                    p = (nameInfo *) (NAMES.getInfo(((weakNameInfo *)p) -> getForwardName()));
                s = p -> seg;
                if (s == NULL || ! (s -> isProcessed ()))
                        return false;
                target_addr = p -> offset;
                break;
        default:
                return false;
        }
        * address = target_addr + f -> fx_offset;
        return true;
}

/*----------------------------------------------------------------------------*/
/*                           EXPORTS                                          */
/*----------------------------------------------------------------------------*/

int EXPORT_SOURCE_MASK = EMASK_SOURCE;

Export * Exports = NULL;
int NExports = 0;

Export ** ExportsTable = NULL;
int NumberOfExports = 0;
int MaxOrdinal      = 0;

Export ** ILExportsTable;
int ILNumberOfExports = 0;

static Export * ExportsTree = NULL;

static Bool InsertExportIntoAVLTree(Export *e, Export **tree)
{
    Export *p1, *p2;

    if( *tree == NULL ) {
          *tree    = e;
          e -> l   = NULL;
          e -> r   = NULL;
          e -> bal = 0;

          e -> next = Exports;
          Exports = e;
          NExports++;

          return true;
    }
    if( (*tree)->extname > e->extname ) {
          if ( InsertExportIntoAVLTree(e, &((*tree)->l)) ) {
               switch( (*tree)->bal ) {
                  case    1: (*tree)->bal =  0;  return false;
                  case    0: (*tree)->bal = -1;  return true;
                  case (-1): 
                      p1 = (*tree)->l;
                      if( p1->bal == -1) {
                           (*tree)->l = p1->r;
                           p1     ->r = (*tree);
                           (*tree)->bal = 0;
                           (*tree)    = p1;
                      } else {
                           p2    = p1->r;
                           p1->r = p2->l;
                           p2->l = p1;
                           (*tree)->l = p2->r;
                           p2->r  = (*tree);
                           switch (p2->bal) {
                               case +1: p1->bal=-1; (*tree)->bal= 0; break;
                               case -1: p1->bal= 0; (*tree)->bal=+1; break;
                               case  0: p1->bal= 0; (*tree)->bal= 0; break;
                           }
                           (*tree) = p2;
                      }
                      (*tree)->bal = 0;
                      return false;
               }
          } else
               return false;
    } else if ((*tree)->extname < e->extname) {
          if ( InsertExportIntoAVLTree(e, &((*tree)->r)) ) {
               switch( (*tree)->bal ) {
                  case -1: (*tree)->bal =  0;  return false;
                  case  0: (*tree)->bal = +1;  return true;
                  case +1: 
                      p1 = (*tree)->r;
                      if( p1->bal == +1) {
                           (*tree)->r = p1->l;
                           p1     ->l = (*tree);
                           (*tree)->bal = 0;
                           (*tree)    = p1;
                      } else {
                           p2    = p1->l;
                           p1->l = p2->r;
                           p2->r = p1;
                           (*tree)->r = p2->l;
                           p2->l  = (*tree);
                           switch (p2->bal) {
                              case +1: p1->bal= 0; (*tree)->bal=-1; break;
                              case -1: p1->bal=+1; (*tree)->bal= 0; break;
                              case  0: p1->bal= 0; (*tree)->bal= 0; break;
                           }
                           (*tree) = p2;
                      }
                      (*tree)->bal = 0;
                      return false;
               }
          } else
               return false;
    } else {
       ASSERT_FALSE();
    }
    return 0;
}

Export * NewExport (ident extname, ident intname, Segment *seg, dword offset, word ordinal, short exp_def_src, ident modname)
{
    VerboseMessage (INFO_SYMBOLDECLARATION, "SYMDECL: New export: %s=%s, src:%d\n", NAMES.Index2Str(extname), NAMES.Index2Str(intname), exp_def_src);

    Export * exp;

    ASSERT(!(exp_def_src & ~EMASK_SOURCE));
    exp = FindExportByExtName(extname);
    if (exp == NULL) {
       exp = (Export *) allocateForever (sizeof(Export));
       exp -> extname = extname;
       exp -> intname = intname;
       exp -> seg     = seg;
       exp -> offset  = offset;
       exp -> ordinal = ordinal;
       if (xNoExportNames)
          exp -> flag = exp_def_src;
       else
          exp -> flag = exp_def_src | EFLAG_NAME_EXPORTED;
       exp -> modname = modname;
       InsertExportIntoAVLTree(exp, &ExportsTree);
    } else {
       if (exp->flag & exp_def_src) {
          Message(xERROR, msgDUP_DEF_FOR_EXPORT, NAMES.Index2Str(extname));
       } else if ((exp->flag & EMASK_SOURCE) < exp_def_src) {
          if (intname != INVALID_ID) exp -> intname = intname;
          if (seg)                   exp -> seg     = seg;
          if (offset)                exp -> offset  = offset;
          if (ordinal)               exp -> ordinal = ordinal;
          if (modname != INVALID_ID) exp -> modname = modname;
          Message(xWARNING, msgDUP_DEF_FOR_EXPORT, NAMES.Index2Str(extname));
       } else
          exp -> flag |= exp_def_src;
    }
    return exp;
}

static Export * FindExportInAVLTree (ident extname, Export *tree)
{
   if (tree == NULL)
         return NULL;
   if (tree->extname == extname)
         return tree;
   if (tree->extname > extname)
         return FindExportInAVLTree(extname, tree->l);
   else
         return FindExportInAVLTree(extname, tree->r);
}

Export * FindExportByExtName (ident extname)
{
   return FindExportInAVLTree(extname, ExportsTree);
}

/*-------------------------------------------------------------------------*/
/*                            PROCESSING                                   */
/*-------------------------------------------------------------------------*/

static int XCDECL compare_ordinals (const void * p1, const void * p2)
{
    Export *e1, *e2;
    e1 = *((Export **)p1);
    e2 = *((Export **)p2);
    if ( e1 -> ordinal > e2 -> ordinal)
        return 1;
    if ( e1 -> ordinal < e2 -> ordinal)
        return -1;
    return 0;
}

static int XCDECL compare_names (const void * p1, const void * p2)
{
    Export *e1 = * (Export **) p1;
    Export *e2 = * (Export **) p2;
    return strcmp (NAMES.Index2Str (e1 -> extname), NAMES.Index2Str (e2 -> extname));
}

/*------------------------------------------------------------------------*/
/*         Calculates all ordinals, NumberOfExports, MaxOrdinal,          */
/*         Creates ExportsTable (sorted by ordinal)                       */
/*------------------------------------------------------------------------*/

void CalculateOrdinals(void)
{
    int n;

    NumberOfExports = 0;
    ILNumberOfExports = 0;
    Export * exp;
    for (exp = Exports; exp; exp = exp -> next)
        if (exp->flag & EXPORT_SOURCE_MASK) {
            NumberOfExports++;

            if (! (exp->flag & EFLAG_NOIMPLIB) )
                ILNumberOfExports++;
        }
    ExportsTable   = (Export **) xalloc (  NumberOfExports * sizeof (Export *));
    ILExportsTable = (Export **) xalloc (ILNumberOfExports * sizeof (Export *));
    Export ** e_table  =   ExportsTable;
    Export ** il_table = ILExportsTable;
    for (exp = Exports; exp; exp = exp -> next)
        if (exp->flag & EXPORT_SOURCE_MASK) {
            *e_table++ = exp;

            if (! (exp->flag & EFLAG_NOIMPLIB) )
                *il_table++ = exp;
        }
    qsort (ExportsTable, NumberOfExports, sizeof (Export *), compare_ordinals);

    for (n = 0; n < NumberOfExports && ExportsTable[n] -> ordinal == 0; n++);

    /* Check duplicating exports */
    for (int i = n; i+1 < NumberOfExports; i++)
        if (ExportsTable[i]->ordinal == ExportsTable[i+1]->ordinal)
            Message(xERROR, msgDUP_ORDINALS, NAMES.Index2Str(ExportsTable[i]->extname), NAMES.Index2Str(ExportsTable[i+1]->extname));

    if (n == NumberOfExports) {
        int curr_ord = 1;
        for(int i = 0; i < NumberOfExports; i++)
            ExportsTable[i] -> ordinal = (word)curr_ord++;
        MaxOrdinal = curr_ord - 1;
    } else {
        int l = n;
        int curr_ord = 1;
        for(int i = 0; i < n; i++) {
            while(l < NumberOfExports && curr_ord == ExportsTable[l] -> ordinal) l++, curr_ord++;
            ExportsTable[i] -> ordinal = (word)curr_ord++;
        }
        MaxOrdinal = curr_ord - 1;
        if (ExportsTable[NumberOfExports-1]->ordinal > MaxOrdinal)
            MaxOrdinal = ExportsTable[NumberOfExports-1]->ordinal;
    }
    if (MaxOrdinal > 65535) 
        Message(xFATAL, msgTOO_MANY_EXPORTED, MaxOrdinal);

    qsort (  ExportsTable,   NumberOfExports, sizeof (Export *), compare_ordinals);
    qsort (ILExportsTable, ILNumberOfExports, sizeof (Export *), compare_names);
}


void PrepareExport (void)
{
    Export * exp;
    for(exp = Exports; exp; exp = exp->next) {
        char *exp_var_name = dup(NAMES.Index2Str(exp->extname), NAMES.Index2StrLen(exp->extname));
        if(check_imp_var_suffix(exp_var_name)) {
            exp -> flag |= EFLAG_VAR153;
            Export * exp_var = FindExportByExtName(NAMES.Str2Index(exp_var_name, strlen(exp_var_name)));
            if (exp_var != NULL)
                exp_var -> flag |= EFLAG_VARIABLE;
        }
        xfree (exp_var_name);

        if (! (exp -> seg)) {
            nameInfo * info = (nameInfo *) (NAMES.getInfo(exp -> intname));
            if (!info) {
                Undefined (exp);
                continue;
            }
            if (info -> kind & K_PUBDEF) {
                int type = kind2type (info -> kind);
                if (type & T_DATA)
                    exp -> flag |= EFLAG_VARIABLE;
            }
        }
    }
    if (TotalErrors != 0)
        return;
    CalculateOrdinals();
}


void PrepareSections (void);
void OrderSegments   (void);
void CalcAddresses   (void);
void CreateImages    (void);

void CollectFixup (struct fixup * f, Segment * seg, Bool CountFixups = true);
void CollectName     (ident name);
void CollectSegments (Segment *);
void CollectAll    (void);

void PrepareJavaStrings  (void);
void PrepareByteStrings  (void);
void PrepareComponentKey (void);
void PrepareNullCheckInformation (void);
void PrepareStackTraceInformation (void);
void StoreImageBase (void);
void FormCPB (void);


void InitIR () {
    VerStamp = new VersionStamp();
}

void ProcessIR(void)
{
    if ((xIMAGE_FORMAT == xELF_IMAGE_FORMAT) && xDLLFlag) {
        xImageBase = 0;
    }
    if (xAutoImageBase && (xAutoImageBasePrevComponent != NULL) &&
        (xIMAGE_FORMAT == xPE_IMAGE_FORMAT))
    {
        ReadDLLForAutoImageBase(xAutoImageBasePrevComponent);
    }

    PrepareTypesTable ();
    PrepareJavaStrings ();
    PrepareByteStrings ();
    PrepareComponentKey ();
    FormEmbeddedFS ();
    PrepareNullCheckInformation ();
    PrepareStackTraceInformation ();
    StoreImageBase();
    FormCPB();
    generateExtraThunks();

    PrepareExport ();
    PrepareSections ();
}

void PrepareSections(void)
{
    if (xWasEntryPoint)
        CollectFixup (EntryPoint, NULL, false /* do not count this fixup */);
    for (Export *r = Exports; r; r = r -> next)
        if (r -> flag & EXPORT_SOURCE_MASK)
            if (r -> seg)
                CollectSegments (r -> seg);
            else
                CollectName (r -> intname);
    if (!xSmart)
        CollectAll ();
    if (TotalErrors == 0) {
        OrderSegments ();
        CalcAddresses ();
        CreateImages ();
    }
}

/*-------------------------------------------------------------------------*/

//  For COMMON-block create BSS segment and link it to CommonsFile

void commonNameInfo::createSegment ()
{
    if (seg != NULL)
        return;

    if (CommonsFile == NULL) {
        new OBJFile ("COMMON BLOCKS");
        CommonsFile = CurrentFile;
    }

    seg = new Segment (BSS, BSS, BSS, false, length, Alignment (length));

#if defined (OBSOLETE)
    seg -> combination = 2;
    seg -> attributes  = 2 << 2;
#endif
    seg -> markAsProcessed ();
}

/*-------------------------------------------------------------------------*/

//  Эта группа процедур собственно и осуществляет smart linking -
//  начиная или с имени, или с fixup-а, отмечаем как нужные все, на
//  кого ссылается или сам fixup, или все fixup-ы в сегменте.

void CollectFixup (struct fixup * f, Segment * seg, Bool CountFixups)
{
    if ((f -> kind == FIXUP_TDINDEX16) || (f -> kind == FIXUP_TDINDEX32)) {
        ASSERT (f -> k_target == TK_ID);
        nameInfo * p = (nameInfo *) (NAMES.getInfo ((ident) f->target));
        if (p) {
            p -> kind |= K_USED;
            Segment * s = p -> seg;
            if (s && !(s -> isProcessed ()))
                CollectSegments (s);
        }
        return;

    }
    if (f -> kind == FIXUP_JAVASTRING32) {
        ASSERT (f -> k_target == TK_TARGET32);
        return;
    }

    resolveFixupTarget (seg, f, K_USED);

    switch (f -> k_target) {
        case TK_SEG:
            {
                Segment * s = (Segment *) (f -> target);
                if (! (s -> isProcessed ()))
                    CollectSegments (s);
            }
            break;

        case TK_IMPORT_ID:
        case TK_JIMPORT_ID:
            break;

        case TK_INVALID:
            return;

        case TK_TARGET32:
            return;

        default:
            ASSERT_FALSE ();
    }

    switch (f -> kind) {
        case FIXUP_SELFRELOFFS32:
        case FIXUP_OFFSET32NB:
        case FIXUP_FAR16_32:
        case FIXUP_FAR16_16:
        case FIXUP_CONSTADDRESS32:
           break;

        case FIXUP_ADDRESS32:
            if (((xIMAGE_FORMAT != xLX_IMAGE_FORMAT) && ((f -> k_target == TK_IMPORT_ID) || (f -> k_target == TK_JIMPORT_ID))) || (f -> k_target == TK_SEG))
                if (CountFixups)
                    NFixups ++;
            break;

        default:
            VerboseMessage ("Invalid fixup kind: %d\n", f->kind);
            Message (xFATAL, msgINVALID_FIXUP_FOR_FLAT_MOD);
            break;
    }
}

void CollectName (ident name)
{

    nameInfo * p = (nameInfo *) (NAMES.getInfo (name));
    if (p) {
        p -> kind |= K_USED;
        Segment * s = p -> seg;
        if (s) {
            if (! (s -> isProcessed ()))
                CollectSegments (s);
        }
        else if ((p -> kind & K_MASK) == K_COMDEF)
            ((commonNameInfo *) p) -> createSegment ();
    } else
        Undefined (name, NULL);
}

static int IsIDATA_DOLLAR (const char *name, char n) {
    return !strncmp (name, ".idata$", 7) && (name[8] == n);
}

void CollectSegments (Segment * s)
{
    s -> markAsProcessed ();

    int i;
    struct fixup * f;
    for (i = s -> nfixups, f = s -> fixups; i; f ++, i --)
        CollectFixup (f, s);

//  A special hack to handle COFF import definitions properly -
//  do not perform smart linking.

    const char * name = NAMES.Index2Str (s -> getName());

    Segment * p;
    if (IsIDATA (name) && !s->file->idata_processed) {
        s->file->idata_processed = true;
        for(p = s -> file -> segs; p; p = p -> next) {
            if(IsIDATA_DOLLAR(NAMES.Index2Str (p -> getName()),'2') && !(p -> isProcessed ())) {
                CollectSegments(p);
                break;
            }
        }
        CollectName (* s -> file -> extrnls);
   }

   if (IsIDATA_DOLLAR(name,'5')) {
        p = s->next;
        ASSERT(p);
        name = NAMES.Index2Str(p->getName());
        ASSERT(IsIDATA_DOLLAR(name,'4'));
        CollectSegments(p);
    }
}

/*-------------------------------------------------------------------------*/

//  Collect segments for files without smart linking

static Bool CollectAllSegments (OBJFile *f)
{
    Bool changed = false;
    for (Segment * p = f -> segs; p; p = p -> next)
        if (p -> isLinkable () && !p -> isProcessed ())
        {
            changed = true;
            CollectSegments (p);
        }
    f -> markAsProcessed ();
    return changed;
}

void CollectAll (void)
{
    Bool changed;
    do {
        changed = false;
        for (OBJFile * f = FileList; f; f = f -> next)
            if (! f -> smart && ! f -> isProcessed ()) {
                if (f -> lib) {
                    for (Segment * s = f -> segs; s; s = s -> next)
                        if (s -> isProcessed ()) {
                            changed = changed || CollectAllSegments (f);
                            break;
                        }
                } else
                    changed = changed || CollectAllSegments (f);
            }
    } while (changed);
}

/*-------------------------------------------------------------------------*/
/*                           COLLECTIONS                                   */
/*-------------------------------------------------------------------------*/

struct collection * code, * code16, * bss, * data, * idata, * rdata;

#if defined (STACK_SEG)
struct collection * stack;
#endif

dword CodeStart, Code16Start, DataStart, BSSStart, IdataStart, RDataStart, JImportStart, JExportStart, CPBStart;
dword CodeLen,   Code16Len,   DataLen,   BSSLen,   IdataLen,   RDataLen, JImportLen, JExportLen;
byte   * CodePtr, * Code16Ptr, * DataPtr, * IdataPtr, * RDataPtr;

dword ImportStart;

/*-------------------------------------------------------------------------*/

// Insert a segment into the ordered list

static void InsertSegment (Segment ** p, Segment * s)
{
    OBJFile * file = s -> file;

    Segment * q;
    for (q = * p; q; p = & (q -> link), q = q -> link)
        if (file -> equalFilename (q -> file))
            for (;;) {
                if (q -> nfixups < s -> nfixups) {
                    s -> link = * p;
                    * p = s;
                    return;
                }
                p = & (q -> link);
                q = q -> link;
                if (q == NULL || !file -> equalFilename (q -> file)) {
                    s -> link = * p;
                    * p = s;
                    return;
                }
            }
    s -> link = * p;
    * p = s;
}

/*----------------------------------------------------------------------------*/

//  Add a segment to a collection

static void AddCollection (struct collection ** p, Segment * s, Bool sort)
{
    s -> markAsCollected ();

    ident  groupName = s -> getGroupName();
    struct collection * q;

    const char * name = NAMES.Index2Str (groupName);
    for (q = * p; q; p = & (q -> next), q = q -> next)
        if (q -> name == groupName) {
            if (sort)
                InsertSegment (& (q -> segs), s);
            else {
                q -> last -> link = s;
                q -> last = s;
            }
            s -> collection = q;
            return;
        }
        else if (sort && strcmp (name, NAMES.Index2Str (q -> name)) < 0)
            break;

    q = (struct collection *) allocateForever (sizeof (struct collection));
    q -> name = groupName;
    q -> segs = q -> last = s;
    q -> next = * p;
    * p = q;
    s -> collection = q;
}

void OrderSegments (void)
{
    if (xReadLinkInfo) {
        ReadLinkInfo ();
    }

    for (OBJFile * f = FileList; f; f = f -> next)
        for (Segment * s = f -> segs; s; s = s -> next)
            if (s -> isProcessed () && ! s -> isCollected ()) {
                if (s -> clazz == CODE) {
                    if (s -> is16bit())
                        AddCollection (& code16, s, false);
                    else
                        AddCollection (& code, s, false);
                } else if (s -> clazz == BSS) {
                    AddCollection (& bss, s, false);
#if defined (STACK_SEG)
                } else if (s -> clazz == STACK) {
                    AddCollection (& stack, s, false);
#endif
                } else if (s -> clazz == IDATA) {
                    AddCollection (& idata, s, true);
                } else if (s -> clazz == CONST) {
                    AddCollection (& rdata, s, true);
                } else {
                    AddCollection (& data, s, false);
                }
            }
}

/*-------------------------------------------------------------------------*/

VASAllocator * VASA = NULL;

static dword CalcAddress (struct collection * p, dword addr)
{
    for (; p; p = p->next) {
        p -> base_vadr = addr;

        VerboseMessage (INFO_VIRTUALADDRESSES, "[VIRTADR] ADR:%XH: Image %s\n", addr, NAMES.Index2Str(p->name));

        for (Segment * s = p -> segs; s; s = s -> link) {
            s -> address = (addr + s -> alignment - 1) & ~ (s -> alignment - 1);
            addr = s -> address + s -> getLen();
            VerboseMessage (INFO_VIRTUALADDRESSES, "[VIRTADR] ADR:%XH FILE:%s SIZE:%d SEGMENT %s\n",
                                                   s->address, (s->file ? s->file->getFilename () : "NONE"), s->getLen(), NAMES.Index2Str(s->name));
        }
    }
    return addr;
}

void SetOffset (ident, void *info) {
    nameInfo * n = (nameInfo *)info;
    if (n && n -> seg && n -> seg -> address != -1)
        n -> offset += n -> seg -> address;
}

void CalcAddresses (void)
{
    VASA = new VASAllocator (xImageBase + xObjectOffset);

// Addresses of CODE (CodeStart..ImportStart)
    CodeStart = VASA->getVirtualAddr();
    ImportStart = (CalcAddress (code,  CodeStart) + 2 - 1) & ~ (2 - 1);

// Addresses of Jumps to DLLs (ImportStart..CodeStart+CodeLen)
    struct dll  * p;
    dword addr = ImportStart;
    for (int i = 0; i < NumberOfImportedNames; i++) {
        importNameInfo * info = (importNameInfo *) (NAMES.getInfo (ImportedNames [i]));
        if ((info -> kind & (K_USED | K_MASK)) == K_USED + K_IMPORT) {
            p = getDLL (info -> getModuleName());
            p -> nentries ++;
            if ((info -> kind & K_IMPFUNC) && (xIMAGE_FORMAT == xPE_IMAGE_FORMAT)) {
                info -> offset = addr;
                addr += 6;
                NFixups ++;
            }
            if ((info -> kind & K_IMPFUNC) && (xIMAGE_FORMAT == xELF_IMAGE_FORMAT)) {
                info -> offset = addr;
                addr += 5;
            }
        }
    }
    addr = (addr + 2 - 1) & ~ (2 - 1); // align by 2

    for (JImportDLL * d = JImportDLLs; d; d = d -> next) {
        for (JImportGroup * g = d -> groups; g; g = g -> next) {
            for (int k = 0; k < g -> entriesfilled; k++) {
                jimportNameInfo * jinfo = (jimportNameInfo *) (NAMES.getInfo (g -> itemnames[k]));
                ASSERT (jinfo && ((jinfo -> kind & K_MASK) == K_JIMPORT));
                if (jinfo -> kind & K_USED) {
                    d -> used = true;
                    jinfo -> group -> usedEntries ++;
                    if ((jinfo -> kind & K_IMPFUNC) &&
                        ((xIMAGE_FORMAT == xPE_IMAGE_FORMAT) || (xIMAGE_FORMAT == xELF_IMAGE_FORMAT)))
                    {
                        jinfo -> offset = addr;
                        addr += 6;
                        NFixups ++;
                    }
                }
            }
        }
    }

    CodeLen = addr - CodeStart;
    VASA->allocateVirtualSpace (CodeStart, CodeLen);

// Addresses of CODE16 (Code16Start..Code16Start+Code16Len)

    Code16Start = VASA->getVirtualAddr();
    Code16Len = (CalcAddress (code16,  Code16Start) + 2 - 1) & ~ (2 - 1) - Code16Start;
    VASA->allocateVirtualSpace (Code16Start, Code16Len);

// Addresses of DATA

    DataStart = VASA->getVirtualAddr();
    DataLen = CalcAddress (data,  DataStart) - DataStart;
    VASA->allocateVirtualSpace (DataStart, DataLen);

// Addresses of BSS

    BSSStart = VASA->getVirtualAddr();
    BSSLen = CalcAddress (bss,  BSSStart) - BSSStart;
    VASA->allocateVirtualSpace (BSSStart, BSSLen);

// Addresses of RData
    RDataStart = VASA->getVirtualAddr();
    RDataLen = CalcAddress (rdata,  RDataStart) - RDataStart;
    VASA->allocateVirtualSpace (RDataStart, RDataLen);

#if defined (STACK_SEG)
    dword StackStart = VASA->getVirtualAddr();
    VASA->allocateVirtualSpace (StackStart, CalcAddress (stack, StackStart));
#endif

    if (xJetComponent || (JImportDLLs != NULL)) {
    // Addresses of JImport
        JImportStart = VASA->getVirtualAddr();
        FormJImportImage ();
        JImportLen = JImportImage -> Index;
        VASA->allocateVirtualSpace (JImportStart, JImportLen);
    }

    VASA -> checkVirtualSpaceSize ();

// COFF import
    IdataStart = VASA->getVirtualAddr();
    dword IdataDataStart = IdataStart + Ndlls * 4 * 5;
    IdataLen = CalcAddress (idata, IdataDataStart) - IdataDataStart;

// Занести адреса в таблицу имен
    NAMES.Iterate (SetOffset);
}

/*-------------------------------------------------------------------------*/
/*                        FIXUPS PROCESSING                                */
/*-------------------------------------------------------------------------*/

static dword * fix;

/* relocations statistics */
int CodeFixups = 0;
int ImportFixups = 0;
int DataFixups = 0;
int RDataFixups = 0;

int FixupTargetsImport = 0;
int FixupTargetsCode   = 0;
int FixupTargetsBSS    = 0;
int FixupTargetsIData  = 0;
int FixupTargetsRData  = 0;
int FixupTargetsData   = 0;
int FixupTargetsImportJump = 0;

int FixupTargetsJImport = 0;

static void relocatableFixup (dword address) {
    * fix ++ = address;
    if ((CodeStart <= address) && (address < ImportStart))
        CodeFixups++;
    if ((ImportStart <= address) && (address < CodeStart + CodeLen))
        ImportFixups++;
    if ((DataStart <= address) && (address < DataStart + DataLen))
        DataFixups++;
    if ((RDataStart <= address) && (address < RDataStart + RDataLen))
        RDataFixups++;
}

int TDIndex16Fixups = 0;
int TDIndex32Fixups = 0;

static void ProcessFixup (Segment * seg, struct fixup * f, byte * location)
{
    ASSERT (seg != NULL);
    ASSERT (f   != NULL);

    ASSERT (seg -> isProcessed ());

    // Process special fixups

    switch (f -> kind) {
        case FIXUP_TDINDEX16:
        {
            ASSERT (f -> k_target == TK_ID);

            const LocalTypesTable* table = seg->getLocalTypesTable();
            ASSERT (table != NULL);

            int target = table->getTDIndex16 ((ident) (f -> target));
            ASSERT (target > 0);
            ASSERT (target <= MAX_TDINDEX16);

            dword * fixup = (dword *) (location + f -> offset);
            * ((word *) fixup) = (word) target;

            TDIndex16Fixups++;
            return;
        }

        case FIXUP_TDINDEX32:
        {
            ASSERT (f -> k_target == TK_ID);

            const LocalTypesTable* table = seg->getLocalTypesTable();
            ASSERT (table != NULL);

            int target = table->getTDIndex32 ((ident) (f -> target));
            ASSERT (target > 0);

            dword * fixup = (dword *) (location + f -> offset);
            *fixup = (dword) target;

            TDIndex32Fixups++;
            return;
        }

        case FIXUP_JAVASTRING32:
        case FIXUP_BYTESTRING32:
        {
            ASSERT (f -> k_target == TK_TARGET32);
            FixupTarget32 * target = (FixupTarget32 *) (f -> target);
            dword * fixup = (dword *) (location + f -> offset);
            * fixup = target->getTargetValue();
            return;
        }
    }

    int target;
    Bool import_var_fixup  = false;
    Bool import_func_fixup = false;
    importNameInfo * imp = NULL;

    switch (f -> k_target) {
        case TK_SEG:
            {
                Segment * s = (Segment *) (f -> target);
                if (s -> is16bit())
                    target = s -> address - s -> collection -> base_vadr;
                else
                    target = s -> address;
            }
            break;

        case TK_IMPORT_ID:
            imp = (importNameInfo *) (f -> target);
            ASSERT ( imp -> kind & K_USED);
            ASSERT ((imp -> kind & K_MASK) == K_IMPORT);
            ASSERT ( imp -> seg == NULL);

            imp -> newFixup ((dword *) (location + f -> offset));

            if (xIMAGE_FORMAT == xPE_IMAGE_FORMAT) {
                if (imp -> kind & K_IMPFUNC)
                    import_func_fixup = true;
                else {
                    import_var_fixup = true;
                }
            }
            if (xIMAGE_FORMAT == xLX_IMAGE_FORMAT)
                import_var_fixup = true;
            target = imp -> offset;
            break;

        case TK_JIMPORT_ID:
            {
                jimportNameInfo * jimp = (jimportNameInfo *) (f -> target);
                ASSERT ( jimp -> kind & K_USED);
                ASSERT ((jimp -> kind & K_MASK) == K_JIMPORT);
                ASSERT ( jimp -> seg == NULL);
                if (xIMAGE_FORMAT == xPE_IMAGE_FORMAT) {
                    if (jimp -> kind & K_IMPFUNC)
                        import_func_fixup = true;
                    else
                        import_var_fixup = true;
                }
                ASSERT (xIMAGE_FORMAT != xLX_IMAGE_FORMAT);
/*
                if (xIMAGE_FORMAT == xLX_IMAGE_FORMAT)
                    import_var_fixup = true;
*/
                target = jimp -> offset;
            }
            break;

        default:
            ASSERT_FALSE ();
    }

    if ((xIMAGE_FORMAT == xLX_IMAGE_FORMAT) && (import_var_fixup)) {
        struct impVarFixup * ivf = (struct impVarFixup *) xalloc(sizeof(struct impVarFixup));
        ivf -> name = imp;
        ivf -> address = (dword *) (location + f -> offset);
        ivf -> vadr = seg -> address + f -> offset;
        ivf -> kind = f -> kind;
        ivf -> next = ImpVarFixups;
        ImpVarFixups = ivf;
    }

    dword trg = target;
    target += f -> fx_offset;

    dword * fixup = (dword *) (location + f -> offset);

    switch (f -> kind) {
        case FIXUP_SELFRELOFFS32:
            if (!(xIMAGE_FORMAT == xLX_IMAGE_FORMAT && import_var_fixup))
                * fixup += target - 4 - seg->address - f->offset;
            break;

        case FIXUP_ADDRESS32:
            if (!(xIMAGE_FORMAT == xLX_IMAGE_FORMAT && import_var_fixup))
                * fixup += target;
            if (((xIMAGE_FORMAT != xLX_IMAGE_FORMAT) && ((f -> k_target == TK_IMPORT_ID) || (f -> k_target == TK_JIMPORT_ID))) || (f -> k_target == TK_SEG)) {

                if (f -> k_target == TK_IMPORT_ID)
                    FixupTargetsImport++;
                if (f -> k_target == TK_JIMPORT_ID)
                    FixupTargetsJImport++;
                if (f -> k_target == TK_SEG) {
                    Segment * s = (Segment *) (f -> target);
                    if (s -> clazz == CODE) {
                       FixupTargetsCode++;
                    } else if (s -> clazz == BSS) {
                       FixupTargetsBSS++;
                    } else if (s -> clazz == IDATA) {
                       FixupTargetsIData++;
                    } else if (s -> clazz == CONST) {
                       FixupTargetsRData++;
                    } else {
                       FixupTargetsData++;
                    }
                }

                relocatableFixup (seg -> address + f -> offset);
                if ( ((long)(*fixup - trg) < 0) && (xIMAGE_FORMAT == xLX_IMAGE_FORMAT)) {
                    FixTrg[NFix]  = *fixup;
                    FixRTrg[NFix] = trg;
                    NFix++;
                }
            }
            break;

        case FIXUP_CONSTADDRESS32:
            * fixup += target;
            break;

        case FIXUP_OFFSET32NB:
            * fixup += target - xImageBase;
            break;

        case FIXUP_FAR16_32:
            new_FAR16_xxFixup (seg -> address + f -> offset, target + *fixup, FIXUP_FAR16_32);

            * fixup += target;
            * (word *)((byte *)(fixup) + 4) = 0;
            break;

        case FIXUP_FAR16_16:
            if (import_var_fixup)
                break;
            ASSERT (target < 0x10000);
            ASSERT (f -> k_target == TK_SEG);
            new_FAR16_xxFixup (seg -> address + f -> offset, ((Segment *)(f -> target)) -> address, FIXUP_FAR16_16);

            *(word *)fixup += target;
            *(word *)((byte *)(fixup) + 2) = 0;
            break;

        default:
            Message (xFATAL, msgINVALID_FIXUP_FOR_FLAT_MOD);
            break;
    }
}

static void ProcessFixups (struct collection * c, byte * image, dword base)
{
    while (c) {
        for (Segment * s = c -> segs; s; s = s -> link) {
            ASSERT (s -> isProcessed ());

            for (int i = 0; i < s -> nfixups; i++)
                ProcessFixup (s, &(s->fixups[i]), image + s -> address - base);
        }

        c = c -> next;
    }
}

/*-------------------------------------------------------------------------*/

static void CreateCollectionImage (struct collection * coll,
                                   dword len,
                                   byte ** image,
                                   byte filler)
{
    if (coll) {
        VerboseMessage (INFO_IMAGECREATION, "[IMAGE] Creating image len:%XH\n", len);
        * image = (byte *) xalloc (len);
        memset (* image, filler, len);
        int offset = 0;
        do {
            VerboseMessage (INFO_IMAGECREATION, "[IMAGE] Subimage %s\n", NAMES.Index2Str(coll->name));
            for (Segment * s = coll -> segs; s; s = s -> link) {
                int padding = ((offset + s -> alignment - 1) & ~ (s -> alignment - 1)) - offset;
                offset += padding;
                int segLen = s -> getLen();

                if (s->file && s->file->lib)
                    VerboseMessage (INFO_IMAGECREATION, "[IMAGE] <LIBRARY: %s> %s SIZE:%d SEGMENT %s OFFS:%XH ALIGN:%d\n",
                                    s->file->lib, s->file->getFilename (), segLen+padding, NAMES.Index2Str(s->name), offset, s->alignment);
                else
                    VerboseMessage (INFO_IMAGECREATION, "[IMAGE] FILE:%s SIZE:%d SEGMENT %s OFFS:%XH ALIGN:%d\n",
                                    (s->file ? s->file->getFilename () : "NONE"), segLen+padding, NAMES.Index2Str(s->name), offset, s->alignment);

                ASSERT (offset + segLen <= (int) len);
                if (segLen != 0) {
                    ASSERT (segLen > 0);
                    if (s -> clazz == BSS) {
                        ASSERT (s -> getText() == NULL);
                    } else {
                        s -> copyTextTo ((* image) + offset);
                        s -> freeText();
                    }
                    offset += segLen;
                }
            }
            coll = coll -> next;
        } while (coll);
//        ASSERT (offset == len);
        VerboseMessage (INFO_IMAGECREATION, "[IMAGE] Image created.\n");
    }
}

static int XCDECL compare (const void * p1, const void * p2)
{
    dword o1 = * (const dword *) p1;
    dword o2 = * (const dword *) p2;
    return (o1 < o2) ? -1 : (o1 == o2) ? 0 : 1;
}

void PatchJImportJumpTable (void);
void SortStackTraceInfoTable ();


static void printSectionSizes()
{
    FILE * out = fopen (xSectionSizesLog, "a");
    if (out == NULL) {
        Message(xWARNING, msgUNABLE_TO_WRITE_FILE, xSectionSizesLog);
        return;
    }

    // strip path from output file name
    const char * p = strrchr (xOutputFileName, '\\');
    if (p == NULL)
        p = strrchr (xOutputFileName, '/');
    if (p == NULL)
        p = xOutputFileName;
    else
        p ++;

    fprintf (out, "\"%s\" .text %d .data %d .rdata %d\n", p, CodeLen, DataLen, RDataLen);
    fclose (out);
}

void CreateImages (void)
{
    if (code)
        CreateCollectionImage (code,   CodeLen,   & CodePtr,   0x90);
    else
        if (CodeLen) {
            CodePtr = (byte *) xalloc(CodeLen);
            memset(CodePtr, 0x90, CodeLen);
        }
    CreateCollectionImage (code16, Code16Len, & Code16Ptr, 0x90);
    CreateCollectionImage (data,   DataLen,   & DataPtr,   0);
    CreateCollectionImage (rdata,  RDataLen,  & RDataPtr,  0);
    CreateCollectionImage (idata,  IdataLen,  & IdataPtr,  0);

    Fix = fix = (dword *) xalloc (NFixups * sizeof (dword));
    if (xIMAGE_FORMAT == xLX_IMAGE_FORMAT) {
        FixTrg    = (dword *) xalloc (NFixups * sizeof (dword));
        FixRTrg   = (dword *) xalloc (NFixups * sizeof (dword));
    }

    ProcessFixups (code,   CodePtr,   CodeStart);
    ProcessFixups (code16, Code16Ptr, Code16Start);

    if (xIMAGE_FORMAT == xPE_IMAGE_FORMAT) {
        dword addr, end;
        for (addr = ImportStart, end = CodeStart + CodeLen; addr < end; addr += 6) {
            FixupTargetsImportJump++;
            relocatableFixup (addr + 2);
        }
    } else if (xIMAGE_FORMAT == xELF_IMAGE_FORMAT)
    {
        for (JImportDLL * d = JImportDLLs; d; d = d -> next) {
            for (JImportGroup * g = d -> groups; g; g = g -> next) {
                for (int k = 0; k < g -> entriesfilled; k++) {
                    jimportNameInfo * jinfo = (jimportNameInfo *) (NAMES.getInfo (g -> itemnames[k]));
                    ASSERT (jinfo && ((jinfo -> kind & K_MASK) == K_JIMPORT));
                    if ((jinfo -> kind & K_USED) && (jinfo -> kind & K_IMPFUNC))
                    {
                        FixupTargetsImportJump++;
                        relocatableFixup (jinfo -> offset + 2);
                    }
                }
            }
        }
    }

    ProcessFixups (data, DataPtr, DataStart);

    dword * save_fix = fix;
    ProcessFixups (rdata, RDataPtr, RDataStart);
    if (save_fix != fix)
        Message(xWARNING, msgFIXUP_IN_RDATA);

    ProcessFixups (idata, IdataPtr, IdataStart + Ndlls * 4 * 5);

    ASSERT ((fix - Fix) == NFixups);

// Проверить: fixups уже отсортированы? Отсортировать, если нет
    int n;
    for (n = NFixups, fix = Fix; n > 1; fix ++, n --)
        if (* fix > * (fix + 1)) {
            qsort (Fix, NFixups, sizeof (dword), compare);
            break;
        }

    VerboseMessage (INFO_RELOCSTAT, "\nTDIndex16 Fixups  = %d;  TDIndex32 Fixups  = %d\n", TDIndex16Fixups, TDIndex32Fixups);

    VerboseMessage (INFO_RELOCSTAT, "CodeFixups = %d;  ImportFixups = %d; DataFixups = %d; RDataFixups = %d\n",
                                    CodeFixups, ImportFixups, DataFixups, RDataFixups);

    VerboseMessage (INFO_RELOCSTAT, "FixupTargetsImport = %d; FixupTargetsJImport = %d;\n"
                                    "FixupTargetsCode   = %d; FixupTargetsBSS  = %d;\n"
                                    "FixupTargetsIData  = %d; FixupTargetsRData = %d;\n"
                                    "FixupTargetsData   = %d; FixupTargetsImportJump = %d\n",
                                    FixupTargetsImport, FixupTargetsJImport,
                                    FixupTargetsCode,   FixupTargetsBSS,
                                    FixupTargetsIData,  FixupTargetsRData,
                                    FixupTargetsData,   FixupTargetsImportJump);
    PatchJImportJumpTable ();

    SortStackTraceInfoTable();

    if (xPrintSectionSizes && (xSectionSizesLog != NULL)) {
        printSectionSizes();
    }
}


/*----------------------------------------------------------------------------*/
/*                                  STRINGS                                   */
/*----------------------------------------------------------------------------*/

///////////////////////////////// Java Strings /////////////////////////////////

StringOptimizer<unichar> * JavaStringsOptimizer = NULL;

JavaStringNode * NewJavaString (int len, unichar *string)
{
    if (JavaStringsOptimizer == NULL)
        JavaStringsOptimizer = new StringOptimizer<unichar>();

    return JavaStringsOptimizer -> AddString (len, string);
}


void PrepareJavaStrings (void)
{
    int NJavaStrings        = 0;
    int TotalJavaStringsLen = 0;

    if (JavaStringsOptimizer != NULL) {
        VerboseMessage (INFO_STRINGOPT, "Java Strings Collected. Total %d strings, %d length\n", JavaStringsOptimizer->getNodes(), JavaStringsOptimizer->getTotalLen());
        JavaStringsOptimizer->Optimize (xOptStrLevel);
        NJavaStrings        = JavaStringsOptimizer->getNodes();
        TotalJavaStringsLen = JavaStringsOptimizer->getTotalLen();
    }
    if ((JavaStringsOptimizer != NULL) || xJetComponent) {

        new OBJFile ("JAVA STRINGS");

        Segment * chars_seg = new Segment (NAMES.Str2Index("Java Strings Chars"), NAMES.Str2Index("JavaStringsChars"), DATA, false, sizeof(struct ArrDesc) + TotalJavaStringsLen*sizeof(unichar), 4);
        struct ArrDesc *ad = (struct ArrDesc *) (chars_seg -> getText());
        ad -> link.tags      = 0;
        ad -> link.td        = 0;
        ad -> jsync          = jsyncFreeObject;
        ad -> kind           = k_array;
        ad -> elTCode        = 0;
        ad -> dimnum         = 1;
        ad -> length         = TotalJavaStringsLen;
        NewPublicName (NAMES.Str2Index("LINK_JavaStringsChars"), chars_seg, sizeof (struct Link), T_DATA);

        unichar * chars = (unichar *) (chars_seg -> getText() + sizeof (struct ArrDesc));
        int chars_pos = 0;

        Segment * info_seg = new Segment (NAMES.Str2Index("Java Strings Info"), NAMES.Str2Index("JavaStringsInfo"), CONST, false, (NJavaStrings == 0) ? 4 : NJavaStrings*sizeof(struct StringInfo), 4);
        NewPublicName (NAMES.Str2Index("LINK_GlobalJavaStringsInfo"), info_seg, 0, T_DATA);

        struct StringInfo * strinfo = (struct StringInfo *) (info_seg -> getText());
        int strindex = 0;

        if (JavaStringsOptimizer != NULL) {
            JavaStringNode * JavaStringsList = JavaStringsOptimizer->List;
            ASSERT ((JavaStringsList->chars == NULL) && (JavaStringsList->ikind == 0));
            for (JavaStringNode * node = JavaStringsList->next; node != JavaStringsList; node = node->next) {
                ASSERT (!node->isMerged());

                // Emit Characters
                memcpy (chars, node -> chars -> str, node -> chars -> len * sizeof (unichar));

                int globalCharsPos = chars_pos;
                node -> chars -> globalpos = globalCharsPos;
        
                chars     += node -> chars -> len;
                chars_pos += node -> chars -> len;

                // Emit String Info
                strinfo -> hash = node -> hash;
                strinfo -> pos  = node -> pos + globalCharsPos;
                strinfo -> len  = node -> len;

                node -> setTargetValue (strindex);

                strinfo   += 1;
                strindex  += 1;

                // Emit Merged Strings
                for (JavaStringNode * mnode = node->merged; mnode; mnode = mnode->merged) {
                    ASSERT (mnode->isMerged());
                    if (mnode->ikind == I_OBJ_MERGED) {
                        // Whole String Object merged, just store index
                        mnode -> setTargetValue (mnode->fwd->getTargetValue());
                    } else if (mnode->ikind == I_CHARS_MERGED) {
                        // Chars merged, Emit String Info
                        strinfo -> hash = mnode -> hash;
                        strinfo -> pos  = mnode -> pos + globalCharsPos;
                        strinfo -> len  = mnode -> len;

                        mnode -> setTargetValue (strindex);

                        strinfo   += 1;
                        strindex  += 1;
                    } else {
                        ASSERT_FALSE ();
                    }
                }
            }
            ASSERT (chars - ((unichar *) (chars_seg -> getText() + sizeof (struct ArrDesc))) == TotalJavaStringsLen);
            ASSERT (strinfo - ((struct StringInfo *) (info_seg -> getText())) == NJavaStrings);
            ASSERT (strindex == NJavaStrings);

            if ((xEncryptStrings != ENCRYPTION_KEY_NO_ENCRYPTION_VALUE) && (xEncryptStrings != ENCRYPTION_KEY_UNDEFINED_VALUE)) {
                byte key = (byte)xEncryptStrings;
                byte * char_arr = (byte *)(chars_seg -> getText() + sizeof (struct ArrDesc));
                while (char_arr < (byte *)chars) {
                    *char_arr ^= key;
                    char_arr++;
                }
            }
        }

        Segment * pool_seg = new Segment (NAMES.Str2Index("Java Strings Pool"), NAMES.Str2Index("JavaStringsPool"), BSS, false, (NJavaStrings == 0) ? 4 : NJavaStrings*sizeof(void *), 4);
        NewPublicName (NAMES.Str2Index("LINK_GlobalJavaStringsPool"), pool_seg, 0, T_DATA);
    }
}

///////////////////////////////// Byte Strings /////////////////////////////////

StringOptimizer<byte> * ByteStringsOptimizer = NULL;

ByteStringNode * NewByteString (int len, byte *string)
{
    if (ByteStringsOptimizer == NULL)
        ByteStringsOptimizer = new StringOptimizer<byte>();

    return ByteStringsOptimizer -> AddString (len, string);
}

#define BYTE_STR_MAX_OFFS    0x01000000
#define BYTE_STR_MAX_LEN     0x80
#define BYTE_STR_LEN_SHIFT   24
#define BYTE_STR_INDEX_FLAG  0x80000000

struct ByteStringInfo {
    dword offs;
    dword len;
};

void PrepareByteStrings (void)
{
    int NByteStrings        = 0;
    int TotalByteStringsLen = 0;

    if (ByteStringsOptimizer != NULL) {
        VerboseMessage (INFO_STRINGOPT, "Byte Strings Collected. Total %d strings, %d length\n", ByteStringsOptimizer->getNodes(), ByteStringsOptimizer->getTotalLen());
        ByteStringsOptimizer->Optimize (xOptStrLevel);
        NByteStrings        = ByteStringsOptimizer->getNodes();
        TotalByteStringsLen = ByteStringsOptimizer->getTotalLen();
    }
    if ((ByteStringsOptimizer != NULL) || xJetComponent) {

        new OBJFile ("BYTE STRINGS");

        Segment * chars_seg = new Segment (NAMES.Str2Index("Byte Strings Chars"), NAMES.Str2Index("ByteStringsChars"), CONST, false, (TotalByteStringsLen ? TotalByteStringsLen : 0), 4);
        NewPublicName (NAMES.Str2Index("LINK_ByteStringsChars"), chars_seg, 0, T_DATA);
      
        byte * chars = chars_seg -> getText();
        int chars_pos = 0;

        Segment * info_seg = new Segment (NAMES.Str2Index("Byte Strings Info"), NAMES.Str2Index("ByteStringsInfo"), CONST, false, NByteStrings*sizeof(struct ByteStringInfo), 4);
        NewPublicName (NAMES.Str2Index("LINK_GlobalByteStringsInfo"), info_seg, 0, T_DATA);

        struct ByteStringInfo * strinfo = (struct ByteStringInfo *) (info_seg -> getText());
        int strindex = 0;

        if (ByteStringsOptimizer != NULL) {
            ByteStringNode * ByteStringsList = ByteStringsOptimizer->List;

            // check first fake node
            ASSERT ((ByteStringsList->chars == NULL) && (ByteStringsList->ikind == 0));

            for (ByteStringNode * node = ByteStringsList->next; node != ByteStringsList; node = node->next) {
                ASSERT (!node->isMerged());

                // Emit Characters
                memcpy (chars, node -> chars -> str, node -> chars -> len);

                int globalCharsPos = chars_pos;
                node -> chars -> globalpos = globalCharsPos;
        
                chars     += node -> chars -> len;
                chars_pos += node -> chars -> len;

                {
                    dword offs = node -> pos + globalCharsPos;
                    dword len  = node -> len;

                    if ((len < BYTE_STR_MAX_LEN) && (offs < BYTE_STR_MAX_OFFS))
                        node -> setTargetValue ((len << BYTE_STR_LEN_SHIFT) + offs);
                    else {
                        // Emit String Info
                        strinfo -> offs = offs;
                        strinfo -> len  = len;
     
                        node -> setTargetValue (strindex | BYTE_STR_INDEX_FLAG);

                        strinfo   += 1;
                        strindex  += 1;
                    }
                }
                // Emit Merged Strings
                for (ByteStringNode * mnode = node->merged; mnode; mnode = mnode->merged) {
                    ASSERT (mnode->isMerged());
                    if (mnode->ikind == I_OBJ_MERGED) {
                        // Whole String Object merged, just store index
                        mnode -> setTargetValue (mnode->fwd->getTargetValue());
                    } else if (mnode->ikind == I_CHARS_MERGED) {
                        // Chars merged, calculate index

                        dword offs = mnode -> pos + globalCharsPos;
                        dword len  = mnode -> len;

                        if ((len < BYTE_STR_MAX_LEN) && (offs < BYTE_STR_MAX_OFFS))
                            mnode -> setTargetValue ((len << BYTE_STR_LEN_SHIFT) + offs);
                        else {
                            // Emit String Info
                            strinfo -> offs = offs;
                            strinfo -> len  = len;
         
                            mnode -> setTargetValue (strindex | BYTE_STR_INDEX_FLAG);

                            strinfo   += 1;
                            strindex  += 1;
                        }
                    }
                }
            }
            ASSERT ((chars - (chars_seg -> getText())) == TotalByteStringsLen);
            info_seg -> setLen(strindex*sizeof(struct ByteStringInfo));
            VerboseMessage (INFO_STRINGOPT, "Byte Strings Prepared. Total %d strings stored, %d strings represented as index\n", NByteStrings, strindex);

            if ((xEncryptStrings != ENCRYPTION_KEY_NO_ENCRYPTION_VALUE) && (xEncryptStrings != ENCRYPTION_KEY_UNDEFINED_VALUE)) {
                byte key = (byte)xEncryptStrings;
                byte * char_arr = (byte *)(chars_seg -> getText());
                while (char_arr < chars) {
                    *char_arr ^= key;
                    char_arr++;
                }
            }
        }
    }
}

//////////////////////////////// String Chars ///////////////////////////////

template<class Char> dword StringChars<Char>::calcSpectrum (int offset, int count)
{
    Char * value = str + offset;
    dword sp = 0;
    while (count > 0) {
        sp |= 1 << ((*value) % 32);
        count--;
        value++;
    }
    return sp;
}

template<class Char> int StringChars<Char>::hashCode (int offset, int count)
{
    Char * value = str + offset;
    int hash = 0;
    while (count > 0) {
        hash = hash*31 + (*value);
        count--;
        value++;
    }
    return hash;
}

template<class Char> inline int StringChars<Char>::isSubstring (StringChars<Char> * subchars)
{
    int sublen = subchars->len;

    Char * substr = subchars->str;

    for (int pos = 0 ;pos + sublen <= len; pos++) {
        Bool match = true;
        for (int i = 0; i < sublen; i++) {
            if (substr [i] != str [pos + i]) {
                match = false;
                break;
            }
        }
        if (match)
            return pos;
    }
    return -1;
}

template<class Char> inline int StringChars<Char>::isOverlapping (StringChars<Char> * subchars)
{
    Char * subchars_str = subchars -> str;
    int subchars_len = subchars -> len;

    int commonLen = len < subchars_len ? len : subchars_len;

    for (int l = commonLen-1; l > 0; l--) {
        Bool match = true;
        int  offs  = len - l;
        for (int pos = 0; pos < l; pos++) {
            if (str [offs + pos] != subchars_str [pos]) {
                match = false;
                break;
            }
        }
        if (match)
            return l;
    }
    return -1;
}


//////////////////////////////// String Node ////////////////////////////////

template<class Char> void StringNode<Char>::mergeNode (StringNode<Char> * _fwd, byte mergeKind, int posIncrement)
{
    ASSERT (!isMerged());
    ASSERT (!_fwd->isMerged());
    ASSERT (((mergeKind == I_OBJ_MERGED) && (posIncrement == 0))||
             (mergeKind == I_CHARS_MERGED));

    delete chars;
    chars = NULL;
    fwd   = _fwd;

    pos += posIncrement;

    StringNode<Char> * last = NULL;
    for (StringNode<Char> * m = merged; m; m = m->merged) {
        if (m->ikind == I_CHARS_MERGED) {
            m -> pos += posIncrement;
            m -> fwd  = _fwd;
        }
        last = m;
    }

    next -> prev = prev;
    prev -> next = next;
    next = NULL;
    prev = NULL;

    if (last)
        last->merged = fwd->merged;
    else
        merged = fwd -> merged;

    fwd -> merged = this;

    ikind = mergeKind;
}

template<class Char> void StringNode<Char>::mergeOverlapping (StringNode<Char> * subnode, int overLen)
{
    ASSERT (!          isMerged());
    ASSERT (! subnode->isMerged());

    int newLen = chars->len + subnode->chars->len - overLen;

    chars->str = (Char *) xrealloc (chars->str,
                                    chars->len * sizeof (Char),
                                    newLen     * sizeof (Char));
    memcpy (chars->str + chars->len, subnode->chars->str + overLen, (subnode->chars->len - overLen) * sizeof (Char));

    int posInc = chars->len - overLen;
    chars->len = newLen;

    chars->spectrum |= subnode->chars->spectrum;

    subnode->mergeChars (this, posInc);
}

////////////////////////////// String Optimizer //////////////////////////////

template<class Char> StringOptimizer<Char>::StringOptimizer ()
{
    List = new StringNode<Char>();

    List->next = List;
    List->prev = List;

    Nodes = 0;
    ListLen = 0;
    TotalLen = 0;
}


template<class Char> StringNode<Char> * StringOptimizer<Char>::AddString (int len, Char *string)
{
    StringNode<Char> * node = new StringNode<Char> ();
    node -> chars = new StringChars<Char> (len, string);

    node -> pos  = 0;
    node -> len  = len;
    node -> hash = node -> chars -> hashCode ();

    node -> spectrum = node -> chars -> calcSpectrum ();

    // insert node into the double-linked list
    node -> next = List -> next;
    node -> prev = List;
    List -> next -> prev = node;
    List -> next         = node;

    Nodes++;
    ListLen++;
    TotalLen += len;

    return node;
}

#define StringOptimizerLevel1HashtableLoadFactor 0.75

template<class Char> void StringOptimizer<Char>::Optimize (int Level)
{
    if (List->next == List)
        return;               // Empty List

    if (Level == 0)
        return;

    // Level 1. Merging of string info for equal strings

    // create hashtable
    int hashtableLen = (int) ((double)Nodes / StringOptimizerLevel1HashtableLoadFactor);
    StringNode<Char> ** hashtable = (StringNode<Char> **) xalloc (hashtableLen * sizeof (StringNode<Char> *));
    memset (hashtable, 0, hashtableLen * sizeof (StringNode<Char> *));

    // fill hashtable, merging equal strings
    for (StringNode<Char> * node = List->next; node != List; node = List->next)
    {
        int index = ((dword) (node -> hash)) % hashtableLen;
        Bool merged = false;
        for (StringNode<Char> * candidate = hashtable[index]; candidate != NULL; candidate = candidate->next)
        {
            if ((node->hash == candidate->hash) && (node->len == candidate->len))
            {
                Char * chars1 = node->chars->str + node->pos;
                Char * chars2 = candidate->chars->str + candidate->pos;
                int len = node->len;
                Bool equals = true;
                for (int i = 0; i < len; i++)
                    if (chars1[i] != chars2[i]) {
                        equals = false;
                        break;
                    }
                if (equals) {
                    node -> mergeObject (candidate);
                    Nodes--;
                    ListLen--;
                    TotalLen-=candidate->chars->len;
                    merged = true;
                    break;
                }
            }
        }
        if (!merged) {
            // remove from double-linked list
            node -> next -> prev = node -> prev;
            node -> prev -> next = node -> next;
            // insert into hashtable
            node -> next = hashtable [index];
            node -> prev = NULL;
            hashtable [index] = node;
        }
    }
    // now we should convert hashtable back to list (ring)
    StringNode<Char> * listPrev = List;
    for (int index = 0; index < hashtableLen; index++) {
        StringNode<Char> * next = NULL;
        for (StringNode<Char> * node = hashtable[index]; node != NULL; node = next)
        {
            next = node -> next;
            node -> next = List -> next;
            node -> prev = List;
            node -> next -> prev = node;
            List -> next = node;
        }
    }
    xfree (hashtable);

    VerboseMessage (INFO_STRINGOPT, "Strings Optimized, Level 1. Total %d strings, %d length\n", Nodes, TotalLen);

    if (Level == 1)
        return;

    /* Level 2. Merging of chars for substrings */

    {
        int progress = 1;
        int percent  = 0;

        for (StringNode<Char> * node = List->next; node != List; node = node->next, progress++) {

            StringNode<Char> * subnode_next;

            for (StringNode<Char> * subnode = List->next; subnode != List; subnode = subnode_next) {
                subnode_next = subnode->next;

                if (node == subnode)
                    continue;

                if (subnode->len >= node->len)
                    continue;

                if ((node->spectrum & subnode->spectrum) != subnode->spectrum) 
                    continue;

                int subpos = node->chars->isSubstring (subnode->chars);

                if (subpos >= 0) {
                    /* update chars */
                    ListLen--;
                    TotalLen-=subnode->chars->len;
                    subnode->mergeChars (node, subpos);
                }
            }

            int cur_percent = (progress > ListLen) ? 100 : (int) ((progress*100)/ListLen);
            if (percent != cur_percent) {
                percent = cur_percent;
                VerboseMessage (INFO_STROPTPROGRESS, "%02d%% strings optimized (level 2)\n", percent);
            }
        }

        VerboseMessage (INFO_STRINGOPT, "Strings Optimized, Level 2. Total %d strings, %d length\n", Nodes, TotalLen);
    }

    if (Level == 2)
        return;

    /* Level 3. Merging of overlapping chars */

    {
        int progress = 1;
        int percent  = 0;
        for (StringNode<Char> * node = List->next; node != List; node = node->next, progress++) {

            int maxOverLen = -1;
            StringNode<Char> * bestSubnode = NULL;

            for (StringNode<Char> * subnode = List->next; subnode != List; subnode = subnode -> next) {
                if (node == subnode)
                    continue;

                if ((node->chars->spectrum & (1 << (subnode->chars->str [0] % 32))) == 0)
                    continue;

                int overLen = node->chars->isOverlapping (subnode->chars);
                if (overLen > maxOverLen) {
                    maxOverLen  = overLen;
                    bestSubnode = subnode;
                }
            }
            if (maxOverLen >= MIN_OVERLAPPING_LEN_FOR_MERGE) {
                /* merge overlapping chars */
                ListLen--;
                TotalLen-=maxOverLen;
                node->mergeOverlapping (bestSubnode, maxOverLen);
            }

            int cur_percent = (progress > ListLen) ? 100 : (int) ((progress*100)/ListLen);
            if (percent != cur_percent) {
                percent = cur_percent;
                VerboseMessage (INFO_STROPTPROGRESS, "%02d%% strings optimized (level 2)\n", percent);
            }
        }

        VerboseMessage (INFO_STRINGOPT, "Strings Optimized, Level 3. Total %d strings, %d length\n", Nodes, TotalLen);
    }

    if (Level == 3)
        return;

    ASSERT_FALSE ();
}


/*----------------------------------------------------------------------------*/
/*                           Resources                                        */
/*----------------------------------------------------------------------------*/

ResourceDirectory * resourceDirectoryList = NULL;
int numberOfResources = 0;
int numberOfResourceDirectories = 0;

static int compareResNameID (unichar *name1, word ID1, unichar *name2, word ID2)
{
    if (name1) {
        if (!name2)
            return -1;
        int i = 0;
        for (; name1 [i] == name2 [i]; i ++)
            if (! name1 [i])
                return 0;
        return name1 [i] < name2 [i] ? -1 : 1;
    }
    return name2 ? 1 : ID1 < ID2 ? -1 : ID1 == ID2 ? 0 : 1;
}

int Resource :: compareNameID (Resource * aRes)
{
    return compareResNameID (this->resourceName, this->resourceID,
                             aRes->resourceName, aRes->resourceID);
}

ResourceDirectory *getResourceDirectory (unichar *typeName, word typeID)
{
    // Find resource type
    int compare_result = 0;
    ResourceDirectory * prev = NULL;
    ResourceDirectory * dir_entry = resourceDirectoryList;
    for(; dir_entry; prev = dir_entry, dir_entry = dir_entry -> next) {
        compare_result = compareResNameID (typeName, typeID, dir_entry->typeName, dir_entry->typeID);
        if (compare_result <= 0)
            break;
    }
    if (!dir_entry || (compare_result < 0)) {
        // Insert new resource type
        ResourceDirectory * new_entry = new ResourceDirectory ();
        new_entry -> typeName = typeName;
        new_entry -> typeID   = typeID;
        new_entry -> typeName = typeName;
        new_entry -> resList  = NULL;

        new_entry -> next = dir_entry;
        if (prev)
            prev -> next = new_entry;
        else
            resourceDirectoryList = new_entry;
        numberOfResourceDirectories++;
        return new_entry;
    }
    return dir_entry;
}

Bool ResourceDirectory::addResource (unichar * resName, word resID, dword datasize, byte * data,
                                     dword DataVersion, word MemoryFlags, word LanguageId, dword Version,
                                     dword Characteristics, byte OSType)
{
    // Insert resource into ordered list
    int compare_result;
    Resource * prev = NULL;
    Resource * res  = resList;
    for(; res; prev = res, res = res->next) {
        compare_result = compareResNameID (resName, resID, res->resourceName, res->resourceID);
        if (compare_result == 0) {
            if (xIMAGE_FORMAT == xLX_IMAGE_FORMAT)
                return false;

            if (LanguageId == res->LanguageId)
                return false;

            if (LanguageId < res->LanguageId)
                break;
        }
        if (compare_result < 0)
            break;
    }
    Resource * new_res = new Resource();
    new_res -> resourceName = resName;
    new_res -> resourceID   = resID;

    new_res -> datasize = datasize;
    new_res -> data  = (byte *) xalloc (datasize);
    memcpy (new_res -> data, data, datasize);

    new_res -> DataVersion     = DataVersion;
    new_res -> MemoryFlags     = MemoryFlags;
    new_res -> LanguageId      = LanguageId;
    new_res -> Version         = Version;
    new_res -> Characteristics = Characteristics;
    new_res -> OSType          = OSType;

    new_res -> next            = res;
    if (prev)
        prev -> next = new_res;
    else
        resList = new_res;
    numberOfResources ++;
    return true;
}

/*----------------------------------------------------------------------------*/
/*                           Version control                                  */
/*----------------------------------------------------------------------------*/

VersionStamp* VerStamp;


VersionStamp::VersionStamp()
    : minorCompilerVersion(0)
    , majorCompilerVersion(0)
    , internalCompilerVersion(0)
    , edition(JET_EDITION_UNKNOWN)
    , versionFile(NULL)
    , versionInitialized(false)
{
}


VersionStamp::VersionStamp(byte minorCompilerVersion_,
                           byte majorCompilerVersion_,
                           byte internalCompilerVersion_,
                           byte edition_,
                           const OBJFile* versionFile_,
                           Bool versionInitialized_)
    : minorCompilerVersion(minorCompilerVersion_)
    , majorCompilerVersion(majorCompilerVersion_)
    , internalCompilerVersion(internalCompilerVersion_)
    , edition(edition_)
    , versionFile(versionFile_)
    , versionInitialized(versionInitialized_)
{
}

const char* VersionStamp::getVersionSource() const {
    return (versionFile != NULL) ? versionFile->getFilename() : "specified in options";
}


void VersionStamp::check (const VersionStamp* ver) {
    Bool equals = true;

    if (versionInitialized && ver->versionInitialized) {
        equals = equals &&
                 (minorCompilerVersion == ver->minorCompilerVersion) &&
                 (majorCompilerVersion == ver->majorCompilerVersion);
    }
    if ((edition != JET_EDITION_UNKNOWN) && (ver->edition != JET_EDITION_UNKNOWN)) {
        equals = equals &&
                 (edition == ver->edition);
    }
    if ((versionFile != NULL) && (ver->versionFile != NULL)) {
        equals = equals &&
                 (internalCompilerVersion == ver->internalCompilerVersion);
    }

    if (!equals) {
        const char * src1 = getVersionSource();
        const char * src2 = ver->getVersionSource();

        Message(xFATAL, msgINCOMPATIBLE_VERSIONS, src1, src2);
    }
}


void VersionStamp::merge (const VersionStamp* ver) {
    check(ver);

    if (!versionInitialized && ver->versionInitialized) {
        minorCompilerVersion = ver->minorCompilerVersion;
        majorCompilerVersion = ver->majorCompilerVersion;
        versionInitialized   = true;
    }
    if ((edition == JET_EDITION_UNKNOWN) && (ver->edition != JET_EDITION_UNKNOWN)) {
        edition = ver->edition;
    }
    if ((versionFile == NULL) && (ver->versionFile != NULL)) {
        internalCompilerVersion = ver->internalCompilerVersion;
        versionFile = ver->versionFile;
    }
}


void VersionStamp::setVersionStamp (dword stamp) {
    byte minor    = (byte) (((stamp >> 12) & 0x0F)*10 + ((stamp >> 8) & 0x0F));
    byte major    = (byte) (((stamp >>  4) & 0x0F)*10 + (stamp & 0x0F));
    byte internal = (byte) ((stamp >> 16) & 0xFF);
    byte edition  = (byte) ((stamp >> 24) & 0xFF);

    VersionStamp ver(minor, major, internal, edition, CurrentFile, true);
    merge(&ver);
}

void VersionStamp::setVersion (byte minor, byte major) {
    VersionStamp ver(minor, major, 0, JET_EDITION_UNKNOWN, NULL, true);
    merge(&ver);
}

void VersionStamp::setEdition (byte edition) {
    VersionStamp ver(0, 0, 0, edition, NULL, false);
    merge(&ver);
}


/*----------------------------------------------------------------------------*/
/*                         MAP file                                           */
/*----------------------------------------------------------------------------*/

#ifdef MAP_BY_NAME
int XCDECL compare_names (const void * p1, const void * p2)
{
    return strcmp (NAMES.Index2Str ( *((ident *)p1) ),
                   NAMES.Index2Str ( *((ident *)p2) ));
}
#else
int XCDECL compare_addresses (const void * p1, const void * p2)
{
    dword o1, o2;

    o1 = ((nameInfo *) (NAMES.getInfo ( *((ident *)p1) ))) -> offset;
    o2 = ((nameInfo *) (NAMES.getInfo ( *((ident *)p2) ))) -> offset;
    return (o1 < o2) ? -1 : (o1 == o2) ? 0 : 1;
}
#endif

ident * map = NULL;
int mapSize = 0;

void GatherProcessedSymbols (ident id, void *info)
{
    nameInfo * n = (nameInfo *)info;
    if (n && n -> seg && n -> seg -> isProcessed ())
        map[mapSize++] = id;
}

void CreateMapFile (char * name)
{
    OBJFile * f;
    int i;
    FILE * fi;
    char buf [1024];

    fi = fopen (name, "w");
    if (fi == NULL)
        Message(xFATAL, msgUNABLE_TO_OPEN_FILE, name);

    map = (ident *) xalloc ((NAMES.getTableSize() + 1) * sizeof (ident));
    NAMES.Iterate (GatherProcessedSymbols);

#ifdef MAP_BY_NAME
    qsort (map, mapSize, sizeof (ident), compare_names);
#else
    qsort (map, mapSize, sizeof (ident), compare_addresses);
#endif

    for (f = NULL, i = 0; i < mapSize; i ++) {
        nameInfo * n = (nameInfo *) (NAMES.getInfo (map[i]));
        if (n -> seg -> file && n -> seg -> file != f) {
            f = n -> seg -> file;
            PrintFileBySeg (buf, n -> seg);
            fprintf (fi, "\n------ %s\n", buf);
        }
        fprintf (fi, "%-6s %7X %s\n"
                 ,NAMES.Index2Str (n -> seg -> name)
                 ,n -> offset
                 ,NAMES.Index2Str (map [i])
                );
    }
    xfree (map);
    fclose (fi);
}


/*----------------------------------------------------------------------------*/
/*                             JEXPORT                                        */
/*----------------------------------------------------------------------------*/

int nJExportGroups    = 0;
int JExportGroupsSize = 256;
struct JExportGroup ** JExportGroups = (struct JExportGroup **) xalloc (JExportGroupsSize*sizeof(struct JExportGroup *));

int NewJExportGroup (int hash, ident groupname, int entries)
{
    if (nJExportGroups == JExportGroupsSize) {
        JExportGroupsSize = JExportGroupsSize*2;
        JExportGroups = (struct JExportGroup **) xrealloc (JExportGroups, nJExportGroups*sizeof(struct JExportGroup *), JExportGroupsSize*sizeof(struct JExportGroup *));
    }
    ASSERT (nJExportGroups < JExportGroupsSize);

    struct JExportGroup * g = (struct JExportGroup *) xalloc (sizeof(struct JExportGroup));

    g -> hash      = hash;
    g -> groupname = groupname;
    g -> entries   = entries;
    g -> itemnames = (ident *) xalloc (entries * sizeof (int));

    JExportGroups[nJExportGroups] = g;
    return nJExportGroups++;
}

void NewJExportItem  (int groupIndex, int itemIndex, ident intname)
{
    ASSERT ((groupIndex >= 0) & (groupIndex < nJExportGroups));
    ASSERT ((itemIndex >= 0) & (itemIndex < JExportGroups[groupIndex]->entries));
    JExportGroups[groupIndex]->itemnames [itemIndex] = intname;
    VerboseMessage (INFO_SYMBOLDECLARATION, "SYMDECL: New J export: %s (%d from %s)\n", NAMES.Index2Str(intname), itemIndex, NAMES.Index2Str(JExportGroups[groupIndex]->groupname));
}


Storage * JExportImage = NULL;


static int XCDECL compare_jexport (const void * p1, const void * p2)
{
    struct JExportGroup *e1, *e2;
    e1 = *((struct JExportGroup **)p1);
    e2 = *((struct JExportGroup **)p2);
    int hash1 = e1->hash;
    int hash2 = e2->hash;

    if (hash1 > hash2)
       return 1;

    if (hash1 < hash2)
       return -1;

    return 0;
}

dword generateConsistencyKey ()
{
    return time (0);
}


#define JEXPORT_GROUP_DELIMITER        '.'
#define DYN_LOOKUP_TABLE_SIZE          1024

class DynLookupTableEntry;

static DynLookupTableEntry* DynLookupTable[DYN_LOOKUP_TABLE_SIZE];
static char* CurrentComponentName;

class DynLookupTableEntry {
  private:
    char * id;
    dword hash;
    Bool pkg;
    DynLookupTableEntry * next;

    DynLookupTableEntry (char* _id, int _hash, Bool _pkg) :
        id (_id), hash (_hash), pkg (_pkg)
    {
        this->next = DynLookupTable[hash % DYN_LOOKUP_TABLE_SIZE];
        DynLookupTable[hash % DYN_LOOKUP_TABLE_SIZE] = this;
    }

  public:
    static void addExportGroup (const char* groupname);
    static void write (FILE* f);
};


static dword hash_code (const char* s)
{
    dword hash = 0;
    while (*s != 0) {
        hash = hash*31 + (*s);
        s++;
    }
    return hash;
}


void DynLookupTableEntry :: addExportGroup (const char* groupname)
{
    char* id;
    Bool pkg;

    const char* p = strrchr (groupname, JEXPORT_GROUP_DELIMITER);
    if (p != NULL) {
        id = dup (groupname, p - groupname);
        pkg = true;
    } else {
        id = dup (groupname, strlen(groupname));
        pkg = false;
    }

    dword hash = hash_code (id);

    for (DynLookupTableEntry* entry = DynLookupTable[hash % DYN_LOOKUP_TABLE_SIZE];
         entry != NULL;
         entry = entry->next)
    {
        if ((hash == entry->hash) && !strcmp(id, entry->id) && (pkg == entry->pkg)) {
            return;
        }
    }

    new DynLookupTableEntry(id, hash, pkg);
}


void DynLookupTableEntry :: write (FILE* f)
{
    if (fprintf (f, "D %s\n", CurrentComponentName) < 0) {
        Message(xERROR, msgUNABLE_TO_WRITE_FILE, xDynamicLookupTable);
        return;
    }

    for (int i = 0; i < DYN_LOOKUP_TABLE_SIZE; i++) {
        for (DynLookupTableEntry* entry = DynLookupTable[i];
             entry != NULL;
             entry = entry->next)
        {
            int res;
            if (entry->pkg) {
                res = fprintf (f, "P %s\n", entry->id);
            } else {
                res = fprintf (f, "C %s\n", entry->id);
            }
            if (res < 0) {
                Message(xERROR, msgUNABLE_TO_WRITE_FILE, xDynamicLookupTable);
                return;
            }
        }
    }
}


static void EmitDynamicLookupTable ()
{
    // strip output file name
    const char * p = strrchr (xOutputFileName, '\\');
    if (p == NULL)
        p = strrchr (xOutputFileName, '/');
    if (p == NULL)
        p = xOutputFileName;
    else
        p ++;
    CurrentComponentName = dup (p, strlen(p));

    if ((xIMAGE_FORMAT == xPE_IMAGE_FORMAT) || (xIMAGE_FORMAT == xLX_IMAGE_FORMAT)) {
        // convert current component name to uppercase
        char *p = CurrentComponentName;
        do {
            *p = (char) toupper(*p);
        } while (* (p ++));
    }

    memset (DynLookupTable, 0, DYN_LOOKUP_TABLE_SIZE*sizeof(DynLookupTableEntry*));

    for (int i = 0; i < nJExportGroups; i++) {
        DynLookupTableEntry :: addExportGroup(NAMES.Index2Str (JExportGroups[i]->groupname));
    }

    FILE* f = fopen (xDynamicLookupTable, "a");
    if (f == NULL) {
        Message(xERROR, msgUNABLE_TO_OPEN_FILE, xDynamicLookupTable);
        return;
    }
    DynLookupTableEntry :: write (f);
    fclose(f);
}


void FormJExportImage (void)
{
    if (xEmitDynamicLookupTable && (xDynamicLookupTable != NULL)) {
        EmitDynamicLookupTable();
    }

    int i;

    // sort by hash
    qsort (JExportGroups, nJExportGroups, sizeof (struct JExportGroup *), compare_jexport);

    JExportImage = newStorage (65536);

    JExportImage -> Put4 (JE_MAGIC);

    if (xNoConsistencyInfo)
        JExportImage -> Put4 (0);
    else
        JExportImage -> Put4 (generateConsistencyKey());

    JExportImage -> Put4 (nJExportGroups);
    JExportImage -> Put4 (0);                // hashIndexTableOfs
    JExportImage -> Put4 (0);                // groupNamesTableOfs
    JExportImage -> Put4 (0);                // exportAddrsTableOfs

    JExportImage -> Align4 ();
    * ((dword *)(JExportImage -> Ptr + 12)) = JExportImage -> Index; // hashIndexTableOfs

    for (i = 0; i < nJExportGroups; i++) {
        JExportImage -> Put4 (JExportGroups[i]->hash);
    }

    JExportImage -> Align4 ();
    dword groupNamesTableOfs = JExportImage -> Index;
    * ((dword *)(JExportImage -> Ptr + 16)) = groupNamesTableOfs; // groupNamesTableOfs

    JExportImage -> ZeroBytes (nJExportGroups*4);

    for (i = 0; i < nJExportGroups; i++) {
        * ((dword *)(JExportImage -> Ptr + groupNamesTableOfs + i*4)) = JExportImage -> Index;
        
        JExportImage -> PutEncryptedS (NAMES.Index2Str (JExportGroups[i]->groupname), NAMES.Index2StrLen (JExportGroups[i]->groupname) + 1, IMPORT_EXPORT_ENCRYPTION_KEY);
    }

    JExportImage -> Align4 ();
    dword exportAddrsTableOfs = JExportImage -> Index;
    * ((dword *)(JExportImage -> Ptr + 20)) = exportAddrsTableOfs; // exportAddrsTableOfs

    JExportImage -> ZeroBytes (nJExportGroups*4);

    for (i = 0; i < nJExportGroups; i++) {
        * ((dword *)(JExportImage -> Ptr + exportAddrsTableOfs + i*4)) = JExportImage -> Index;
        JExportGroup * g = JExportGroups[i];
        JExportImage -> Put4 (g -> entries);
        for (int j = 0; j < g -> entries; j++)
            JExportImage -> Put4 ( ((nameInfo *) (NAMES.getInfo (g -> itemnames[j]))) -> offset - xImageBase);
        VerboseMessage (INFO_JEXPORT, "Export group: %s (hash = %d, entries = %d)\n", NAMES.Index2Str(g->groupname), g->hash, g->entries);
    }
}

/*----------------------------------------------------------------------------*/
/*                             JIMPORT                                        */
/*----------------------------------------------------------------------------*/

JImportDLL * JImportDLLs = NULL;

struct JImportDLL * getJImportDLL (ident dllname) {
    struct JImportDLL * dll = JImportDLLs;
    for (; dll; dll = dll -> next)
        if (dll -> name == dllname)
            return dll;

    dll = (struct JImportDLL *) allocateForever (sizeof (struct JImportDLL));
    dll -> name   = dllname;
    dll -> groups = NULL;
    dll -> used   = false;
    dll -> cKey   = 0;

    dll -> next   = JImportDLLs;
    JImportDLLs = dll;

    return dll;
}


struct JImportGroup * GetJImportGroup (ident dllname, int hash, ident groupname, int entries)
{
    struct JImportDLL * dll = getJImportDLL (dllname);

    struct JImportGroup * g = dll -> groups;
    for (; g; g = g -> next) {
        if (g -> groupname == groupname) {
            break;
        }
    }
    ASSERT (entries >= 0);

    if (g == NULL) {
        g = (struct JImportGroup *) xalloc (sizeof (struct JImportGroup));

        g -> hash          = hash;
        g -> groupname     = groupname;
        g -> entries       = entries;
        g -> entriesfilled = 0;
        g -> usedEntries   = 0;
        g -> itemindexes   = (int *) xalloc (entries * sizeof (int));
        g -> itemnames     = (ident *) xalloc (entries * sizeof (ident));

        g -> next = dll -> groups;
        dll -> groups = g;
    } else {
        ASSERT (g -> hash == hash);

        if (g -> entriesfilled + entries > g -> entries) {
            g -> itemindexes = (int *) xrealloc (g -> itemindexes,
                                                 g -> entries * sizeof (int),
                                                (g -> entriesfilled + entries) * sizeof (int));
            g -> itemnames = (ident *) xrealloc (g -> itemnames,
                                                 g -> entries * sizeof (ident),
                                                (g -> entriesfilled + entries) * sizeof (ident));
            g -> entries = g -> entriesfilled + entries;
        }
    }
    return g;
}

void NewJImportItem  (struct JImportGroup * g, int itemIndex, ident intname, int type)
{
    ASSERT (itemIndex >= 0);
    ASSERT ((g -> entriesfilled) < (g -> entries));
    ASSERT ((type == 0) || (type == K_IMPFUNC));

    nameInfo * info = (nameInfo *) (NAMES.getInfo (intname));
    if (info != NULL) {
        if (info->kind & K_JIMPORT) {
            jimportNameInfo *jiinfo = (jimportNameInfo *)info;
            if (((jiinfo->kind & type) == type) && (jiinfo->group == g)) {
                for (int i = 0; i < g->entriesfilled; i++) {
                    if (g->itemindexes [i] == itemIndex) {
                        if (g->itemnames [i] == intname)
                           return; // redeclaration
                        else
                           break;
                    }
                }
            }
        }
        return; // local symbol declared with the same name, 
                // ignore import
    }

    g -> itemindexes [g -> entriesfilled] = itemIndex;
    g -> itemnames   [g -> entriesfilled] = intname;

    jimportNameInfo *jiinfo = new jimportNameInfo (K_JIMPORT | type, g);
    NAMES.setInfo (intname, jiinfo);

    if (type == K_IMPFUNC)
        VerboseMessage (INFO_SYMBOLDECLARATION, "SYMDECL: New J import: %s (%d from %s), code\n", NAMES.Index2Str(intname), itemIndex, NAMES.Index2Str(g->groupname));
    else
        VerboseMessage (INFO_SYMBOLDECLARATION, "SYMDECL: New J import: %s (%d from %s), data\n", NAMES.Index2Str(intname), itemIndex, NAMES.Index2Str(g->groupname));

    g -> entriesfilled++;
}

Storage * JImportImage = NULL;

#define JI_DLL    01
#define JI_GROUP  02
#define JI_END    03


void FormJImportImage (void)
{
    JImportImage = newStorage (65536);

    JImportImage->Put4 (JI_MAGIC);

    for (JImportDLL * d = JImportDLLs; d; d = d -> next) {

        JImportImage -> Put4 (JI_DLL);
        JImportImage -> Put4 (0);

        dword dllInfoSizeOffs = JImportImage -> Index;
        JImportImage -> Put4 (0);

        JImportImage -> PutAlignedName (NAMES.Index2Str (d -> name), NAMES.Index2StrLen (d -> name) + 1);
        JImportImage -> Put4 (d -> cKey);

        if (d -> used) {
            for (JImportGroup * g = d -> groups; g; g = g -> next) {
                if (g -> usedEntries > 0) {
                    JImportImage -> Put4 (JI_GROUP);
                    JImportImage -> Put4 (g -> hash);

                    JImportImage -> PutEncryptedAlignedName (NAMES.Index2Str (g -> groupname), NAMES.Index2StrLen (g -> groupname) + 1, IMPORT_EXPORT_ENCRYPTION_KEY);
                    
                    JImportImage -> Put4 (g -> usedEntries);
                    for (int k = 0; k < g -> entriesfilled; k++) {
                        jimportNameInfo * jinfo = (jimportNameInfo *) (NAMES.getInfo (g -> itemnames[k]));
                        ASSERT (jinfo && ((jinfo -> kind & K_MASK) == K_JIMPORT));
                        if (jinfo -> kind & K_USED) {
                            JImportImage -> Put4 (g -> itemindexes [k]);
                            jinfo -> jidataOffset = JImportImage -> Index;
                            if (! (jinfo -> kind & K_IMPFUNC) )
                                jinfo -> offset = jinfo->jidataOffset + JImportStart;
                            else
                                ASSERT (( ((dword) jinfo -> offset) >= ImportStart) && (((dword) jinfo -> offset) <= (CodeStart+CodeLen)));
                            JImportImage -> Put4 (0);
                        }
                    }
                }
            }
        } else {
            VerboseMessage (INFO_UNUSEDDLLIMPORT, "[UNUSED DLL IMPORT] %s\n", NAMES.Index2Str(d->name));
        }

        JImportImage -> Set4 (dllInfoSizeOffs, JImportImage->Index - dllInfoSizeOffs);
    }
    JImportImage -> Put4 (JI_END);
}


void PatchJImportJumpTable (void) {
    for (JImportDLL * d = JImportDLLs; d; d = d -> next) {
        if (d -> used) {
            for (JImportGroup * g = d -> groups; g; g = g -> next) {
                for (int k = 0; k < g -> entriesfilled; k++) {
                    jimportNameInfo * jinfo = (jimportNameInfo *) (NAMES.getInfo (g -> itemnames[k]));
                    ASSERT (jinfo && ((jinfo -> kind & K_MASK) == K_JIMPORT));
                    if ((jinfo -> kind & (K_USED | K_IMPFUNC)) == (K_USED | K_IMPFUNC)) {
                        ASSERT ((((dword) jinfo -> offset) >= ImportStart) && (((dword) jinfo -> offset) <= (CodeStart+CodeLen)));
                        ASSERT (jinfo -> jidataOffset != (dword) (-1));
                        dword jump_offset = jinfo -> offset - CodeStart;
                        *(( word *) (CodePtr + jump_offset    )) = JUMP_CODE;
                        *((dword *) (CodePtr + jump_offset + 2)) = jinfo->jidataOffset + JImportStart;
                    }
                }
            }
        }
    }
}


/*----------------------------------------------------------------------------*/
/*                         COMPONENT KEY                                      */
/*----------------------------------------------------------------------------*/

#define MaxComponentKeyLen    1024
#define ComponentKeyMagic     0x4B435546       /* "FUCK" */

void PrepareComponentKey (void)
{
    if (xJetComponent && xDLLFlag && (NExports == 0)) {
        // Create Component Key Export (debugger requires export from dll)
        new OBJFile ("COMPONENT KEY");
        Segment * key_seg = new Segment (COMPONENT_KEY, COMPONENT_KEY, DATA, false, sizeof (int));
        * ((int *) (key_seg -> getText())) = ComponentKeyMagic;
        NewPublicName (COMPONENT_KEY, key_seg, 0, T_DATA);

        Export * exp = FindExportByExtName (COMPONENT_KEY);
        if (!exp) {
            exp = NewExport (COMPONENT_KEY, COMPONENT_KEY, NULL, 0, 0, EFLAG_SOURCE_SYNTHETIC, INVALID_ID);
        } else {
            ASSERT (exp->seg     == NULL);
            ASSERT (exp->intname == COMPONENT_KEY);
        }
        exp -> flag |= (EFLAG_SOURCE_SYNTHETIC | EFLAG_NAME_EXPORTED | EFLAG_NOIMPLIB);
    }
}

/*----------------------------------------------------------------------------*/
/*                              STORE IMAGE BASE                              */
/*----------------------------------------------------------------------------*/

void StoreImageBase (void)
{
    if (xJetComponent && (xIMAGE_FORMAT == xELF_IMAGE_FORMAT) && !xDLLFlag) {
        new OBJFile ("IMAGE BASE");

        ident ImageBaseID = NAMES.Str2Index("LINK_ImageBase");
        Segment * seg = new Segment (ImageBaseID, ImageBaseID, DATA, false, sizeof (dword));
        * ((dword *) (seg -> getText())) = xImageBase;
        NewPublicName (ImageBaseID, seg, 0, T_DATA);
    }
}

/*----------------------------------------------------------------------------*/
/*                     Control Parameter Block (CPB)                          */
/*----------------------------------------------------------------------------*/

Storage * CPB = NULL;
static int CPB_nParams = 0;

void EmitCPBOption (int optionIndex)
{
    if (!CPB) {
        CPB = newStorage (1024);
        struct CPBHeader header;
        memset (&header, 0, sizeof(header));
        header.magic      = CPB_MAGIC;
        header.CPBVersion = CPB_VERSION;
        header.linkerDataVersion = LINKER_GENERATED_DATA_VERSION;
        CPB->PutS (&header, sizeof(header));
    }
    CPB->Put4 (CPBKeys [optionIndex].keyIndex);
    CPB_nParams++;
}

void NewCPBOption (int optionIndex, dword  value)
{
    ASSERT (CPBKeys [optionIndex].optionType == CPB_KEY_TYPE_CARD32);
    EmitCPBOption (optionIndex);
    CPB->Put4 (sizeof (dword));
    CPB->Put4 (value);
}

void NewCPBOption (int optionIndex, char * value)
{
    ASSERT (CPBKeys [optionIndex].optionType == CPB_KEY_TYPE_STRING);
    EmitCPBOption (optionIndex);
    CPB->PutAlignedName (value, strlen (value) + 1);
}

void NewEncryptedCPBOption (int optionIndex, char * value) 
{
    if (xEncryptStrings == ENCRYPTION_KEY_UNDEFINED_VALUE) {
        Message(xFATAL, msgOPTION_REQUIRED_BEFORE_CONFIG, "-zimer=<value>", CPBKeys [optionIndex].keyName);
        return;
    }

    ASSERT (CPBKeys [optionIndex].optionType == CPB_KEY_TYPE_ENCRYPTEDSTRING);
    EmitCPBOption (optionIndex);
    CPB->PutEncryptedAlignedName (value, strlen (value) + 1, (byte) xEncryptStrings);
}

void FormCPB (void)
{
    if (CPB) {
        struct CPBHeader * header = (struct CPBHeader *) CPB->Ptr;

        ASSERT (VerStamp != NULL);
        header->jetVerMajor = VerStamp->getMajorCompilerVersion();
        header->jetVerMinor = VerStamp->getMinorCompilerVersion();
        header->jetEdition  = VerStamp->getEdition();
        header->vcode       = xVCode;
        header->nParams     = CPB_nParams;
        header->encryptionKey = (xEncryptStrings == ENCRYPTION_KEY_UNDEFINED_VALUE) ? ENCRYPTION_KEY_NO_ENCRYPTION_VALUE : xEncryptStrings;
    }
}

/*----------------------------------------------------------------------------*/
/*                                 LINK Info                                  */
/*----------------------------------------------------------------------------*/

void writeCollections (FILE * f, char * secName, struct collection * coll)
{
    fprintf (f, "@Section %s\n", secName);
    for (;coll != NULL; coll = coll -> next) {
        for (Segment * s = coll->segs; s; s = s -> link) {
            if ((s -> isProcessed ()) &&
                (s -> address != (dword)-1) &&
                (s -> getLen() != 0))
            {
                ASSERT (s -> file != NULL);
                fprintf (f, " %10X %4X \"%s\"#\"%s\"\n", s->address, s->getLen(), s->file->getUIDName(), NAMES.Index2Str(s->getKeyName()));
            }
        }
    }
    fprintf (f, "@EndSection\n");
}


void WriteLinkInfo ()
{
    const char * fname = xWriteLinkInfoFile ? 
                   (HasExtension  (xWriteLinkInfoFile) ? xWriteLinkInfoFile :
                                                         MakeExtension (xWriteLinkInfoFile, "li")) :
                   MakeExtension (xOutputFileName,    "li");

    FILE * f = fopen (fname, "w");
    fprintf (f, "@ImageBase %X\n", xImageBase);
    fprintf (f, "@Segments\n");

    writeCollections (f, "CODE",   code);
    writeCollections (f, "CODE16", code16);
    writeCollections (f, "DATA",   data);
    writeCollections (f, "IDATA",  idata);
    writeCollections (f, "BSS",    bss);
    writeCollections (f, "RDATA",  rdata);

    fprintf (f, "@EndSegments\n");
    fclose (f);
}


Segment * findSegment (char * uidname, char * _keyName)
{
    OBJFile * file = FileList->findFileByUIDName (uidname);
    if (file == NULL)
        return NULL;

    ident keyName = NAMES.Str2Index (_keyName);

    for (Segment * s = file -> segs; s; s = s -> next) {
        if (s -> getKeyName () == keyName) {
                return s;
        }
    }
    return NULL;
}


struct collection * allocateCollection (ident name)
{
    struct collection * coll = (struct collection *) allocateForever (sizeof (struct collection));
    coll -> base_vadr = 0;
    coll -> name      = name;
    coll -> segs      = NULL;
    coll -> last      = NULL;
    coll -> next      = NULL;
    return coll;
}


void ReadSegmentsFromSection (TextFileParser * parser, struct collection ** coll, ident name)
{
    for (;;) {
        parser -> readLine ();

        ASSERT (! parser -> eof ());

        parser -> skipSpaces ();

        char * directive = parser -> peekToken ();
        if ((directive != NULL) && (directive [0] == '@')) {
            parser -> expect ("@EndSection");
            return;
        }

        dword va = parser -> getHex ();

        parser -> skipSpaces ();
        dword len = parser -> getHex ();

        char filename [16384];
        parser -> skipSpaces ();
        char * _filename = parser -> getQuoted (filename, sizeof (filename));
        ASSERT (_filename != NULL);

        parser -> expect ("#");

        char segKeyName [16384];
        parser -> skipSpaces ();
        char * _segKeyName = parser -> getQuoted (segKeyName, sizeof(segKeyName));
        ASSERT (_segKeyName != NULL);

        VerboseMessage (INFO_READLINKINFO, " %10X %4X \"%s\"#\"%s\"\n", va, len, filename, segKeyName);

        Segment * s = findSegment (filename, segKeyName);

        if ((s != NULL) && s -> isProcessed() && !s -> isCollected()) {
            if (s->clazz != name) {
                VerboseMessage (INFO_READLINKINFO, "[LINK MAP ORDER REJECTED] link map defined clazz = %s, actual clazz = %s\n", NAMES.Index2Str(name), NAMES.Index2Str(s->clazz));
            } else {
                if ( (*coll) == NULL)
                    (*coll) = allocateCollection (name);

                s -> link = NULL;

                if ((*coll) -> segs) {
                    ASSERT ( (*coll) -> last != NULL);
                    (*coll) -> last -> link = s;
                    (*coll) -> last = s;
                } else {
                    (*coll) -> segs = (*coll) -> last = s;
                }
                s -> collection = (*coll);

                s -> markAsCollected ();
            }
        }
    }
}

void ReadSegments (TextFileParser * parser)
{
    for (;;) {
        parser -> readLine ();

        ASSERT (!parser -> eof ());

        char * directive = parser -> peekToken ();
        ASSERT (directive != NULL);

        if (!strcmp (directive, "@Section")) {
            parser -> getToken ();
            char * secname = parser -> getToken ();
            VerboseMessage (INFO_READLINKINFO, "@Section %s\n", secname);
            if (!strcmp (secname, "CODE"))
                ReadSegmentsFromSection (parser, &code, CODE);
            else if (!strcmp (secname, "CODE16"))
                ReadSegmentsFromSection (parser, &code16, CODE);
            else if (!strcmp (secname, "DATA"))
                ReadSegmentsFromSection (parser, &data, DATA);
            else if (!strcmp (secname, "IDATA"))
                ReadSegmentsFromSection (parser, &idata, IDATA);
            else if (!strcmp (secname, "BSS"))
                ReadSegmentsFromSection (parser, &bss, BSS);
            else if (!strcmp (secname, "RDATA"))
                ReadSegmentsFromSection (parser, &rdata, CONST);
            else {
                VerboseMessage ("Unknown section: %s\n", secname);
                ASSERT_FALSE ();
            }
        } else if (!strcmp (directive, "@EndSegments")) {
            return;
        } else {
            VerboseMessage ("Unknown directive: %s\n", directive);
            ASSERT_FALSE ();
        }
    }
}


void ReadLinkInfo ()
{
    ASSERT (code   == NULL);
    ASSERT (code16 == NULL);
    ASSERT (data   == NULL);
    ASSERT (idata  == NULL);
    ASSERT (bss    == NULL);
    ASSERT (rdata  == NULL);

    const char * fname = xReadLinkInfoFile ? 
                   (HasExtension  (xReadLinkInfoFile) ? xReadLinkInfoFile :
                                                         MakeExtension (xReadLinkInfoFile, "li")) :
                   MakeExtension (xOutputFileName,    "li");

    TextFileParser * parser = new TextFileParser (fname);

    for (;;) {
        parser -> readLine ();

        if (parser -> eof ())
            break;

        parser -> skipSpaces ();

        char * directive = parser -> peekToken ();

        if (!strcmp (directive, "@Segments")) {
            parser -> getToken ();
            VerboseMessage (INFO_READLINKINFO, "@Segments\n");
            ReadSegments (parser);
        } else if (!strcmp (directive, "@ImageBase")) {
            parser -> getToken ();
            VerboseMessage (INFO_READLINKINFO, "@ImageBase %X\n", parser -> getHex());
        } else {
            VerboseMessage ("Unknown directive: %s\n", directive);
            ASSERT_FALSE ();
        }
    }

    delete parser;

    if (xDoDebug) {
        Message (xWARNING, msgUNABLE_DEBUG_INFO_IN_REORD);
        xDoDebug = false;
    }
}

/*----------------------------------------------------------------------------*/
/*                        Null-Check Information                              */
/*----------------------------------------------------------------------------*/

#define NO_NULL_CHECK_INFO 0xFFFFFFFF

/*

  Segment with clazz = NULLCHECKS should have the following layout:
  -----------------------------------------------------------------

   RECORD
     codeAddr   :4 bytes -- here fixup should be (kind = FIXUP_ADDRESS32) to code
     length     :4 bytes -- length of code
     nChecks    :4 bytes -- number of records describing null-checks
     nullChecks :ARRAY nChecks OF RECORD
                   codeOffs   :4 bytes -- offset relative to codeAddr
                   accessOffs :2 bytes -- offset, which will be accessed
                 END;
   END;

  Linker should form Null-Checks Information with following layout:
  -----------------------------------------------------------------

   RECORD
     nCodeSegs :4 bytes -- number of code segments OR NO_NULL_CHECK_INFO
     codeSegs  :ARRAY nCodeSegs OF RECORD
                  codeAddr       :4 bytes
                  length         :4 bytes
                  nChecks        :4 bytes -- number of records describing null-checks
                  nullChecksOffs :4 bytes -- offset relative to null-checks table,
                                          -- pointing to array of nullChecks for given segment
                END;
     -- here arrays of nullChecks are follow, with the same layout as in segments
   END;

  Linker should create symbol LINK_NullChecksTable pointing to the beginning of
  null-checks information.

*/

class NullChecksSegment;

static dword NSegments = 0;
static NullChecksSegment * NullChecks = NULL;

class NullChecksSegment {
  private:
    Segment * seg;
    NullChecksSegment * next;

    NullChecksSegment (Segment * s) {
        seg        = s;
        next       = NullChecks;
        NullChecks = this;

        NSegments++;
    }

    friend void PrepareNullCheckInformation (void);
};

void PrepareNullCheckInformation (void)
{
    for (OBJFile * f = FileList; f; f = f->next) {
        for (Segment * s = f -> segs; s; s = s->next) {
            if (s -> getClazz () == NULLCHECKS) {
                // found segment with null-checks information
                new NullChecksSegment (s);
            }
        }
    }

    if ((NSegments == 0) && !xJetComponent)
        return;

    Storage * NCI = newStorage (16384);

    if (!xEmitNullCheckInfo) {
        VerboseMessage (INFO_NULLCHECKS, "Null Checks: OFF\n");
        NCI -> Put4 (NO_NULL_CHECK_INFO);
        NSegments = 0;
    } else {
        NCI -> Put4 (NSegments);
    }

    struct fixup ** fixups = (struct fixup **) xalloc (NSegments * sizeof(struct fixup *));

    VerboseMessage (INFO_NULLCHECKS, "Null Checks: %d segments\n", NSegments);

    if (NSegments > 0) {
        NCI -> ZeroBytes (NSegments * 16); // reserve space for segments directory

        NullChecksSegment * ncs = NullChecks;
        for (dword i = 0; i < NSegments; i++) {
            ASSERT (ncs != NULL);
            Segment * seg = ncs -> seg;

            // get information from one null-checks segment

            ASSERT (seg -> nfixups == 1);
            fixups [i] = & (seg -> fixups [0]);
            ASSERT (fixups [i]->kind == FIXUP_ADDRESS32);

            byte* segText = seg -> getText();
            dword nChecks = *((dword *) (segText + 8));

            NCI -> Set4 (4 + i*16 + 0,  *((dword *) (segText + 0))); // codeAddr
            NCI -> Set4 (4 + i*16 + 4,  *((dword *) (segText + 4))); // codeLength
            NCI -> Set4 (4 + i*16 + 8,  nChecks);
            NCI -> Set4 (4 + i*16 + 12, NCI -> GetPos ());               // nullChecksOffs

            for (dword j = 0; j < nChecks; j++) {
                NCI -> Put4 ( *((dword *) (segText + 12 + j*6 + 0)) ); // codeOffs
                NCI -> Put2 ( *(( word *) (segText + 12 + j*6 + 4)) ); // accessOffs
            }

            // free NullChecksSegment
            NullChecksSegment * toRemove = ncs;
            ncs = ncs -> next;
            delete toRemove;
        }
    }

    new OBJFile ("NULL CHECKS");

    ident NullChecksTableID = NAMES.Str2Index ("LINK_NullChecksTable");
    Segment * null_checks_seg = new Segment (NullChecksTableID, NullChecksTableID, DATA, false, NCI->GetLen());
    NewPublicName (NullChecksTableID, null_checks_seg, 0, T_DATA);

    null_checks_seg -> allocateFixups (NSegments);

    for (dword i = 0; i < NSegments; i++) {
        addFixup (null_checks_seg,
                  FIXUP_ADDRESS32,
                  4 + 16*i,
                  fixups [i] -> k_target,
                  fixups [i] -> target,
                  fixups [i] -> fx_offset);
    }
    null_checks_seg -> nfixups = NSegments;

    VerboseMessage (INFO_NULLCHECKS, "Null Checks Segment: %d bytes\n", NCI->GetLen());

    memcpy (null_checks_seg->getText(), NCI->GetData(), NCI->GetLen());
    delete NCI;
}


/*----------------------------------------------------------------------------*/
/*                       Stack Trace Information                              */
/*----------------------------------------------------------------------------*/

#define NO_STACK_TRACE_INFO 0xFFFFFFFF

/*

  Segment with clazz = STACKTRACE should have the following layout:
  -----------------------------------------------------------------

   RECORD
     codeAddr   :4 bytes -- here fixup should be (kind = FIXUP_ADDRESS32) to code
     length     :4 bytes -- length of code
     excSites   :4 bytes -- fixup (kind = FIXUP_ADDRESS32) to the per-method exception sites info
   END;

  Linker should form Stack Trace Info Table with following layout:
  -----------------------------------------------------------------

  STTableEntry_STR = RECORD
    addr     :ADDRESS;  -- CONSTADDR
    length   :CARD32;
    excSites :ADDRESS;  -- CONSTADDR
  END;

  STTable_STR = RECORD
    nMethods :CARD32;  -- number of methods with stack trace info
    methods  :ARRAY nMethods OF STTableEntry_STR;
  END;

  Linker should create symbol LINK_StackTraceInfoTable pointing to the
  beginning of stack trace information table.

*/

struct STTableEntry_STR {
    void* addr;
    dword length;
    void* excSites;
};


static dword N_STSegments = 0;
static GrowingArray<Segment *> StackTraceSegs = GrowingArray<Segment *>(0, 32);
static Segment* StackTraceInfoTableSeg = NULL;

void PrepareStackTraceInformation (void)
{
    if (xEmitStackTraceInfo) {
        for (OBJFile * f = FileList; f; f = f->next) {
            for (Segment * s = f -> segs; s; s = s->next) {
                if (s -> getClazz () == STACKTRACE) {
                    // found segment with stack trace information
                    StackTraceSegs.addElement (s);
                    N_STSegments++;
                }
            }
        }
    }

    if ((N_STSegments == 0) && !xJetComponent)
        return;

    if (!xEmitStackTraceInfo) {
        VerboseMessage ("Stack Trace Info: OFF\n");
        ASSERT (N_STSegments == 0);
    } else {
        VerboseMessage ("Stack Trace Info: %d segments\n", N_STSegments);
    }

    new OBJFile ("STACK TRACE INFO");

    ident StackTraceInfoTableID = NAMES.Str2Index ("LINK_StackTraceInfoTable");
    StackTraceInfoTableSeg = new Segment (StackTraceInfoTableID, StackTraceInfoTableID, CONST, false, 4 + N_STSegments*sizeof(struct STTableEntry_STR));
    NewPublicName (StackTraceInfoTableID, StackTraceInfoTableSeg, 0, T_DATA);

    StackTraceInfoTableSeg -> allocateFixups (N_STSegments*2);

    if (!xEmitStackTraceInfo) {
        * ((dword *) (StackTraceInfoTableSeg -> getText())) = NO_STACK_TRACE_INFO;
    } else {
        * ((dword *) (StackTraceInfoTableSeg -> getText())) = N_STSegments;
    }

    for (dword i = 0; i < N_STSegments; i++) {
        // get information from one stack trace segment
        Segment * seg = StackTraceSegs[i];
        ASSERT (seg -> getLen() >= sizeof(struct STTableEntry_STR));

        memcpy (StackTraceInfoTableSeg -> getText() + 4 + i*sizeof(struct STTableEntry_STR),
                seg -> getText(),
                sizeof(struct STTableEntry_STR));

        ASSERT (seg -> nfixups == 2);
        struct fixup * codeAddrFixup = NULL;
        struct fixup * excSitesFixup = NULL;

        if (seg->fixups[0].offset == 0) {
            ASSERT (seg->fixups[1].offset == 8);
            codeAddrFixup = &(seg->fixups[0]);
            excSitesFixup = &(seg->fixups[1]);
        } else {
            ASSERT (seg->fixups[1].offset == 0);
            ASSERT (seg->fixups[0].offset == 8);
            codeAddrFixup = &(seg->fixups[1]);
            excSitesFixup = &(seg->fixups[0]);
        }

        // codeAddr
        addFixup (StackTraceInfoTableSeg,
                  FIXUP_CONSTADDRESS32,
                  4 + 12*i,
                  codeAddrFixup -> k_target,
                  codeAddrFixup -> target,
                  codeAddrFixup -> fx_offset);

        // excSites
        addFixup (StackTraceInfoTableSeg,
                  FIXUP_CONSTADDRESS32,
                  4 + 12*i + 8,
                  excSitesFixup -> k_target,
                  excSitesFixup -> target,
                  excSitesFixup -> fx_offset);
    }
}


static int XCDECL compare_stack_trace_table_entries (const void * p1, const void * p2)
{
    const struct STTableEntry_STR * e1 = (const struct STTableEntry_STR *) p1;
    const struct STTableEntry_STR * e2 = (const struct STTableEntry_STR *) p2;

    ASSERT (e1->addr != NULL);
    ASSERT (e2->addr != NULL);

    if ((dword)(e1->addr) < (dword)(e2->addr))
        return -1;
    if ((dword)(e1->addr) > (dword)(e2->addr))
        return 1;

    return 0;
}


void SortStackTraceInfoTable ()
{
    if (StackTraceInfoTableSeg != NULL) {
        ASSERT (StackTraceInfoTableSeg->address >= RDataStart);
        ASSERT (StackTraceInfoTableSeg->address <  RDataStart + RDataLen);

        dword offset = (StackTraceInfoTableSeg->address - RDataStart);

        ASSERT (offset + 4 + N_STSegments*sizeof(struct STTableEntry_STR) <= RDataLen);
        qsort (RDataPtr + offset + 4, N_STSegments, sizeof(struct STTableEntry_STR), compare_stack_trace_table_entries);
    }
}

/*----------------------------------------------------------------------------*/
/*                      Extra thunks for JET method test                      */
/*----------------------------------------------------------------------------*/

#define THUNK_LEN           8
#define THUNK_SYMBOL_OFFS   1
#define THUNK_TARGET_OFFS   3

static const byte THUNK_CODE[THUNK_LEN] =
             {0x90, 0x90, 0xE9, 0, 0, 0, 0, 0x90}; // nop; nop; jump addr; nop

struct ExtraThunkEntry {
    byte k_target;
    void* target;
    ident thunkId;
};

static struct ExtraThunkEntry * ExtraThunkEntries = 0;
static int nExtraThunkEntries = 0;
static int ExtraThunkTableSize = 0;

ident newExtraThunk (byte k_target, void* target) {
    for (int i = 0; i < nExtraThunkEntries; i++) {
        if ((ExtraThunkEntries[i].k_target == k_target) &&
            (ExtraThunkEntries[i].target   == target))
        {
            return ExtraThunkEntries[i].thunkId;
        }
    }
    struct ExtraThunkEntry * new_thunk;
    Grow (nExtraThunkEntries, ExtraThunkTableSize, 32, ExtraThunkEntries, struct ExtraThunkEntry, new_thunk);

    char name[256];
    sprintf (name, "$$$ extra thunk #%d", nExtraThunkEntries);
    ident thunkId = NAMES.Str2Index (name);

    new_thunk->k_target = k_target;
    new_thunk->target   = target;
    new_thunk->thunkId  = thunkId;

    return thunkId;
}


void generateExtraThunks () {
    if (nExtraThunkEntries == 0)
        return;

    new OBJFile ("EXTRA THUNKS");

    for (int i = 0; i < nExtraThunkEntries; i++) {
        Segment * thunk_seg = new Segment (ExtraThunkEntries[i].thunkId, NAMES.Str2Index("THUNK_CODE"), CODE, false, THUNK_LEN, 4);
        memcpy (thunk_seg -> getText(), THUNK_CODE, THUNK_LEN);

        addFixup (thunk_seg,
                  FIXUP_SELFRELOFFS32,
                  THUNK_TARGET_OFFS,
                  ExtraThunkEntries[i].k_target,
                  ExtraThunkEntries[i].target,
                  0);

        NewPublicName (ExtraThunkEntries[i].thunkId, thunk_seg, THUNK_SYMBOL_OFFS, T_CODE);
        VerboseMessage ("Extra thunk created: %s\n", NAMES.Index2Str(ExtraThunkEntries[i].thunkId));
    }
}

/*----------------------------------------------------------------------------*/
/*                   Linking of files as data/rdata segments                  */
/*----------------------------------------------------------------------------*/

void LinkFileIntoSegment (ident segmentKind,
                          ident symbol,
                          const char* filename)
{
    OSFile * file = OS -> File ();

    byte * rawdata = NULL;
    unsigned long size = 0;

    if (! file -> OpenRead (filename, rawdata, size))
        return;

    dword len = (dword)size;

    new OBJFile (filename);

    Segment * seg = new Segment (symbol, symbol, segmentKind, false, len + 4, 4);

    *((dword *) (seg -> getText())) = len;
    memcpy (seg -> getText() + 4, rawdata, len);

    NewPublicName (symbol, seg, 0, T_DATA);

    VerboseMessage ("File \"%s\" (len %d) is linked as symbol \"%s\" into %s\n", filename, len, NAMES.Index2Str(symbol), NAMES.Index2Str(segmentKind));

    delete file;
}

/*----------------------------------------------------------------------------*/

close_namespace

