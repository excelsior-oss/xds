#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "xdefs.h"

open_namespace

#include "struct.h"
#include "idents.h"
#include "messages.h"
#include "xdebug.h"
#include "omf.h"
#include "implib.h"
#include "xmem.h"
#include "writer.h"


/*----------------------------------------------------------------------------*/
/*                                                                            */
/*                      Form export section                                   */
/*                                                                            */
/*----------------------------------------------------------------------------*/

#define LibPageSize 0x10

static Storage * LibImage;
static int LEntries = 0;

/*----------------------------------------------------------------------------*/

void AddImportLibItem(const char *ImpName,
                      const char *ModName, word ordinal)
{
    word HeaderLen, comlen; 
    dword     modlen, implen;

    modlen = strlen(ModName);
    implen = strlen(ImpName);
/*
    if (xImplib_XOMF) {
      dword theadr_name_len = implen;
      ASSERT(modlen < 0x100);
      LibImage->PutB(XOMF_THEADR);
      if (implen > 255)
         theadr_name_len = 255;
      HeaderLen = (word) (1 + theadr_name_len + 1);
      LibImage->Put2(HeaderLen);
      LibImage->PutB(theadr_name_len);
      LibImage->PutS(ImpName, theadr_name_len);
      LibImage->PutB(0x00);
      
      LibImage->PutB(COMENT);
      comlen = (word) (4 + 2 + implen + 1 + modlen + ((xUseOrdFlag && ordinal) ? 2 : 2 + implen) + 1);
      LibImage->Put2(comlen);
      LibImage->PutB(0x00);
      LibImage->PutB(0xA0);
      LibImage->PutB(IMPDEF);
      LibImage->PutB(((xUseOrdFlag && ordinal) ? 0x01 : 0x00));
      LibImage->Put2(implen);
      LibImage->PutS(ImpName, implen);
      LibImage->PutB(modlen);
      LibImage->PutS(ModName, modlen);
      if (xUseOrdFlag && ordinal)
          LibImage->Put2(ordinal);
      else{
          LibImage->Put2( implen);
          LibImage->PutS( ImpName, implen);
      }
      LibImage->PutB(0x00);

    } else {
*/
      ASSERT(modlen < 0x100);
      ASSERT(implen < 0x100);
      LibImage->PutB(THEADR);
      HeaderLen = (word) (1 + implen + 1);
      LibImage->Put2(HeaderLen);
      LibImage->PutB(implen);
      LibImage->PutS(ImpName, implen);
      LibImage->PutB(0x00);
    
      LibImage->PutB(COMENT);
      comlen = (word) (4 + 1 + implen + 1 + modlen + ((xUseOrdFlag && ordinal) ? 2 : 1 + implen) + 1);
      LibImage->Put2(comlen);
      LibImage->PutB(0x00);
      LibImage->PutB(0xA0);
      LibImage->PutB(IMPDEF);
      LibImage->PutB(((xUseOrdFlag && ordinal) ? 0x01 : 0x00));
      LibImage->PutB(implen);
      LibImage->PutS(ImpName, implen);
      LibImage->PutB(modlen);
      LibImage->PutS(ModName, modlen);
      if (xUseOrdFlag && ordinal)
          LibImage->Put2(ordinal);
      else{
          LibImage->PutB(implen);
          LibImage->PutS(ImpName, implen);
      }
      LibImage->PutB(0x00);

    LibImage->PutB(MODEND);
    LibImage->Put2(0x02);
    LibImage->PutB(0x00);
    LibImage->PutB(0x00);
    while(LibImage->Index % LibPageSize) LibImage->PutB(0x00);
    LEntries++;
}

void StartImpLib(){
        LibImage = new Storage(xFileAlign);
/*
        if (xImplib_XOMF)
          LibImage->PutB(XOMF_LIBHDR);
        else
*/
          LibImage->PutB(LIBHDR);
        LibImage->Put2(LibPageSize - 3);
        LibImage->ZeroBytes(0x10 - 3);
        LEntries = 0;
}

void EndImpLib(){
    int len = 0;
    LibImage->PutB(LIBEND);
    if ((LibImage->Index+2) % LibPageSize != 0)
        len = LibPageSize - ((LibImage->Index+2) % LibPageSize);
    if ((LibImage->Index+2+len) % xFileAlign != 0)
        len += xFileAlign - ((LibImage->Index+2+len) % xFileAlign);
    LibImage->Put2(len);
    LibImage->ZeroBytes(len);
    *((dword*)(LibImage->Ptr + 3)) = LibImage->Index;
    *(( word*)(LibImage->Ptr + 7)) = 0x0;
    *(( word*)(LibImage->Ptr + 9)) = 0x1;
}

void WriteLibFile(const char *name){
        FILE *fp;

        fp = fopen (name, "wb");
        if (fp == NULL)
                Message(xFATAL, msgUNABLE_TO_OPEN_FILE, name);
        if (fwrite (LibImage->Ptr, LibImage->Index, 1, fp) != 1) {
                fclose (fp);
                Message(xFATAL, msgUNABLE_TO_WRITE_FILE, name);
        }
        if (fclose (fp))
                Message(xFATAL, msgUNABLE_TO_WRITE_FILE, name);
}

void WriteImpLib()
{
   StartImpLib();
   if (!StrippedOutputFileName) {
        StrippedOutputFileName = strrchr (xOutputFileName, '\\');
        if (!StrippedOutputFileName)
            StrippedOutputFileName = strrchr (xOutputFileName, '/');
        if (!StrippedOutputFileName)
            StrippedOutputFileName = xOutputFileName;
        else
            StrippedOutputFileName++;
   }
   for (int n = 0; n < ILNumberOfExports; n ++) {
      Export * s = ILExportsTable[n];
      AddImportLibItem(NAMES.Index2Str (s -> extname), StrippedOutputFileName, s -> ordinal);
   }
   EndImpLib();
   WriteLibFile(xOutputLibName);
   xfree(ILExportsTable);
}



///// Import library for GCC ///////////////////////////////////////////


struct Symbol {
    Symbol(Symbol *next_, ident name_, Bool func_)
        : next(next_), name(name_), func(func_)
    {
    }

    Symbol *next;

    const ident name;
    const Bool func;
};


struct VersionEntry {
    VersionEntry(VersionEntry *next_, ident version_)
        : next(next_), sym(NULL), version(version_)
    {
    }
    
    VersionEntry *next;
    Symbol *sym;

    const ident version;
};


static VersionEntry *versionEntries = NULL;


static VersionEntry *findVersionEntry(ident version) {
    VersionEntry *verEntry = versionEntries;

    while(verEntry != NULL && verEntry->version != version)
        verEntry = verEntry->next;

    // entry not found -- create a fresh one
    if(verEntry == NULL) {
        verEntry = new VersionEntry(versionEntries, version);
        versionEntries = verEntry;
    }

    return verEntry;
}


static void CollectVersionedSymbols() {
    for (int i = 0; i < NumberOfImportedNames; i++) {
        importNameInfo * info = (importNameInfo *) (NAMES.getInfo (ImportedNames [i]));

        if (info->getVersion() != INVALID_ID) {
            VersionEntry *verEntry = findVersionEntry(info->getVersion());

            Bool isFunc = info->kind & K_IMPFUNC;

            // add a new symbol entry
            verEntry->sym = new Symbol(verEntry->sym, info->getName(), isFunc);
        }
    }
}


static void FreeVersionedSymbols() {
    VersionEntry *verEntry = versionEntries;

    while(verEntry != NULL) {
        Symbol *sym = verEntry->sym;

        while(sym != NULL) {
            Symbol *deadSym = sym;

            sym = sym->next;

            delete deadSym;
        }

        VersionEntry *deadEntry = verEntry;

        verEntry = verEntry->next;

        delete deadEntry;
    }

    versionEntries = NULL;
}


static void WriteVersionedSymbols(const char *libName) {
    char *libNameC = (char *) xalloc(strlen(libName) + 5);
    char *libNameV = (char *) xalloc(strlen(libName) + 5);

    sprintf(libNameC, "%s.c", libName);
    sprintf(libNameV, "%s.ver", libName);

    FILE *outFileC = fopen (libNameC, "w");
    FILE *outFileV = fopen (libNameV, "w");

    if(outFileC == NULL)
        Message(xFATAL, msgUNABLE_TO_OPEN_FILE, libNameC);

    if(outFileV == NULL)
        Message(xFATAL, msgUNABLE_TO_OPEN_FILE, libNameV);

    VersionEntry *verEntry = versionEntries;

    while(verEntry != NULL) {
        const char *version = NAMES.Index2Str(verEntry->version);

        fprintf(outFileV, "%s {\n", version);
        fprintf(outFileC, "// %s\n", version);

        Symbol *sym = verEntry->sym;

        ASSERT(sym != NULL);

        while(sym != NULL) {
            // there usually a symbol with name equal to 
            // the version name; so just skip it
            if(sym->name != verEntry->version) {
                const char *name = NAMES.Index2Str(sym->name);

                fprintf(outFileV, "\t%s;\n", name);
                
                if(sym->func)
                    fprintf(outFileC, "void %s() {}\n", name);
                else
                    fprintf(outFileC, "void* %s;\n", name);
            }

            sym = sym->next;
        }

        fprintf(outFileV, "};\n\n");
        fprintf(outFileC, "\n");

        verEntry = verEntry->next;
    }
    
    fclose (outFileC);
    fclose (outFileV);

    xfree(libNameC);
    xfree(libNameV);
}


void WriteGCCImportLibrary(const char *libName)  {
    CollectVersionedSymbols();

    WriteVersionedSymbols(libName);
    
    FreeVersionedSymbols();
}


close_namespace

