#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "xdefs.h"

open_namespace

#include "struct.h"
#include "idents.h"
#include "messages.h"
#include "debug.h"
#include "xmem.h"

#include "readcoff.h"

/*----------------------------------------------------------------------------*/

#define SYMBOL_BUF_LENGTH 9

/*----------------------------------------------------------------------------*/

class COFFObjectFileReader {
  private:

    Segment ** SegsTable;
    dword SegsSize;

    PIMAGE_SECTION_HEADER Sections;       /* Sections                     */
    dword                 nSections;      /* # of sections in COFF file   */

    PIMAGE_SYMBOL PCOFFSymbolTable;       /* COFF symbol table            */
    dword         COFFSymbolCount;        /* # of symbols in symbol table */

    PSTR stringTable;

    ident * publicNames;
    dword   nPublicNames;

    // get symbol from COFF symbol table, sizeof (buf) >= SYMBOL_BUF_LENGTH
    inline char * Symbol (PIMAGE_SYMBOL pSymbol, char * buf) {
        if (pSymbol -> N.Name.Short != 0) {
            buf [SYMBOL_BUF_LENGTH-1] = '\0';
            return (char *) memcpy (buf, pSymbol -> N.ShortName, 8);
        } else
            return stringTable + pSymbol -> N.Name.Long;
    }

    void ReadSection (byte * rawdata, PIMAGE_SECTION_HEADER s, dword secIndex);

    void SetFileName (char * filename);

    void DeclarePublicName (ident name, Segment * s, int offset);

    void ProcessSymbols (void);

    void ProcessRelocations (PIMAGE_SECTION_HEADER sec,
                             PIMAGE_RELOCATION pRelocs,
                             dword count,
                             Segment * s);

  public:
    COFFObjectFileReader () {
        SegsSize  = 256;
        SegsTable = (Segment **) xalloc (SegsSize * sizeof (Segment *));
        memset (SegsTable, 0, SegsSize * sizeof (Segment *));
    }

    ~COFFObjectFileReader () {
        xfree (SegsTable);
    }

    void Read (byte * rawdata, long size, char * filename, char * libname);

    void ReadLibrary (byte * rawdata, long size, char * libname);
};

/*----------------------------------------------------------------------------*/

void COFFObjectFileReader::ReadSection (byte * rawdata, PIMAGE_SECTION_HEADER s, dword secIndex)
{
    if (s -> Characteristics & IMAGE_SCN_LNK_REMOVE)
        return;

    ident clazz = (s -> Characteristics & IMAGE_SCN_CNT_CODE) ? CODE:
                  (s -> Characteristics & IMAGE_SCN_CNT_UNINITIALIZED_DATA) ? BSS:
                  IsIDATA (s -> Name) ? IDATA : DATA;

    // UNSUPPORTED: long section names
    ident name = NAMES.Str2Index ((char *)(s -> Name), 8);

    if ((name == SYMBOLS_COFF) || (name == TYPES_COFF)) {
        DebugFormat |= DBG_FMT_CV;
        if (!FirstCVModule) FirstCVModule = GetCurrentOBJFileName ();
    }

    // Determine segment's alignment
    dword alignment = 1;
    switch (s -> Characteristics & (IMAGE_SCN_ALIGN_64BYTES | IMAGE_SCN_ALIGN_128BYTES)) {
        case IMAGE_SCN_ALIGN_1BYTES:    alignment = 1;    break;
        case IMAGE_SCN_ALIGN_2BYTES:    alignment = 2;    break;
        case IMAGE_SCN_ALIGN_4BYTES:    alignment = 4;    break;
        case IMAGE_SCN_ALIGN_8BYTES:    alignment = 8;    break;
        case IMAGE_SCN_ALIGN_16BYTES:   alignment = 16;   break;
        case IMAGE_SCN_ALIGN_32BYTES:   alignment = 32;   break;
        case IMAGE_SCN_ALIGN_64BYTES:   alignment = 64;   break;
        case IMAGE_SCN_ALIGN_128BYTES:  alignment = 128;  break;
        case IMAGE_SCN_ALIGN_256BYTES:  alignment = 256;  break;
        case IMAGE_SCN_ALIGN_512BYTES:  alignment = 512;  break;
        case IMAGE_SCN_ALIGN_1024BYTES: alignment = 1024; break;
        case IMAGE_SCN_ALIGN_2048BYTES: alignment = 2048; break;
        case IMAGE_SCN_ALIGN_4096BYTES: alignment = 4096; break;
        case IMAGE_SCN_ALIGN_8192BYTES: alignment = 8192; break;

        default:                        if (s -> Characteristics & IMAGE_SCN_TYPE_NO_PAD)
                                            alignment = 1;
                                        else
                                            alignment = 16;
                                        break;
    }

    Segment * seg = new Segment (name,
                                 name,
                                 clazz,
                                 false,
                                 s -> SizeOfRawData,
                                 alignment);

#if defined (OBSOLETE)
    seg -> combination = 2;                   /* Public */
    seg -> attributes  = 2 << 2;
#endif

    if (s -> PointerToRawData && s -> SizeOfRawData) {
        byte * p = rawdata + s -> PointerToRawData;
        if (seg -> clazz == BSS) {
            // check that BSS segment is zero-initialized
            for (dword j = 0; j < s -> SizeOfRawData; j++) {
                if (p [j]) {
                    Message(xFATAL, msgCANNOT_INIT_BSS_SEG,
                            GetCurrentOBJFileName (),
                            NAMES.Index2Str (seg -> name));
                    return;
                }
            }
        } else {
            memcpy (seg -> getText(), p, s -> SizeOfRawData);
        }
    }

    // Read section line numbers
    if (s -> NumberOfLinenumbers) {
        PIMAGE_LINENUMBER pln = (PIMAGE_LINENUMBER) (rawdata + s -> PointerToLinenumbers);

        seg -> nlines = seg -> linessize = s -> NumberOfLinenumbers;
        struct Line * l = seg -> lines = (struct Line *) xalloc (s -> NumberOfLinenumbers * sizeof (struct Line));

        word origin = 0;
        for (dword j = 0; j < s -> NumberOfLinenumbers; j++, pln ++) {
            if (pln -> Linenumber == 0) { /* Symbol table index */
                PIMAGE_SYMBOL pSymbol = PCOFFSymbolTable + pln -> Type.SymbolTableIndex;
                pSymbol += pSymbol -> NumberOfAuxSymbols + 1;

                char buf [SYMBOL_BUF_LENGTH];

                if ((pSymbol -> StorageClass == IMAGE_SYM_CLASS_FUNCTION) &&
                    ! strcmp (Symbol (pSymbol, buf), ".bf") &&
                    (pSymbol -> NumberOfAuxSymbols == 1))
                {
                    l -> line   = origin = ((PIMAGE_AUX_SYMBOL) (pSymbol + 1)) -> Sym.Misc.LnSz.Linenumber;
                    l -> offset = pSymbol -> Value;
                    l ++;
                } else {
                    seg -> nlines --;
                }
            } else {
                ASSERT (pln -> Linenumber + origin < 0x10000);
                l -> line   = (word) (pln -> Linenumber + origin);
                l -> offset = pln -> Type.VirtualAddress;
                l ++;
            }
        }
    }

    SegsTable [secIndex] = seg;

    // UNSUPPORTED: COMDAT sections & Extended relocations
    if (s -> Characteristics & IMAGE_SCN_LNK_COMDAT) {
        VerboseMessage ("WARNING: COMDAT section %s detected (file %s)\n",
                        NAMES.Index2Str (seg->name),
                        GetCurrentOBJFileName ());
    }
    if (s -> Characteristics & IMAGE_SCN_LNK_NRELOC_OVFL) {
        VerboseMessage ("WARNING: Section %s with EXTENDED RELOCATIONS detected (file %s)\n",
                        NAMES.Index2Str (seg->name),
                        GetCurrentOBJFileName ());
    }
    
    // UNSUPPORTED: Section grouping & ordering
}

/*----------------------------------------------------------------------------*/

void COFFObjectFileReader::SetFileName (char * filename)
{
    CurrentFile -> setSource(dup (filename, strlen (filename)));
}

/*----------------------------------------------------------------------------*/

void COFFObjectFileReader::DeclarePublicName (ident name, Segment * s, int offset)
{
    if (CurrentFile -> lib) {
        // public names declared in library are limited by library "export"
        for (dword i = 0; i < nPublicNames; i++)
            if (publicNames[i] == name) {
                NewPublicName (name, s, offset);
                return;
            }
    } else
        NewPublicName (name, s, offset);
}

/*----------------------------------------------------------------------------*/

void COFFObjectFileReader::ProcessSymbols (void)
{
    PIMAGE_SYMBOL pSymbol = PCOFFSymbolTable;
    int nnames = 0;

    for (dword i = 0; i < COFFSymbolCount; i += pSymbol -> NumberOfAuxSymbols + 1,
                                     pSymbol += pSymbol -> NumberOfAuxSymbols + 1)
    {
        switch (pSymbol -> StorageClass) {
            case IMAGE_SYM_CLASS_EXTERNAL:
            {
                short section = pSymbol -> SectionNumber;

                char buf [SYMBOL_BUF_LENGTH];
                char * symname = Symbol (pSymbol, buf);

                VerboseMessage ("COFF: external symbol %s section %h\n", symname, section);

                if ((section >= 1) && ((dword)section <= nSections) &&
                    (SegsTable [section] != NULL))
                {
                    DeclarePublicName (NAMES.Str2Index (symname),
                                       SegsTable [section],
                                       pSymbol -> Value);
                } else if (section == IMAGE_SYM_ABSOLUTE) {
                    DeclarePublicName (NAMES.Str2Index (symname),
                                       NULL,
                                       pSymbol -> Value);
                } else if (section == IMAGE_SYM_UNDEFINED) {
                    if (pSymbol -> Value != 0)
                        // Size is known, but symbol's origin is unknown
                        // here we should set attributes for K_COMDEF
                        NewCommonName (NAMES.Str2Index (symname), pSymbol -> Value);
                    else
                        nnames ++;
                } else {
                    VerboseMessage ("COFF: WARNING: IMAGE_SYM_CLASS_EXTERNAL symbol %s section %h IGNORED\n", symname, section);
                }
            }
            break;

            case IMAGE_SYM_CLASS_WEAK_EXTERNAL:
            {
                char symnameBuf [SYMBOL_BUF_LENGTH];
                char * symname = Symbol (pSymbol, symnameBuf);

                if (pSymbol -> NumberOfAuxSymbols != 1) {
                    Message (xFATAL, msgINVALID_WEAK_EXTERN, GetCurrentOBJFileName (), symname);
                    return;
                }

                PIMAGE_SYMBOL def = PCOFFSymbolTable + ((PIMAGE_AUX_SYMBOL) (pSymbol + 1)) -> Sym.TagIndex;
                // UNSUPPORTED: Characteristics of weak extern symbol

                if (def -> StorageClass != IMAGE_SYM_CLASS_EXTERNAL) {
                    Message (xFATAL, msgINVALID_WEAK_EXTERN, GetCurrentOBJFileName (), symname);
                    return;
                }

                char fwdsymnameBuf [SYMBOL_BUF_LENGTH];
                char * fwdsymname = Symbol (pSymbol, fwdsymnameBuf);

                NewWeakName (NAMES.Str2Index (symname), NAMES.Str2Index (fwdsymname));
            }
            break;

            case IMAGE_SYM_CLASS_FILE:
                SetFileName ((char *) (pSymbol + 1));
                break;

            case IMAGE_SYM_CLASS_NULL:
            {
                short section = pSymbol -> SectionNumber;

                char buf [SYMBOL_BUF_LENGTH];
                char * symname = Symbol (pSymbol, buf);

                DeclarePublicName (NAMES.Str2Index (symname),
                                   SegsTable [section],
                                   pSymbol -> Value);
            }
            break;

            default:
            {
                char buf [SYMBOL_BUF_LENGTH];
                char * symname = Symbol (pSymbol, buf);
                VerboseMessage ("COFF: WARNING: symbol %s (storage class %d, section %X, value %X) IGNORED\n",
                                symname, pSymbol->StorageClass, (dword) (pSymbol->SectionNumber), pSymbol->Value);
            }
        }
    }

    if (nnames) {
        ident * names = CurrentFile -> extrnls = (ident *) xrealloc(CurrentFile -> extrnls,
                                                                    CurrentFile -> nextrnls * sizeof (ident),
                                                                    (CurrentFile -> nextrnls + nnames) * sizeof (ident));
        names += CurrentFile -> nextrnls;
        CurrentFile -> nextrnls += nnames;

        PIMAGE_SYMBOL pSymbol = PCOFFSymbolTable;

        for (dword i = 0; i < COFFSymbolCount; i += pSymbol -> NumberOfAuxSymbols + 1,
                                         pSymbol += pSymbol -> NumberOfAuxSymbols + 1)
        {
            if ((pSymbol -> StorageClass  == IMAGE_SYM_CLASS_EXTERNAL) &&
                (pSymbol -> SectionNumber == IMAGE_SYM_UNDEFINED) &&
                (pSymbol -> Value == 0))
            {
                char buf [SYMBOL_BUF_LENGTH];
                * names ++ = NAMES.Str2Index (Symbol (pSymbol, buf));
            }
        }
    }
}

/*----------------------------------------------------------------------------*/

void COFFObjectFileReader::ProcessRelocations (PIMAGE_SECTION_HEADER sec,
                                               PIMAGE_RELOCATION pRelocs,
                                               dword count,
                                               Segment * s)
{
    if (count == 0)
        return;

    s -> allocateFixups (count);

    do {
        if (pRelocs -> SymbolTableIndex >= COFFSymbolCount) {
            Message (xFATAL, msgILLEGAL_SYM_IDX, GetCurrentOBJFileName (), pRelocs -> SymbolTableIndex);
            return;
        }

        PIMAGE_SYMBOL pSymbol = PCOFFSymbolTable + pRelocs -> SymbolTableIndex;

        byte kind = 0;
        switch (pRelocs -> Type) {
            case IMAGE_REL_I386_DIR32:
                kind = FIXUP_ADDRESS32;
                break;

            case IMAGE_REL_I386_DIR32NB:
                kind = FIXUP_OFFSET32NB;
                break;

            case IMAGE_REL_I386_REL32:
                kind = FIXUP_SELFRELOFFS32;
                break;

            default:
                /* UNSUPPORTED Reloc Types: IMAGE_REL_I386_ABSOLUTE
                                            IMAGE_REL_I386_SECTION
                                            IMAGE_REL_I386_SECREL
                */
                Message (xFATAL, msgUNSUPPORTED_FIXUP_TYPE, GetCurrentOBJFileName (), pRelocs -> Type);
                return;
        }

        if (pRelocs -> VirtualAddress < sec -> VirtualAddress) {
            Message (xERROR, msgINCORRECT_RECORD_FORMAT, GetCurrentOBJFileName (), "RELOC", "Reloc VA < Section VA");
            return;
        }
        dword  offset    = pRelocs -> VirtualAddress - sec -> VirtualAddress;

        byte   k_target  = 0;
        void * target    = NULL;
        int    fx_offset = 0;

        short section = pSymbol -> SectionNumber;
        if ((section >= 1) && ((dword)section <= nSections)) {
            k_target = TK_SEG;
            target   = SegsTable [section];
            if (target == NULL) {
                Message (xFATAL, msgINVALID_SECTION, GetCurrentOBJFileName (), section);
                return;
            }
            fx_offset = pSymbol -> Value;
        }
        else if (section == IMAGE_SYM_UNDEFINED) {
            char buf [SYMBOL_BUF_LENGTH];
            ident name = NAMES.Str2Index (Symbol (pSymbol, buf));

            switch (pSymbol -> StorageClass) {
                case IMAGE_SYM_CLASS_EXTERNAL:
                case IMAGE_SYM_CLASS_WEAK_EXTERNAL:
                    k_target = TK_ID;
                    target   = (void *) name;
                    break;

                case IMAGE_SYM_CLASS_SECTION:
                    k_target = TK_FWD_SEG;
                    target   = (void *) name;
                    break;

                default:
                    Message (xFATAL, msgBAD_STORAGE_CLASS, GetCurrentOBJFileName (), pSymbol -> StorageClass);
                    return;
            }
        } else {
            Message(xFATAL, msgINVALID_SECTION, GetCurrentOBJFileName (), section);
            return;
        }
        pRelocs ++;

        addFixup (s, kind, offset, k_target, target, fx_offset);

    } while (-- count);
}

/*----------------------------------------------------------------------------*/

void COFFObjectFileReader::Read (byte * rawdata, long /*size*/, char * filename, char * libname)
{
    PIMAGE_FILE_HEADER pImageFileHeader = (PIMAGE_FILE_HEADER) rawdata;

    if (pImageFileHeader -> Machine != IMAGE_FILE_MACHINE_I386 &&
        pImageFileHeader -> Machine != IMAGE_FILE_MACHINE_UNKNOWN)
    {
        Message (xERROR, msgILLEGAL_CPU_TYPE, filename, pImageFileHeader -> Machine);
        return;
    }

    SetFile (filename, libname);

    nSections = pImageFileHeader -> NumberOfSections;
    if (nSections >= SegsSize) {
        xfree (SegsTable);
        SegsSize = nSections + 1;
        SegsTable = (Segment **) xalloc (SegsSize * sizeof (Segment *));
        memset (SegsTable, 0, SegsSize * sizeof (Segment *));
    }

    PCOFFSymbolTable = (PIMAGE_SYMBOL) (rawdata + pImageFileHeader -> PointerToSymbolTable);
    COFFSymbolCount  = pImageFileHeader -> NumberOfSymbols;

    stringTable = (PSTR) & PCOFFSymbolTable [COFFSymbolCount];

    Sections = (PIMAGE_SECTION_HEADER) (rawdata + sizeof (IMAGE_FILE_HEADER) +
                                        pImageFileHeader -> SizeOfOptionalHeader);

    dword i;
    PIMAGE_SECTION_HEADER section = Sections;
    for (i = 1; i <= nSections; i ++, section ++)
        ReadSection (rawdata, section, i);

    ProcessSymbols ();

    section = Sections;
    for (i = 1; i <= nSections; i ++, section ++)
        if (SegsTable [i] && section -> PointerToRelocations != 0)
            ProcessRelocations (section,
                                (PIMAGE_RELOCATION) (rawdata + section -> PointerToRelocations),
                                section -> NumberOfRelocations,
                                SegsTable [i]);
}

/*----------------------------------------------------------------------------*/

#define FILENAME_LEN 16

static Bool checkSize (long totalSize, long base, long sizeToRead, char * libname) {
    if (base + sizeToRead > totalSize) {
        Message (xERROR, msgINVALID_COFF_LIBRARY, libname);
        return false;
    }
    return true;
}

void COFFObjectFileReader::ReadLibrary (byte * rawdata,
                                        long size,
                                        char * libname)
{
    long base = IMAGE_ARCHIVE_START_SIZE;
    long sizeToRead;

    PIMAGE_ARCHIVE_MEMBER_HEADER header;

    // 1st Linker Member
    sizeToRead = sizeof (IMAGE_ARCHIVE_MEMBER_HEADER);
    if (!checkSize (size, base, sizeToRead, libname)) return;

    header = (PIMAGE_ARCHIVE_MEMBER_HEADER) (rawdata + base);
    if (memcmp (header -> Name, IMAGE_ARCHIVE_LINKER_MEMBER, FILENAME_LEN)) {
       Message (xERROR, msgREQUIRED_MEMBER_MISSING, "1st Linker", libname);
       return;
    }
    char * publicDirectory = (char *) (rawdata + base + sizeof (IMAGE_ARCHIVE_MEMBER_HEADER));
    sizeToRead += strtoul ((char *)(header -> Size), 0, 10);
    if (!checkSize (size, base, sizeToRead, libname)) return;
    base += sizeToRead;

    nPublicNames = (( (* (byte *)(publicDirectory  ))  * 256 +
                      (* (byte *)(publicDirectory+1))) * 256 +
                      (* (byte *)(publicDirectory+2))) * 256 +
                      (* (byte *)(publicDirectory+3));
    VerboseMessage ("COFF Library: %d public names\n", nPublicNames);
    publicNames = (ident *) xalloc (sizeof (ident) * nPublicNames);
    publicDirectory = publicDirectory + 4 + nPublicNames*4;
    for (dword i = 0; i < nPublicNames; i++) {
        int len  = strlen (publicDirectory);
        publicNames [i] = NAMES.Str2Index (publicDirectory, len);
        publicDirectory += len + 1;
    }

    // 2nd Linker Member
    sizeToRead = sizeof (IMAGE_ARCHIVE_MEMBER_HEADER);
    if (!checkSize (size, base, sizeToRead, libname)) return;

    header = (PIMAGE_ARCHIVE_MEMBER_HEADER) (rawdata + base);
    if (memcmp (header -> Name, IMAGE_ARCHIVE_LINKER_MEMBER, FILENAME_LEN)) {
       Message (xERROR, msgREQUIRED_MEMBER_MISSING, "2nd Linker", libname);
       return;
    }
    sizeToRead += strtoul ((char *)(header -> Size), 0, 10);
    if (!checkSize (size, base, sizeToRead, libname)) return;
    base += sizeToRead;

    // Longnames Member, if present
    sizeToRead = sizeof (IMAGE_ARCHIVE_MEMBER_HEADER);
    if (!checkSize (size, base, sizeToRead, libname)) return;
    header = (PIMAGE_ARCHIVE_MEMBER_HEADER) (rawdata + base);

    char * longnames = NULL;
    if (!memcmp (header -> Name, IMAGE_ARCHIVE_LONGNAMES_MEMBER, FILENAME_LEN)) {
        longnames = (char *) (rawdata + base + sizeof (IMAGE_ARCHIVE_MEMBER_HEADER));

        sizeToRead += strtoul ((char *)(header -> Size), 0, 10);
        if (!checkSize (size, base, sizeToRead, libname)) return;
        base += sizeToRead;
    }

    // COFF obj files
    while (base < size) {
        sizeToRead = sizeof (IMAGE_ARCHIVE_MEMBER_HEADER);
        if (!checkSize (size, base, sizeToRead, libname)) return;

        header = (PIMAGE_ARCHIVE_MEMBER_HEADER) (rawdata + base);

        char * filename, buf [FILENAME_LEN + 1];
        if (header -> Name [0] == '/') {
            filename = longnames + strtoul ((char *)(header -> Name + 1), 0, 10);
        } else {
            memcpy (buf, header -> Name, FILENAME_LEN);
            buf [FILENAME_LEN] = 0;
            char * end = strchr (buf, '/');
            if (end)
                * end = 0;
            filename = buf;
        }
        sizeToRead += (strtoul ((char *)(header -> Size), 0, 10) + 1) & ~1;
        if (!checkSize (size, base, sizeToRead, libname)) return;

        Read (rawdata + base + sizeof (IMAGE_ARCHIVE_MEMBER_HEADER),
              sizeToRead,
              filename,
              libname);

        base += sizeToRead;
    }
}

/*----------------------------------------------------------------------------*/

static COFFObjectFileReader * COFFReader;

void InitCOFF  (void) { COFFReader = new COFFObjectFileReader(); }
void ClearCOFF (void) { delete COFFReader; }

void ReadCOFF (byte * rawdata, long size, char * filename)
{
    COFFReader->Read (rawdata, size, filename, NULL);
}

void ReadCOFFLibrary (byte * rawdata, long size, char * libname)
{
    COFFReader->ReadLibrary (rawdata, size, libname);
}

close_namespace

