#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <ctype.h>

#include "xdefs.h"

open_namespace

#include "struct.h"
#include "idents.h"
#include "messages.h"
#include "xdebug.h"
#include "debug.h"
#include "implib.h"
#include "xmem.h"
#include "xpe.h"

#include "writepe.h"
#include "writer.h"

/*----------------------------------------------------------------------------*/

byte DefaultPEStub [DefaultPEStubSize] = {
        0x4D, 0x5A, 0x80, 0x00, 0x01, 0x00, 0x00, 0x00,
        0x04, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0x00, 0x00,
        0xB8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x0E, 0x1F, 0xBA, 0x0E, 0x00, 0xB4, 0x09, 0xCD,
        0x21, 0xB8, 0x01, 0x4C, 0xCD, 0x21, 0x74, 0x68,
        0x69, 0x73, 0x20, 0x69, 0x73, 0x20, 0x61, 0x20,
        0x57, 0x69, 0x6E, 0x64, 0x6F, 0x77, 0x73, 0x20,
        0x4E, 0x54, 0x20, 0x63, 0x68, 0x61, 0x72, 0x61,
        0x63, 0x74, 0x65, 0x72, 0x2D, 0x6D, 0x6F, 0x64,
        0x65, 0x20, 0x65, 0x78, 0x65, 0x63, 0x75, 0x74,
        0x61, 0x62, 0x6C, 0x65, 0x0D, 0x0A, 0x24, 0x00
};

/*----------------------------------------------------------------------------*/

static dword TimeDateStamp;

/*----------------------------------------------------------------------------*/
/*                                                                            */
/*                      Form import section                                   */
/*                                                                            */
/*----------------------------------------------------------------------------*/

static Storage * ImportImage = NULL;

/*----------------------------------------------------------------------------*/

static void CreateDLLTables (void)
{
    if (Ndlls == 0 && IdataLen == 0) {
        VASA->discardVirtualSpace (IdataStart);
        return;
    }

    ImportImage = newStorage(65536);

    //  Reserve space for directory

    if (IdataLen == 0)
        ImportImage -> ZeroBytes ((Ndlls + 1) * 4 * 5);
    else {
        ImportImage -> ZeroBytes ((Ndlls * 4 * 5 + IdataLen + 3) & ~ 3);
        memcpy (ImportImage->Ptr + Ndlls * 4 * 5, IdataPtr, IdataLen);
    }

/*
    // Check DLL overlapping
    if (xCheckDLLOverlapping) {
        for (int i = 0; i < Ndlls; i++) {
            GetImageLocation (
            char * dllname = OS -> FindDLL (NAMES.Index2Str(dlls[i].name));
            if (dllname == NULL) {
                ASSERT_FALSE (); // warining: not found
            }
            OSFile dllfile = OS -> File ();
            if (!dllfile -> OpenRead (dllname, data, size)) {
                ASSERT_FALSE (); // warining: can't read
            }

        }
    }
*/

    // Set DLL names
    int i = 0;
    for (; i < Ndlls; i++) {
        * ((dword *) (ImportImage->Ptr + i*4*5 + 12)) = ImportImage->Index + IdataStart - xImageBase;
        ImportImage->PutS (NAMES.Index2Str(dlls[i].name), NAMES.Index2StrLen(dlls[i].name) + 1);
        ImportImage->Align2();
    }
    ImportImage->Align4();

    // Set lookup tables/address tables RVAs; reserve space for them
    for (i = 0; i < Ndlls; i++) {
        dlls[i].lookups = ImportImage->Index;
        * ((dword *) (ImportImage->Ptr + i*4*5 + 0)) = ImportImage->Index + IdataStart - xImageBase;
        ImportImage->ZeroBytes ((dlls[i].nentries + 1) * 4);

        dlls[i].addresses = ImportImage->Index;
        * ((dword *) (ImportImage->Ptr + i*4*5 + 16)) = ImportImage->Index + IdataStart - xImageBase;
        ImportImage->ZeroBytes ((dlls[i].nentries + 1) * 4);
    }

    // Form lookup/address tables
    for (i = 0; i < NumberOfImportedNames; i++) {
        importNameInfo * s = (importNameInfo *) (NAMES.getInfo (ImportedNames[i]));
        if (s && ((s -> kind & (K_USED | K_MASK)) == K_USED + K_IMPORT))
        {
                ident modulename = s -> getModuleName();
                /*FIXME: findDLL */
                int k = 0;
                for (; k < Ndlls; k++)
                    if (dlls[k].name == modulename)
                        break;
                ASSERT (k < Ndlls);
                struct dll * q = &(dlls [k]);
                q -> nentries --;
                dword lookup_ofs  = q -> lookups   + q -> nentries * 4;
                dword address_ofs = q -> addresses + q -> nentries * 4;
                if (s -> byOrdinal ()) {
                    *((dword *)(ImportImage->Ptr + lookup_ofs )) =
                    *((dword *)(ImportImage->Ptr + address_ofs)) = 0x80000000 + s -> getOrdinal();
                } else {
                    *((dword *)(ImportImage->Ptr + lookup_ofs )) =
                    *((dword *)(ImportImage->Ptr + address_ofs)) = ImportImage->Index + IdataStart - xImageBase;

                    ident impname = s -> getName();
                    ImportImage->PutB (1);
                    ImportImage->PutB (0);
                    ImportImage->PutS (NAMES.Index2Str(impname), NAMES.Index2StrLen(impname) + 1);
                    ImportImage->Align2();
                }
                if (s -> kind & K_IMPFUNC) {
                    dword jump_offset = s -> offset - CodeStart;
                    *(( word *) (CodePtr + jump_offset    )) = JUMP_CODE;
                    *((dword *) (CodePtr + jump_offset + 2)) = q -> addresses + q -> nentries * 4 + IdataStart;
                } else {
                    s -> patchFixups (address_ofs + IdataStart);
                }
        }
    }

    ImportImage->Align4();
    IdataLen = ImportImage->Index;

    VASA->allocateVirtualSpace (IdataStart, IdataLen);
}

/*----------------------------------------------------------------------------*/
/*                                                                            */
/*                      Form export section                                   */
/*                                                                            */
/*----------------------------------------------------------------------------*/

static int ExportStart;
static Storage * ExportImage = NULL;

/*----------------------------------------------------------------------------*/

static int XCDECL compare_names (const void * p1, const void * p2)
{
    Export *e1 = * (Export **) p1;
    Export *e2 = * (Export **) p2;
    return  (e1->flag & EFLAG_NAME_EXPORTED) ? 
           ((e2->flag & EFLAG_NAME_EXPORTED) ? 
             strcmp (NAMES.Index2Str (e1 -> extname), NAMES.Index2Str (e2 -> extname)) : 1) :
           ((e2->flag & EFLAG_NAME_EXPORTED) ? -1 : 0);
}

/*----------------------------------------------------------------------------*/

static void CreateExport (char * fname)
{
    if ((Exports == NULL) || (MaxOrdinal == 0))
        return;

    qsort (ExportsTable, NumberOfExports, sizeof (Export *), compare_names);

    ExportStart = VASA->getVirtualAddr();

    ExportImage = newStorage (65536);

    // Create Export Directory

    IMAGE_EXPORT_DIRECTORY dir;

    memset (& dir, 0, sizeof (dir));
    dir.TimeDateStamp         = TimeDateStamp;
    dir.Base                  = 1;
    dir.NumberOfFunctions     = MaxOrdinal;
    dir.NumberOfNames         = NumberOfExports;

    int funs_offs  = sizeof (dir);
    int names_offs = funs_offs  + dir.NumberOfFunctions * sizeof (PDWORD);
    int ords_offs  = names_offs + dir.NumberOfNames     * sizeof (PDWORD);

    dir.AddressOfFunctions    = (PDWORD *) (ExportStart - xImageBase + funs_offs );
    dir.AddressOfNames        = (PDWORD *) (ExportStart - xImageBase + names_offs);
    dir.AddressOfNameOrdinals = (PWORD  *) (ExportStart - xImageBase + ords_offs );
    dir.Name                  = ExportStart - xImageBase + ords_offs + dir.NumberOfNames * sizeof (WORD);

    ExportImage->PutS ((byte *) &dir, sizeof (dir));

    // Reserve space
    ExportImage->ZeroBytes( dir.NumberOfFunctions * sizeof (PDWORD) +
                            dir.NumberOfNames     * sizeof (PDWORD) +
                            dir.NumberOfNames     * sizeof (WORD) );

    // Write stripped output file name
    char * p = strrchr (fname, '\\');
    if (p == NULL)
            p = strrchr (fname, '/');
    if (p == NULL)
            p = fname;
    else
            p ++;
    StrippedOutputFileName = p;
    do {
            ExportImage->PutB((byte) toupper (* p));
    } while (* (p ++));
    ExportImage->Align2();

    // Fill in export image
    for (int n = 0; n < NumberOfExports; n ++) {
        Export * s = ExportsTable[n];
        ASSERT (s -> ordinal != 0);

        PDWORD funs  = (PDWORD) (ExportImage->Ptr + funs_offs);
        PWORD  ords  = (PWORD)  (ExportImage->Ptr + ords_offs);
        PDWORD names = (PDWORD) (ExportImage->Ptr + names_offs);

        ords_offs  += sizeof(WORD);
        names_offs += sizeof(DWORD);

        if (s -> seg) {
            ASSERT ((s -> seg -> isProcessed ()) && (s -> seg -> address > 0));
            ASSERT ((int)(s -> offset) <= (s -> seg -> getLen()));
            funs [s -> ordinal - 1] = s -> seg -> address + s -> offset;
        } else {
            nameInfo * info = (nameInfo *) (NAMES.getInfo (s -> intname));
            ASSERT (info != NULL);
            ASSERT (info->kind & K_USED);
            ASSERT ((dword)(info->offset) >= xImageBase);
            funs [s -> ordinal - 1] = info -> offset - xImageBase;
        }

        * ords  = (word) (s -> ordinal - 1);

        if (s -> flag & EFLAG_NAME_EXPORTED) {
            * names = (ExportImage -> Index + ExportStart - xImageBase);
            ExportImage->PutS (NAMES.Index2Str(s->extname), NAMES.Index2StrLen(s->extname) + 1);
            ExportImage->Align2 ();
        } else
            * names = 0;
    }
    xfree (ExportsTable);

    VASA->allocateVirtualSpace (ExportStart, ExportImage -> Index);
}

/*----------------------------------------------------------------------------*/
/*                                                                            */
/*                      Form relocation section                               */
/*                                                                            */
/*----------------------------------------------------------------------------*/

static Storage * RelocImage = NULL;
static dword RelStart, RelLen = 0;

static void WriteRelocBlock (dword * fixAddrs, int nAddrs)
{
    dword len = ((nAddrs + 1) / 2) * 4 + 8;
    dword base = (* fixAddrs) & ~ 4095;

    RelocImage -> Check (len);

    RelocImage -> Put4 (base - xImageBase);
    RelocImage -> Put4 (len);

    do {
        int rel = (IMAGE_REL_BASED_HIGHLOW << 12) + * (fixAddrs ++) - base;
        ASSERT( (rel > 0) && (rel < 0x10000) );
        RelocImage -> Put2 (rel);
    } while (-- nAddrs);

    RelocImage -> Align4 ();
}

static void CreateRelocations (void)
{
    if (xFixed) return;

    RelStart = VASA->getVirtualAddr();

    RelocImage = newStorage (65536);

    if (NFixups == 0) {
        RelocImage -> Put4 (0);
        RelocImage -> Put4 (8);

    } else {
        int n = NFixups;
        dword * fixAddrs = Fix;
        do {
            if (n == 1) {
                WriteRelocBlock (fixAddrs, 1);
                break;
            }
            int m = n;
            dword * q = fixAddrs;
            do {
                if ((q [0] & ~ 4095) != (q [1] & ~ 4095))
                    break;
                q ++;
                m --;
            } while (m > 1);
            WriteRelocBlock (fixAddrs, n - m + 1);
            fixAddrs = q + 1;
            n = m - 1;
        } while (n);
    }

    RelLen = RelocImage -> Index;

    // Microsoft's linker always writes 262 zero bytes after relocs.
    // It seems that if size of relocs == virtual size of section == 4K,
    // the DLL could not be loaded on Windows 98 system.
    RelocImage -> ZeroBytes (262);

    VASA->allocateVirtualSpace (RelStart, RelocImage -> Index);
}

/*----------------------------------------------------------------------------*/
/*                                                                            */
/*                      Form resources section                                */
/*                                                                            */
/*----------------------------------------------------------------------------*/

static dword ResStart, ResLen = 0, ResNamesLen = 0;
static byte * ResImage;

/*----------------------------------------------------------------------------*/

static int byte_len (word * p)
{
        int len;

        for (len = 2; * p; len += 2, p ++)
                ;
        return len;
}

/*----------------------------------------------------------------------------*/

void CalcResLen (void)
{
    ResourceDirectory * res_dir;
    Resource          * res;

    if (numberOfResourceDirectories == 0)
        return;

    ResLen = sizeof (IMAGE_RESOURCE_DIRECTORY) +
             sizeof (IMAGE_RESOURCE_DIRECTORY_ENTRY) * numberOfResourceDirectories +

             sizeof (IMAGE_RESOURCE_DIRECTORY) * numberOfResourceDirectories +
             sizeof (IMAGE_RESOURCE_DIRECTORY_ENTRY) * numberOfResources +

             sizeof (IMAGE_RESOURCE_DIRECTORY) * numberOfResources +
             sizeof (IMAGE_RESOURCE_DIRECTORY_ENTRY) * numberOfResources +
             sizeof (IMAGE_RESOURCE_DATA_ENTRY) * numberOfResources;

    ResNamesLen = 0;
    for (res_dir = resourceDirectoryList; res_dir; res_dir = res_dir->next) {
        if (res_dir -> typeName)
            ResNamesLen += byte_len (res_dir -> typeName);
        for (res = res_dir -> resList; res; res = res->next) {
            if (res -> resourceName)
                ResNamesLen += byte_len (res->resourceName);
            ResLen += (res -> datasize + 3) & ~ 3;
        }
    }
    ResNamesLen = (ResNamesLen + 3) & ~ 3;
    ResLen += ResNamesLen;
}

/*----------------------------------------------------------------------------*/

static byte * FormResName (byte * p, word * name)
{
        int len;

        len = byte_len ((word*)name);
        * (word *) p = (word) (len / 2 - 1);
        memcpy (p + 2, name, len - 2);
        return p + len;
}

/*----------------------------------------------------------------------------*/

void CreateResImage (void)
{
    byte * main_dir, * sec_dirs, * third_dirs, * ptrs, * names, * data;
    ResourceDirectory * res_dir;
    Resource          * res;

    IMAGE_RESOURCE_DIRECTORY       dir;
    IMAGE_RESOURCE_DIRECTORY_ENTRY ent;
    IMAGE_RESOURCE_DATA_ENTRY      dat;

    dir . Characteristics = 0;
    dir . TimeDateStamp   = TimeDateStamp;
    dir . MajorVersion    =
    dir . MinorVersion    = 0;
    dir . NumberOfNamedEntries = 0;
    dir . NumberOfIdEntries    = 0;

    dat . Reserved = 0;

    main_dir = ResImage = (byte *) xalloc (ResLen);
    memset (ResImage, 0, ResLen);
    sec_dirs = main_dir +
            sizeof (IMAGE_RESOURCE_DIRECTORY) +
            sizeof (IMAGE_RESOURCE_DIRECTORY_ENTRY) * numberOfResourceDirectories;
    third_dirs = sec_dirs +
            sizeof (IMAGE_RESOURCE_DIRECTORY) * numberOfResourceDirectories +
            sizeof (IMAGE_RESOURCE_DIRECTORY_ENTRY) * numberOfResources;
    ptrs = third_dirs +
            sizeof (IMAGE_RESOURCE_DIRECTORY) * numberOfResources +
            sizeof (IMAGE_RESOURCE_DIRECTORY_ENTRY) * numberOfResources;
    names = ptrs + sizeof (IMAGE_RESOURCE_DATA_ENTRY) * numberOfResources;
    data  = names + ResNamesLen;

    dir . NumberOfNamedEntries = dir . NumberOfIdEntries = 0;
    for (res_dir = resourceDirectoryList; res_dir; res_dir = res_dir->next)
        if (res_dir -> typeName)
            dir . NumberOfNamedEntries ++;
        else
            dir . NumberOfIdEntries ++;
    * (IMAGE_RESOURCE_DIRECTORY *) main_dir = dir;
    main_dir += sizeof (IMAGE_RESOURCE_DIRECTORY);
    for (res_dir = resourceDirectoryList; res_dir; res_dir = res_dir->next) {
        ent . OffsetToData = IMAGE_RESOURCE_DATA_IS_DIRECTORY | (sec_dirs - ResImage);
        if (res_dir -> typeName) {
            ent . Name = IMAGE_RESOURCE_NAME_IS_STRING | (names - ResImage);
            names = FormResName (names, res_dir -> typeName);
        } else
            ent . Name = res_dir -> typeID;
        * (IMAGE_RESOURCE_DIRECTORY_ENTRY *) main_dir = ent;
        main_dir += sizeof (IMAGE_RESOURCE_DIRECTORY_ENTRY);
        dir . NumberOfNamedEntries = dir . NumberOfIdEntries = 0;

        Resource * prev = NULL;
        for (res = res_dir -> resList; res; prev = res, res = res->next) {
            if (prev && (prev->compareNameID (res) == 0))
                continue;

            if (res -> resourceName)
                dir . NumberOfNamedEntries ++;
            else
                dir . NumberOfIdEntries ++;
        }
        * (IMAGE_RESOURCE_DIRECTORY *) sec_dirs = dir;
        sec_dirs += sizeof (IMAGE_RESOURCE_DIRECTORY);

        for (res = res_dir -> resList; res;) {
            ent . OffsetToData = IMAGE_RESOURCE_DATA_IS_DIRECTORY | (third_dirs - ResImage);
            if (res -> resourceName) {
                ent . Name = IMAGE_RESOURCE_NAME_IS_STRING | (names - ResImage);
                names = FormResName (names, res -> resourceName);
            } else
                ent . Name = res -> resourceID;
            * (IMAGE_RESOURCE_DIRECTORY_ENTRY *) sec_dirs = ent;
            sec_dirs += sizeof (IMAGE_RESOURCE_DIRECTORY_ENTRY);

            WORD sameNameResources = 1;
            Resource * resNext = res->next;
            for (; resNext; resNext = resNext->next) {
                if (res->compareNameID(resNext) == 0)
                    sameNameResources++;
                else
                    break;
            }

            dir . NumberOfNamedEntries = 0;
            dir . NumberOfIdEntries    = sameNameResources;
            * (IMAGE_RESOURCE_DIRECTORY *) third_dirs = dir;
            third_dirs += sizeof (IMAGE_RESOURCE_DIRECTORY);

            for (int j = 0; j < sameNameResources; j++, res = res->next)
            {
                ent . OffsetToData = (ptrs - ResImage);
                ent . Name = res -> LanguageId;
                * (IMAGE_RESOURCE_DIRECTORY_ENTRY *) third_dirs = ent;
                third_dirs += sizeof (IMAGE_RESOURCE_DIRECTORY_ENTRY);

                dat . OffsetToData = ResStart + (data - ResImage) - xImageBase;
                dat . Size         = res -> datasize;
                dat . CodePage     = 0;                 /* ????? */
                * (IMAGE_RESOURCE_DATA_ENTRY *) ptrs = dat;
                ptrs += sizeof (IMAGE_RESOURCE_DATA_ENTRY);
                memcpy (data, res -> data, res -> datasize);
                data += (res -> datasize + 3) & ~ 3;
            }

            ASSERT (res == resNext);
        }
    }
}

/*----------------------------------------------------------------------------*/
/*                                                                            */
/*                    Debug directory                                         */
/*                                                                            */
/*----------------------------------------------------------------------------*/

static dword DebugDirStart, DebugDirLen = 0;

/*----------------------------------------------------------------------------*/
/*                                                                            */
/*                    Description directory                                   */
/*                                                                            */
/*----------------------------------------------------------------------------*/

static dword DescStart, DescLen = 0;

/*----------------------------------------------------------------------------*/
/*                                                                            */
/*                    Main procedure - form and write PE                      */
/*                                                                            */
/*----------------------------------------------------------------------------*/

static FILE * OutFile = NULL;

static dword nbytes = 0;
static char* output_file_name = NULL;

static void WriteOutFile (const void * buf, dword size)
{
    nbytes += size;
    if (fwrite (buf, size, 1, OutFile) != 1) {
        fclose (OutFile);
        Message(xFATAL, msgUNABLE_TO_WRITE_FILE, output_file_name);
    }
}

/*----------------------------------------------------------------------------*/

static byte * filler;

static void AlignOutFile (void)
{
    dword len = ((nbytes + xFileAlign - 1) & ~ (xFileAlign - 1)) - nbytes;

    if (len > 0)
        WriteOutFile (filler, len);
}

/*----------------------------------------------------------------------------*/

IMAGE_FILE_HEADER     Header;
IMAGE_OPTIONAL_HEADER OptionalHeader;
IMAGE_DEBUG_DIRECTORY DebugDir;

#define MAX_NUMBER_OF_SECTIONS 32

IMAGE_SECTION_HEADER sections[MAX_NUMBER_OF_SECTIONS];
int numberOfSections = 0;

dword filePointer;

class PESection {
  private:
    int       index;
    dword     fileOffset;
    byte    * Ptr;
    dword     Len;

  public:

    PESection (dword vaddr, byte *ptr, dword len, const char *name, dword attr, int DirEntry = -1, dword DirEntrySize = 0) {
        index = numberOfSections++;

        ASSERT (numberOfSections <= MAX_NUMBER_OF_SECTIONS);

        Ptr = ptr;
        Len = len;

        dword size = (len + xFileAlign - 1) & ~ (xFileAlign - 1);

        IMAGE_SECTION_HEADER * sec = &(sections[index]);

        ASSERT (strlen (name) < IMAGE_SIZEOF_SHORT_NAME);
        strcpy ((char *)(sec -> Name), name);
        sec -> Misc.VirtualSize = (len + xObjectOffset - 1) & ~ (xObjectOffset - 1);
        sec -> VirtualAddress   = vaddr - xImageBase;
        sec -> Characteristics  = attr;

        if (Ptr) {
            fileOffset = filePointer;
            sec -> PointerToRawData = fileOffset;
            sec -> SizeOfRawData    = size;
            filePointer += size;
        } else {
            fileOffset = 0;
            sec -> PointerToRawData = 0;
            sec -> SizeOfRawData    = 0;
        }

        if (DirEntry >= 0) {
            ASSERT (DirEntry < IMAGE_NUMBEROF_DIRECTORY_ENTRIES);

            OptionalHeader.DataDirectory [DirEntry].VirtualAddress = vaddr - xImageBase;
            OptionalHeader.DataDirectory [DirEntry].Size           = DirEntrySize;
        }
    }

    dword getSize (void) {
        return sections [index].Misc.VirtualSize;
    }

    void WriteSection (void) {
        if (Ptr) {
            VerboseMessage (INFO_SECTIONSIZE, "PE: %s Length %d\n", sections[index].Name, Len);
            WriteOutFile (Ptr, Len);
            AlignOutFile ();
        }
    }
};

/*----------------------------------------------------------------------------*/

dword PE_getOffset (dword vadr, word object) {
    ASSERT ((object > 0) && (object <= numberOfSections));
    return vadr - (sections[object-1].VirtualAddress + xImageBase);
}

word PE_getObjectNumber (dword vadr) {

    for (int i = 0; i < numberOfSections; i++) {
        dword min_adr = sections[i].VirtualAddress + xImageBase;
        dword max_adr = sections[i].VirtualAddress + xImageBase + sections[i].Misc.VirtualSize;
        if ((vadr >= min_adr) && (vadr < max_adr))
            return (word) (i + 1);
    }

    /* There is no such object */
    ASSERT_FALSE();
    return 0;
}

/*----------------------------------------------------------------------------*/

#define IMAGE_DLL_PROCESS_INIT   0x01
#define IMAGE_DLL_PROCESS_TERM   0x02
#define IMAGE_DLL_THREAD_INIT    0x04
#define IMAGE_DLL_THREAD_TERM    0x08

void WritePE (char * name)
{
    CreateDLLTables ();

    if (xCreateTimeDateStamps)
        TimeDateStamp = time (0);
    else
        TimeDateStamp = 0U;

    // Form J Export Image
    if (xJetComponent || (nJExportGroups > 0)) {
        JExportStart = VASA->getVirtualAddr();
        FormJExportImage ();
        JExportLen = JExportImage -> Index;
        VASA->allocateVirtualSpace (JExportStart, JExportLen);
    }

    CreateExport (name);
    CreateRelocations ();

    FILE * fp;

    memset (&Header,         0, sizeof(Header));
    memset (&OptionalHeader, 0, sizeof(OptionalHeader));
    memset (&DebugDir,       0, sizeof(DebugDir));
    memset (sections,        0, sizeof(sections));

    DebugDir.Characteristics  = 0;
    DebugDir.TimeDateStamp    = TimeDateStamp;
    DebugDir.MajorVersion     =
    DebugDir.MinorVersion     = 0;
    DebugDir.Type             = DebugFormat & DBG_FMT_CV ? IMAGE_DEBUG_TYPE_CODEVIEW : IMAGE_DEBUG_TYPE_UNKNOWN;
    DebugDir.SizeOfData       = 0;
    DebugDir.AddressOfRawData = 0;
    DebugDir.PointerToRawData = 0;

    filler = (byte *) xalloc (xFileAlign - 1);
    memset (filler, 0, xFileAlign - 1);

    // Determine Entry Point Address
    dword start = 0;
    if (xWasEntryPoint) {
        ASSERT (EntryPoint != NULL);

        ident i;
        switch (EntryPoint->k_target) {
            case TK_SEG:
                start = ((Segment *) EntryPoint->target) -> address;
                break;
            case TK_ID:
                i = (ident) (EntryPoint->target);
                start = ((nameInfo *) (NAMES.getInfo (i))) -> offset;
                break;
            default:
                Message (xFATAL, msgINVALID_ENTRY_POINT_TARGET);
        }
        start += EntryPoint->fx_offset;
    }

    // Create Section Table
    int nsecs =  (CodeLen > 0) +
                 (DataLen > 0) +
                 (BSSLen > 0) +
                 (RDataLen > 0) +
                 (JImportLen > 0) +
                 (IdataLen > 0) +
                 (JExportLen > 0) +
                 (ExportImage != NULL) +
                 (RelLen > 0) +
                 (numberOfResourceDirectories != 0) +
                 (Description != NULL) +
                 (xDoDebug != 0) +
                 (CPB != NULL);

    dword headers_size = (StubSize +
                          4 +                      // PE Signature
                          sizeof (Header) +
                          sizeof (OptionalHeader) +
                          sizeof (IMAGE_SECTION_HEADER) * nsecs +
                          xFileAlign - 1) & ~ (xFileAlign - 1);

    filePointer = headers_size;

    // Code Section
    PESection * CodeSection = NULL;
    if (CodeLen > 0) {
        CodeSection = new PESection (CodeStart, CodePtr, CodeLen, ".text",
                  IMAGE_SCN_CNT_CODE    +
                  IMAGE_SCN_MEM_EXECUTE +
                  IMAGE_SCN_MEM_READ    +
                  (xWritableCodeSection ? IMAGE_SCN_MEM_WRITE : 0));
    }
 
    // Data Section
    PESection * DataSection = NULL;
    if (DataLen > 0) {
        DataSection = new PESection (DataStart, DataPtr, DataLen, ".data",
                  IMAGE_SCN_CNT_INITIALIZED_DATA +
                  IMAGE_SCN_MEM_READ             +
                  IMAGE_SCN_MEM_WRITE);
    }

    // BSS Section
    PESection * BSSSection = NULL;
    if (BSSLen > 0) {
        BSSSection = new PESection (BSSStart, NULL, BSSLen, ".bss",
                 IMAGE_SCN_CNT_UNINITIALIZED_DATA +
                 IMAGE_SCN_MEM_READ               +
                 IMAGE_SCN_MEM_WRITE);
    }

    // Read-only Data Section
    PESection * RDataSection = NULL;
    if (RDataLen > 0) {
        RDataSection = new PESection (RDataStart, RDataPtr, RDataLen, ".rdata",
                   IMAGE_SCN_CNT_INITIALIZED_DATA +
                   IMAGE_SCN_MEM_READ             +
                   IMAGE_SCN_MEM_SHARED);
    }

    // J Import Section
    PESection * JImportSection = NULL;
    if (JImportLen > 0) {
        JImportSection = new PESection (JImportStart, JImportImage->Ptr, JImportLen, ".jidata",
                  IMAGE_SCN_CNT_INITIALIZED_DATA +
                  IMAGE_SCN_MEM_READ             +
                  IMAGE_SCN_MEM_WRITE);
    }

    // Import (Idata) Section
    PESection * IdataSection = NULL;
    if (IdataLen > 0) {
        IdataSection = new PESection (IdataStart, ImportImage->Ptr, IdataLen, ".idata",
                  IMAGE_SCN_CNT_INITIALIZED_DATA +
                  IMAGE_SCN_MEM_READ             +
                  IMAGE_SCN_MEM_WRITE,
                  IMAGE_DIRECTORY_ENTRY_IMPORT, IdataLen);
    }

    // J Export Section
    PESection * JExportSection = NULL;
    if (JExportLen > 0) {
        JExportSection = new PESection (JExportStart, JExportImage->Ptr, JExportLen, JExportSectionName,
                  IMAGE_SCN_CNT_INITIALIZED_DATA +
                  IMAGE_SCN_MEM_READ);
    }

    // Export Section
    PESection * ExportSection = NULL;
    if (ExportImage) {
        ExportSection = new PESection (ExportStart, ExportImage->Ptr, ExportImage->Index, ".edata",
                  IMAGE_SCN_CNT_INITIALIZED_DATA +
                  IMAGE_SCN_MEM_READ,
                  IMAGE_DIRECTORY_ENTRY_EXPORT,
                  ExportImage->Index);
    }

    // Relocations Section
    PESection * RelocSection = NULL;
    if (RelLen > 0) {
        RelocSection = new PESection (RelStart, RelocImage->Ptr, RelocImage->Index, ".reloc",
                   IMAGE_SCN_CNT_INITIALIZED_DATA +
                   IMAGE_SCN_MEM_DISCARDABLE      +
                   IMAGE_SCN_MEM_READ,
                   IMAGE_DIRECTORY_ENTRY_BASERELOC,
                   RelLen);
    }

    // Resources Section
    PESection * ResourceSection = NULL;
    if (numberOfResourceDirectories != 0) {
        ResStart = VASA->getVirtualAddr();
        CalcResLen ();
        CreateResImage ();
        VASA->allocateVirtualSpace (ResStart, ResLen);
        
        ResourceSection = new PESection (ResStart, ResImage, ResLen, ".rsrc",
                  IMAGE_SCN_CNT_INITIALIZED_DATA +
                  IMAGE_SCN_MEM_READ,
                  IMAGE_DIRECTORY_ENTRY_RESOURCE, ResLen);
    }

    // Description Section
    PESection * DescriptionSection = NULL;
    if (Description) {
        DescStart = VASA->getVirtualAddr();
        DescLen   = strlen (Description) + 1;
        VASA->allocateVirtualSpace (DescStart, DescLen);

        DescriptionSection = new PESection (DescStart, (byte *) Description, DescLen, ".desc",
                  IMAGE_SCN_CNT_INITIALIZED_DATA +
                  IMAGE_SCN_MEM_READ,
                  IMAGE_DIRECTORY_ENTRY_COPYRIGHT, DescLen);
    }

    // Debug Directory Section
    PESection * DebugDirSection = NULL;
    if (xDoDebug) {
        DebugDirStart = VASA->getVirtualAddr();
        DebugDirLen   = sizeof (IMAGE_DEBUG_DIRECTORY);
        VASA->allocateVirtualSpace (DebugDirStart, DebugDirLen);

        DebugDirSection = new PESection (DebugDirStart, (byte *) &DebugDir, DebugDirLen, ".debug",
                   IMAGE_SCN_CNT_INITIALIZED_DATA +
                   IMAGE_SCN_MEM_DISCARDABLE      +
                   IMAGE_SCN_MEM_READ,
                   IMAGE_DIRECTORY_ENTRY_DEBUG, DebugDirLen);
    }

    // Control Parameter Block
    PESection * CPBSection = NULL;
    if (CPB != NULL) {
        dword CPBStart = VASA->getVirtualAddr();
        VASA->allocateVirtualSpace (CPBStart, CPB->Index);

        CPBSection = new PESection (CPBStart, CPB->Ptr, CPB->Index, ".config",
                   IMAGE_SCN_CNT_INITIALIZED_DATA +
                   IMAGE_SCN_MEM_READ             +
                   IMAGE_SCN_MEM_SHARED);
    }

    ASSERT (numberOfSections == nsecs);

    // Form PE header/optional header

    Header . Machine              = IMAGE_FILE_MACHINE_I386;
    Header . NumberOfSections     = (WORD) numberOfSections;
    Header . TimeDateStamp        = TimeDateStamp;
    Header . PointerToSymbolTable = 0;
    Header . NumberOfSymbols      = 0;
    Header . SizeOfOptionalHeader = IMAGE_SIZEOF_NT_OPTIONAL_HEADER;
    Header . Characteristics      = IMAGE_FILE_EXECUTABLE_IMAGE +
                                    IMAGE_FILE_32BIT_MACHINE;
    if (xDLLFlag)
        Header . Characteristics |= IMAGE_FILE_DLL;

    if (!xDoDebug)
        Header . Characteristics |= IMAGE_FILE_DEBUG_STRIPPED +
                                    IMAGE_FILE_LINE_NUMS_STRIPPED +
                                    IMAGE_FILE_LOCAL_SYMS_STRIPPED;

    if (xLargeAddressAware)
        Header . Characteristics |= IMAGE_FILE_LARGE_ADDRESS_AWARE;

    OptionalHeader . Magic                   = 0x010B;
    OptionalHeader . MajorLinkerVersion      = 3;
    OptionalHeader . MinorLinkerVersion      = 0;
    OptionalHeader . SizeOfCode              = CodeSection ? CodeSection->getSize() : 0;
    OptionalHeader . SizeOfInitializedData   = DataSection ? DataSection->getSize() : 0;
    OptionalHeader . SizeOfUninitializedData = BSSSection  ? BSSSection ->getSize() : 0;
    OptionalHeader . AddressOfEntryPoint     = xWasEntryPoint ? start - xImageBase : 0;
    OptionalHeader . BaseOfCode              = CodeStart - xImageBase;
    OptionalHeader . BaseOfData              = DataStart - xImageBase;

    OptionalHeader . ImageBase               = xImageBase;
    OptionalHeader . SectionAlignment        = xObjectOffset;
    OptionalHeader . FileAlignment           = xFileAlign;

    OptionalHeader . MajorOperatingSystemVersion = 1;
    OptionalHeader . MinorOperatingSystemVersion = 0;

    OptionalHeader . MajorImageVersion = 0;
    OptionalHeader . MinorImageVersion = 0;

    OptionalHeader . MajorSubsystemVersion = (word) xOSmajor;
    OptionalHeader . MinorSubsystemVersion = (word) xOSminor;

    OptionalHeader . SizeOfImage         = ((VASA->getVirtualAddr () - xImageBase) + xObjectOffset - 1) & ~ (xObjectOffset - 1);
    OptionalHeader . SizeOfHeaders       = headers_size;
    OptionalHeader . CheckSum            = 0;
    OptionalHeader . Subsystem           = (xSystem == xSUBSYSTEM_CUI ?
                                            IMAGE_SUBSYSTEM_WINDOWS_CUI :
                                            IMAGE_SUBSYSTEM_WINDOWS_GUI);
    OptionalHeader . DllCharacteristics  = xWasEntryPoint ? IMAGE_DLL_PROCESS_INIT || IMAGE_DLL_PROCESS_TERM : 0;
    OptionalHeader . SizeOfStackReserve  = xStackSize;
    OptionalHeader . SizeOfStackCommit   = xStackCommit;
    OptionalHeader . SizeOfHeapReserve   = xHeapSize;
    OptionalHeader . SizeOfHeapCommit    = xHeapCommit;
    OptionalHeader . LoaderFlags         = 0;
    OptionalHeader . NumberOfRvaAndSizes = IMAGE_NUMBEROF_DIRECTORY_ENTRIES;

    // Write output file
    VerboseMessage("Write PE: start writing output file %s\n", name);
    OutFile = fp = fopen (name, "wb");
    if (OutFile == NULL)
        Message(xFATAL, msgUNABLE_TO_OPEN_FILE, name);

    output_file_name = dup(name, strlen(name)+1);

    WriteOutFile (Stub, StubSize);
    WriteOutFile ("PE\0\0", 4);
    WriteOutFile (& Header,         sizeof (Header));
    WriteOutFile (& OptionalHeader, sizeof (OptionalHeader));
    WriteOutFile (  sections,       sizeof (IMAGE_SECTION_HEADER) * numberOfSections);
    AlignOutFile ();

    if (CodeSection) {
        CodeSection->WriteSection();
        xfree (CodePtr);
    }
    if (DataSection) {
        DataSection->WriteSection();
        xfree (DataPtr);
    }
    if (RDataSection) {
        RDataSection->WriteSection();
        xfree (RDataPtr);
    }
    if (JImportSection) {
        JImportSection->WriteSection();
        delete JImportImage;
    }
    if (IdataSection) {
        IdataSection->WriteSection();
        delete ImportImage;
    }
    if (JExportSection) {
        JExportSection->WriteSection();
        delete JExportImage;
    }
    if (ExportSection) {
        ExportSection->WriteSection();
        delete ExportImage;
    }
    if (RelocSection) {
        RelocSection->WriteSection();
        delete RelocImage;
    }
    if (ResourceSection) {
        ResourceSection->WriteSection();
        xfree (ResImage);
    }
    if (DescriptionSection) {
        DescriptionSection->WriteSection();
        xfree (Description);
    }
    if (xDoDebug) {
        CreateDebugInfo();

        if (CPBSection) {
            DebugDir.PointerToRawData = ((( (((nbytes
                        + DebugDirLen ) + xFileAlign - 1) & ~ (xFileAlign - 1))
                        + CPB->Index  ) + xFileAlign - 1) & ~ (xFileAlign - 1));
        } else {
            DebugDir.PointerToRawData = ((nbytes
                        + DebugDirLen ) + xFileAlign - 1) & ~ (xFileAlign - 1);
        }

        DebugDir.SizeOfData = DebugInfo->Index;
        DebugDirSection->WriteSection ();
    }
    if (CPBSection) {
        CPBSection->WriteSection();
        delete CPB;
    }
    if (xDoDebug) {
        ASSERT (DebugDir.PointerToRawData == nbytes);

        VerboseMessage(INFO_SECTIONSIZE, "PE: DEBUG Info Length %d\n", DebugInfo->Index);
        WriteOutFile  (DebugInfo->Ptr, DebugInfo->Index);
        delete DebugInfo;
    }

    if (fclose (OutFile))
        Message(xFATAL, msgUNABLE_TO_WRITE_FILE, name);

    xfree (filler);
    xfree (output_file_name);
}

close_namespace

