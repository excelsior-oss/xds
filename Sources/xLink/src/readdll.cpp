#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "xdefs.h"

open_namespace

#include "struct.h"
#include "idents.h"
#include "messages.h"
#include "xmem.h"
#include "xpe.h"
#include "xos.h"
#include "readdll.h"

#define CodeSectionCharacteristics (IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_EXECUTE)

class DLLReader {
  private:
    ident  dllname;
    byte * rawdata;
    long   size;

    PIMAGE_NT_HEADERS pNTHeader;

  // ------------------------------------------- //

    ident getDLLName (const char * filename);

    byte isCode (dword address);

    PIMAGE_SECTION_HEADER findSectionByVA   (dword va);
    PIMAGE_SECTION_HEADER findSectionByName (char *secName);

    void readExportSection  (PIMAGE_SECTION_HEADER sec);
    void readJExportSection (PIMAGE_SECTION_HEADER sec);

  public:
    void Read (byte * rawdata, long size, const char * filename);
};


byte DLLReader::isCode (dword address)
{
    PIMAGE_SECTION_HEADER s;
    int n;

    for (s = (PIMAGE_SECTION_HEADER) (pNTHeader + 1),
         n = pNTHeader -> FileHeader.NumberOfSections; n; s++, n--)
    {
        if ((address >= s->VirtualAddress) && (address < (s->VirtualAddress + s->Misc.VirtualSize))) {
            if ((s -> Characteristics & CodeSectionCharacteristics) == CodeSectionCharacteristics)
                return K_IMPFUNC;
            else
                return 0;
        }
    }
    return 0;
}


PIMAGE_SECTION_HEADER DLLReader::findSectionByVA (dword va)
{
    PIMAGE_SECTION_HEADER sec;
    int n;

    for (sec = (PIMAGE_SECTION_HEADER) (pNTHeader + 1),
         n = pNTHeader -> FileHeader.NumberOfSections; n > 0; sec++, n--)
    {
        if (sec -> VirtualAddress == va)
            return sec;
    }

    return NULL;
}


PIMAGE_SECTION_HEADER DLLReader::findSectionByName (char *secName)
{
    ASSERT (strlen (secName) <= IMAGE_SIZEOF_SHORT_NAME);

    PIMAGE_SECTION_HEADER sec;
    int n;

    for (sec = (PIMAGE_SECTION_HEADER) (pNTHeader + 1),
         n = pNTHeader -> FileHeader.NumberOfSections; n > 0; sec++, n--)
    {
        if (!strncmp((char *)(sec->Name), secName, IMAGE_SIZEOF_SHORT_NAME))
            return sec;
    }

    return NULL;
}


ident DLLReader::getDLLName (const char * filename)
{
    char buf [1024];
    ASSERT (strlen (filename) < sizeof (buf));

    const char * r = strrchr (filename, '\\');
    if (r == NULL)
        r = strrchr (filename, '/');
    if (r == NULL)
        strcpy (buf, filename);
    else
        strcpy (buf, r + 1);

    return NAMES.Str2Index (ConvDLLName (buf));
}


void DLLReader::readExportSection (PIMAGE_SECTION_HEADER sec)
{
    PIMAGE_EXPORT_DIRECTORY exportDir = (PIMAGE_EXPORT_DIRECTORY) (rawdata + (DWORD) (sec -> PointerToRawData));

    byte * edata = rawdata + sec->PointerToRawData;

    PSTR  * name    = (PSTR  *)(edata + (DWORD) (exportDir->AddressOfNames)        - (DWORD) sec->VirtualAddress);
    word  * ordinal = (word  *)(edata + (DWORD) (exportDir->AddressOfNameOrdinals) - (DWORD) sec->VirtualAddress);
    dword * address = (dword *)(edata + (DWORD) (exportDir->AddressOfFunctions)    - (DWORD) sec->VirtualAddress);

    for (int n = exportDir -> NumberOfNames; n > 0; name ++, ordinal ++, n --) {
        if (* name == 0)
            continue;
        ident id = NAMES.Str2Index ((char *) (edata + (DWORD) (* name) - (DWORD) sec->VirtualAddress));
        if (id != COMPONENT_KEY)
            NewImportName (id, isCode (address[*ordinal]) | K_IMPORT, dllname, /*FIXME*/(int)id);
    }
}


void DLLReader::readJExportSection (PIMAGE_SECTION_HEADER sec)
{
    if (!sec)
        return;

    byte * jedata = rawdata + sec->PointerToRawData;

    if (* ((dword *)jedata) != JE_MAGIC)
        return;

    dword cKey = * ((dword *) (jedata + 4));

    struct JImportDLL * dll = getJImportDLL (dllname);
    dll -> cKey = cKey;
}


void DLLReader::Read (byte * _rawdata, long _size, const char * filename)
{
    rawdata = _rawdata;
    size    = _size;

    pNTHeader = (PIMAGE_NT_HEADERS) (rawdata + ((PIMAGE_DOS_HEADER) rawdata) -> e_lfanew);
    if ( (((byte *) pNTHeader) - rawdata + sizeof (IMAGE_NT_HEADERS) > (dword) size) ||
         pNTHeader -> Signature != IMAGE_NT_SIGNATURE)
    {
        Message(xERROR, msgINVALID_EXE_TYPE, filename);
        return;
    }

    dllname = getDLLName (filename);

    // Read Export Section
    dword exportVA   = pNTHeader -> OptionalHeader.DataDirectory [IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress;
    dword exportSize = pNTHeader -> OptionalHeader.DataDirectory [IMAGE_DIRECTORY_ENTRY_EXPORT].Size;
    if (exportVA && exportSize) {
        PIMAGE_SECTION_HEADER sec = findSectionByVA (exportVA);
        if (sec)
            readExportSection (sec);
    }

    readJExportSection (findSectionByName (JExportSectionName));
}

static DLLReader * dllReader;

void InitDLLReader  (void) { dllReader = new DLLReader(); }
void ClearDLLReader (void) { delete dllReader; }

void ReadDLL (byte * rawdata, dword size, const char * filename)
{
    dllReader->Read (rawdata, size, filename);
}

int IsDLL (byte * rawdata)
{
    return (((PIMAGE_DOS_HEADER) rawdata) -> e_magic == IMAGE_DOS_SIGNATURE);
}


#define IMAGE_BASE_ALIGN     1*1024*1024

void ReadDLLForAutoImageBase (const char * filename)
{
    OSFile * file = OS -> File ();

    byte * rawdata = NULL;
    unsigned long size = 0;

    if (! file -> OpenRead (filename, rawdata, size))
        return;

    PIMAGE_NT_HEADERS pNTHeader = (PIMAGE_NT_HEADERS) (rawdata + ((PIMAGE_DOS_HEADER) rawdata) -> e_lfanew);
    if ( (((byte *) pNTHeader) - rawdata + sizeof (IMAGE_NT_HEADERS) > (dword) size) ||
         pNTHeader -> Signature != IMAGE_NT_SIGNATURE)
    {
        Message(xERROR, msgINVALID_EXE_TYPE, filename);
        return;
    }

    dword imageBase = pNTHeader->OptionalHeader.ImageBase;

    xImageBase = ((((imageBase + IMAGE_BASE_ALIGN-1) & ~(IMAGE_BASE_ALIGN-1)) + size)
                               + IMAGE_BASE_ALIGN-1) & ~(IMAGE_BASE_ALIGN-1);

    VerboseMessage ("Image base set to %x\n", xImageBase);

    delete file;
}

close_namespace

