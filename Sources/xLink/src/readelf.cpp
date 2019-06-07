
#include "xdefs.h"

open_namespace

#include "struct.h"
#include "idents.h"
#include "messages.h"
#include "xmem.h"
#include "xelf.h"
#include "readelf.h"
#include "xos.h"

#include "implib.h"

/*----------------------------------------------------------------------------*/

#define LinuxExtensions    /* Use ELF Linux Extensions */

/*----------------------------------------------------------------------------*/

struct ELFVerDef {
    Elf32_Half ndx;
    ident id;
    struct ELFVerDef * next;
};


class ELFReader {
  private:
    byte * rawdata;
    dword  size;
    char * filename;
    ident  dllname;

    Elf32_Ehdr * ehdr;

    Elf32_Phdr * phdr;
    int          nseg;

    Elf32_Shdr * shdr;
    int          nsec;

    struct ELFVerDef * verdefs;

  // ------------------------------------------- //

    void readJExportSection (dword offs, dword sz);
    dword va2offs (Elf32_Addr va);
    void ReadSharedObject ();

    void  defineVersion (Elf32_Half ndx, char * name);
    ident findVersion   (Elf32_Half ndx);
    void  freeVersions  ();

  public:

    void Read (byte * rawdata, long size, char * filename);
};


dword ELFReader::va2offs (Elf32_Addr _va)
{
    dword va = (dword)_va;

    for (int i = 0; i < nseg; i++) {
        dword vaddr = (dword) (phdr[i].p_vaddr);

        if ((vaddr <= va) &&
            (va < vaddr + phdr[i].p_memsz))
        {
            dword delta = va - vaddr;

            if (delta < phdr[i].p_filesz) {
                return phdr[i].p_offset + delta;
            }
        }
    }

    VerboseMessage ("Unable to convert VA %x to offset\n", va);
    Message(xERROR, msgINVALID_ELF, filename);
    return 0;
}


void ELFReader::defineVersion (Elf32_Half ndx, char * name)
{
    VerboseMessage (INFO_ELFREAD, "ELFREAD: VERDEF %d %s\n", ndx, name);

    struct ELFVerDef * verdef = (struct ELFVerDef *) xalloc (sizeof (struct ELFVerDef));
    verdef -> ndx = ndx;
    verdef -> id  = NAMES.Str2Index (name);

    // skip entries with the same .ndx
    struct ELFVerDef * prev = NULL;
    struct ELFVerDef * cur  = verdefs;
    while ((cur != NULL) && (cur -> ndx == ndx)) {
        prev = cur;
        cur  = cur -> next;
    }

    // insert after prev
    if (prev) {
        verdef -> next = prev -> next;
        prev -> next = verdef;
    } else {
        verdef -> next = verdefs;
        verdefs = verdef;
    }
}


ident ELFReader::findVersion (Elf32_Half ndx)
{
    for (struct ELFVerDef * verdef = verdefs; verdef != NULL; verdef = verdef -> next)
    {
        if (verdef -> ndx == ndx)
            return verdef -> id;
    }

    return INVALID_ID;
}

void ELFReader::freeVersions ()
{
    struct ELFVerDef * verdef = verdefs;
    while (verdef != NULL)
    {
        struct ELFVerDef * next = verdef -> next;
        xfree (verdef);
        verdef = next;
    }
}

void ELFReader::readJExportSection (dword offs, dword sz)
{
    if ((size < offs + sz) || (sz < 8)) {
        VerboseMessage ("Invalid jexport section: total size %x, sec offs %x, sec size %x\n", size, offs, sz);
        Message(xERROR, msgINVALID_ELF, filename);
        return;
    }

    byte * jedata = rawdata + offs;

    if (* ((dword *)jedata) != JE_MAGIC)
        return;

    dword cKey = * ((dword *) (jedata + 4));

    struct JImportDLL * dll = getJImportDLL (dllname);
    dll -> cKey = cKey;
}


void ELFReader::ReadSharedObject ()
{
    // process DYNAMIC program header

    for (int j = 0; j < nseg; j++) {
        if (phdr[j].p_type == PT_DYNAMIC) {
            if (phdr[j].p_offset + phdr[j].p_filesz > size) {
                VerboseMessage ("Invalid PHDR: total size %x, seg offs %x, seg size %x\n", size, phdr[j].p_offset, phdr[j].p_filesz);
                Message(xERROR, msgINVALID_ELF, filename);
                return;
            }

            Elf32_Dyn * dynamic = (Elf32_Dyn *) (rawdata + phdr[j].p_offset);
            dword dynSize = phdr[j].p_filesz;

            Elf32_Addr hashVA   = 0;
            Elf32_Addr dynstrVA = 0;
            Elf32_Addr dynsymVA = 0;
            dword dynstrSZ = 0;
            dword sonameID = 0;

#ifdef LinuxExtensions
            Elf32_Addr versymVA = 0;
            Elf32_Addr verdefVA = 0;
            dword      verdefNM = 0;
#endif


            for (;;dynamic++, dynSize-=sizeof (Elf32_Dyn)) {
                if (dynSize < sizeof (Elf32_Dyn)) {
                    VerboseMessage (INFO_ELFREAD, "Error #1\n");
                    Message(xERROR, msgINVALID_ELF, filename);
                    return;
                }

                if (dynamic -> d_tag == DT_NULL)
                    break;

                switch (dynamic -> d_tag) {
                    case DT_HASH:        hashVA   = dynamic -> d_un.d_ptr; break;
                    case DT_STRTAB:      dynstrVA = dynamic -> d_un.d_ptr; break;
                    case DT_SYMTAB:      dynsymVA = dynamic -> d_un.d_ptr; break;
                    case DT_STRSZ:       dynstrSZ = dynamic -> d_un.d_val; break;
                    case DT_SONAME:      sonameID = dynamic -> d_un.d_val; break;

#ifdef LinuxExtensions
                    case DT_VERSYM:      versymVA = dynamic -> d_un.d_ptr; break;
                    case DT_VERDEFNUM:   verdefNM = dynamic -> d_un.d_val; break;
                    case DT_VERDEF:      verdefVA = dynamic -> d_un.d_ptr; break;
#endif

                    case DT_SYMENT:
                            if (dynamic -> d_un.d_val != sizeof (Elf32_Sym)) {
                                VerboseMessage (INFO_ELFREAD, "Error #2 (DT_SYMENT = %d, sz = %d)\n", dynamic -> d_un.d_val, sizeof (Elf32_Sym));
                                Message(xERROR, msgINVALID_ELF, filename);
                                return;
                            }
                            break;
                }
            }

            if ((hashVA   == 0) ||
                (dynstrVA == 0) ||
                (dynsymVA == 0) ||
                (dynstrSZ == 0))
            {
                VerboseMessage (INFO_ELFREAD, "Error #3\n");
                Message(xERROR, msgINVALID_ELF, filename);
                return;
            }

            dword hashOffs   = va2offs (hashVA);
            dword dynstrOffs = va2offs (dynstrVA);
            dword dynsymOffs = va2offs (dynsymVA);

            if ((hashOffs + 2*sizeof (Elf32_Word) > size) ||
                (!xNoDynstrSizeCheck && (dynstrOffs + dynstrSZ > size)))
            {
                VerboseMessage (INFO_ELFREAD, "Error #4\n");
                Message(xERROR, msgINVALID_ELF, filename);
                return;
            }

            // extract number of symbols from the ELF hash table
            dword nsym = ((Elf32_Word *) (rawdata + hashOffs)) [1];

            // extract string table
            char * dynstr = (char *) (rawdata + dynstrOffs);

            if (sonameID != 0) {
                if (!xNoDynstrSizeCheck && (sonameID >= dynstrSZ)) {
                    VerboseMessage ("sonameID (%d) is larger than .dynstr size (%d)\n", sonameID, dynstrSZ);
                    Message(xERROR, msgINVALID_ELF, filename);
                    return;
                }

                // get SONAME
                dllname = NAMES.Str2Index (dynstr + sonameID);
            }

            // check zero-termination of string table,
            // size of symbol table
            // and soname index in string table
            if ((dynstr [dynstrSZ-1] != 0) ||
                (dynsymOffs + sizeof(Elf32_Sym) * nsym > size))
            {
                VerboseMessage (INFO_ELFREAD, "Error #5\n");
                Message(xERROR, msgINVALID_ELF, filename);
                return;
            }

#ifdef LinuxExtensions
            // parse version definitions

            dword versymOffs = 0;
            verdefs = NULL;

            if (versymVA != 0) {
                versymOffs = va2offs (versymVA);
                VerboseMessage (INFO_ELFREAD, "VERSYM offset %x\n", versymOffs);
                if (versymOffs + nsym*sizeof(Elf32_Half) > size) {
                    VerboseMessage (INFO_ELFREAD, "Error #9\n");
                    Message(xERROR, msgINVALID_ELF, filename);
                    return;
                }
            }

            if ((versymVA != 0) && (verdefVA != 0) && (verdefNM != 0)) {
                dword verdefOffs = va2offs (verdefVA);
                for (dword i = 0; i < verdefNM; i++) {
                    if (verdefOffs + sizeof(Elf32_Verdef) > size) {
                        VerboseMessage (INFO_ELFREAD, "Error #10\n");
                        Message(xERROR, msgINVALID_ELF, filename);
                        return;
                    }

                    Elf32_Verdef * verdef = (Elf32_Verdef *) (rawdata + verdefOffs);
                    VerboseMessage (INFO_ELFREAD, "VERDEF at offset 0x%x\n", verdefOffs);

                    if (verdef -> vd_version != VER_DEF_CURRENT) {
                        VerboseMessage (INFO_ELFREAD, "Error #11\n");
                        Message(xERROR, msgINVALID_ELF, filename);
                        return;
                    }

                    dword verdauxOffs = verdefOffs + verdef->vd_aux;
                    for (int j = 0; j < verdef->vd_cnt; j++) {
                         if (verdauxOffs + sizeof(Elf32_Verdaux) > size) {
                             VerboseMessage (INFO_ELFREAD, "Error #12\n");
                             Message(xERROR, msgINVALID_ELF, filename);
                             return;
                         }

                         VerboseMessage (INFO_ELFREAD, "VERDAUX at offset 0x%x\n", verdauxOffs);
                         Elf32_Verdaux * verdaux = (Elf32_Verdaux *) (rawdata + verdauxOffs);

                         if (!xNoDynstrSizeCheck && (verdaux->vda_name > dynstrSZ)) {
                             VerboseMessage (INFO_ELFREAD, "Error #13: verdaux name 0x%x > .dynstr size 0x%x\n", verdaux->vda_name, dynstrSZ);
                             Message(xERROR, msgINVALID_ELF, filename);
                             return;
                         }

                         defineVersion (verdef->vd_ndx, dynstr + verdaux->vda_name);
                         verdauxOffs = verdauxOffs + verdaux->vda_next;
                    }

                    verdefOffs = verdefOffs + verdef->vd_next;
                }
            }
#endif

            // parse symbol table

            Elf32_Sym * dynsym = (Elf32_Sym *) (rawdata + dynsymOffs);
            for (unsigned k = 0; k < nsym; k++) {
                if (((ELF32_ST_BIND (dynsym[k].st_info) == STB_GLOBAL) ||
                     (ELF32_ST_BIND (dynsym[k].st_info) == STB_WEAK)) &&

                    ((ELF32_ST_TYPE (dynsym[k].st_info) == STT_OBJECT) ||
                     (ELF32_ST_TYPE (dynsym[k].st_info) == STT_FUNC)) &&

                    (dynsym[k].st_shndx != SHN_UNDEF)  &&
                    (dynsym[k].st_shndx != SHN_COMMON))
                {
                    // valid symbol for dynamic linking

                    if (!xNoDynstrSizeCheck && (dynsym[k].st_name >= dynstrSZ)) {
                        VerboseMessage (INFO_ELFREAD, "Error #6\n");
                        Message(xERROR, msgINVALID_ELF, filename);
                        return;
                    }

                    ident id = NAMES.Str2Index (dynstr + dynsym[k].st_name);
                    VerboseMessage (INFO_ELFREAD, "ELFREAD: DYNSYM: %s\n", NAMES.Index2Str(id));

                    ident version = INVALID_ID;
                    Bool hidden = false;
#ifdef LinuxExtensions
                    if (versymVA != 0) {
                        Elf32_Half ver = ((Elf32_Half *) (rawdata + versymOffs)) [k];
                        if ((ver != VER_NDX_LOCAL) && (ver != VER_NDX_GLOBAL) && (ver < VER_NDX_LORESERVE)) {
                            version = findVersion (ver);
                        }
                        if (ver & 0x8000) {
                            VerboseMessage (INFO_ELFREAD, "ELFREAD: DYNSYM: %s, hidden by version %x\n", NAMES.Index2Str(id), (int)ver);
                            hidden = true;
                        }
                    }
#endif

                    if (ELF32_ST_BIND (dynsym[k].st_info) == STB_WEAK) {
                        nameInfo * info = (nameInfo *) (NAMES.getInfo (id));
                        if (info != NULL) {
                            VerboseMessage (INFO_ELFREAD, "ELFREAD: DYNSYM: %s, hidden by weak\n", NAMES.Index2Str(id));
                            hidden = true;
                        }
                    }

                    if (!hidden) {
                        importNameInfo * info = NewImportName (id, K_IMPORT | ((ELF32_ST_TYPE (dynsym[k].st_info) == STT_FUNC) ? K_IMPFUNC : 0), dllname, /*FIXME*/(int)id, version);

                        if (!xDLLFlag && (info != NULL)) {
                            if (ELF32_ST_TYPE (dynsym[k].st_info) == STT_OBJECT) {
                                // create .bss segment to hold global data object
                                Segment * seg = new Segment (id, dllname, BSS, false, (dynsym[k].st_size == 0) ? 4 : dynsym[k].st_size, 4);
                                info -> seg    = seg;
                                info -> offset = 0;
                            }
                        }
                    }
                }
            }

#ifdef LinuxExtensions
            freeVersions ();
#endif

            break;
        }
    }

    // look for jexport section in section headers

    if (nsec == 0)
        return;

    // find and check .shstrtab

    if ((ehdr->e_shstrndx >= nsec) || (ehdr->e_shstrndx == 0) ||
        (shdr[ehdr->e_shstrndx].sh_type != SHT_STRTAB) ||
        (shdr[ehdr->e_shstrndx].sh_offset + shdr[ehdr->e_shstrndx].sh_size > size))
    {
        VerboseMessage (INFO_ELFREAD, "Error #7\n");
        Message(xERROR, msgINVALID_ELF, filename);
        return;
    }

    char * shstrtab     = (char *) (rawdata + shdr[ehdr->e_shstrndx].sh_offset);
    dword  shstrtabSize = shdr[ehdr->e_shstrndx].sh_size;

    // string table should be zero-terminated

    if (shstrtab [shstrtabSize-1] != 0) {
        VerboseMessage (INFO_ELFREAD, "Error #8\n");
        Message(xERROR, msgINVALID_ELF, filename);
        return;
    }

    // scan section headers, look for jexport & config sections

    Bool jetCompiled = false;

    for (int i = 0; i < nsec; i++) {
        if (shdr[i].sh_name < shstrtabSize) {
            if (!strcmp (shstrtab + shdr[i].sh_name, "jexport")) {
                jetCompiled = true;
                readJExportSection (shdr[i].sh_offset, shdr[i].sh_size);
            } else 
            if (!strcmp (shstrtab + shdr[i].sh_name, "config")) {
                jetCompiled = true;
            }
        }
    }


    if (!jetCompiled) {
        // strictly link to this shared library
        getDLL (dllname);
    }

    if(xWriteGCCImportLibrary)
        WriteGCCImportLibrary(NAMES.Index2Str(dllname));
}


void ELFReader::Read (byte * _rawdata, long _size, char * _filename)
{
    rawdata  = _rawdata;
    size     = (dword)_size;
    filename = _filename;

    // strip path to make dllname
    char * p = strrchr (_filename, OS->FileSep());
    if (p == NULL)
        p = _filename;
    else
        p++;
    dllname  = NAMES.Str2Index (p);

    CurrentFile = new OBJFile (_filename);

    ASSERT (size >= sizeof (Elf32_Ehdr));

    ehdr = (Elf32_Ehdr *) rawdata;

    if ((ehdr -> e_shentsize != sizeof (Elf32_Shdr)) ||
        (ehdr -> e_phentsize != sizeof (Elf32_Phdr)))
    {
        VerboseMessage ("Invalid ELF header sizes (SHDR: %d, should be %d); (PHDR: %d, should be %d)\n",
                        ehdr -> e_shentsize, sizeof (Elf32_Shdr),
                        ehdr -> e_phentsize, sizeof (Elf32_Phdr));
        Message(xERROR, msgINVALID_ELF, filename);
        return;
    }

    // find section headers

    if ((ehdr -> e_shoff != 0) && (ehdr -> e_shnum != 0)) {
        if (size < (ehdr->e_shoff + sizeof(Elf32_Shdr) * ehdr->e_shnum)) {
            VerboseMessage ("Invalid section headers: total size:%x, offs %x, size %x\n",
                            size, ehdr->e_shoff, sizeof(Elf32_Shdr) * ehdr->e_shnum);
            Message(xERROR, msgINVALID_ELF, filename);
            return;
        }
        shdr = (Elf32_Shdr *) (rawdata + ehdr->e_shoff);
        nsec = ehdr -> e_shnum;
    } else {
        shdr = NULL;
        nsec = 0;
    }

    // find program headers

    if ((ehdr -> e_phoff != 0) && (ehdr -> e_phnum != 0)) {
        if (size < (ehdr->e_phoff + sizeof(Elf32_Phdr) * ehdr->e_phnum)) {
            VerboseMessage ("Invalid program headers: total size:%x, offs %x, size %x\n",
                            size, ehdr->e_phoff, sizeof(Elf32_Phdr) * ehdr->e_phnum);
            Message(xERROR, msgINVALID_ELF, filename);
            return;
        }
        phdr = (Elf32_Phdr *) (rawdata + ehdr->e_phoff);
        nseg = ehdr -> e_phnum;
    } else {
        phdr = NULL;
        nseg = 0;
    }

    // parse file depending of its type

    switch (ehdr -> e_type) {
        case ET_DYN:
            ReadSharedObject ();
            break;

        default:
            VerboseMessage ("Unknown ELF .e_type: %d (ET_DYN =  %d)\n", ehdr -> e_type, ET_DYN);
            Message(xERROR, msgINVALID_ELF, filename);
            return;
    }
}

/* ------------------------------------------------------------------------- */

static ELFReader * elfReader;

void InitELFReader  (void) { elfReader = new ELFReader(); }
void ClearELFReader (void) { delete elfReader; }

void ReadELF (byte * rawdata, dword size, char * filename)
{
    elfReader->Read (rawdata, size, filename);
}


Bool IsELF (byte * rawdata, dword size)
{
    if (size < sizeof(Elf32_Ehdr))
        return false;

    Elf32_Ehdr * hdr = (Elf32_Ehdr *)rawdata;

    return (hdr->e_ident [EI_MAG0   ] == ELFMAG0)     &&
           (hdr->e_ident [EI_MAG1   ] == ELFMAG1)     &&
           (hdr->e_ident [EI_MAG2   ] == ELFMAG2)     &&
           (hdr->e_ident [EI_MAG3   ] == ELFMAG3)     &&
           (hdr->e_ident [EI_CLASS  ] == ELFCLASS32)  &&
           (hdr->e_ident [EI_DATA   ] == ELFDATA2LSB) &&
           (hdr->e_ident [EI_VERSION] == EV_CURRENT)  &&
           (hdr->e_machine == EM_386);
}

close_namespace

