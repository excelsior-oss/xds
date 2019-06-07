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
#include "xmem.h"
#include "xelf.h"
#include "debug.h"
#include "xos.h"

#include "writeelf.h"
#include "writer.h"

/*----------------------------------------------------------------------------*/

#define LinuxExtensions    /* Use ELF Linux Extensions */

/*----------------------------------------------------------------------------*/

class ELFStringTable {
  private:
    Storage * buf;
    Bool      locked;

  public:
    ELFStringTable () {
        buf = newStorage (1024);
        buf -> PutB (0);

        locked = false;
    }

    Elf32_Word ID (const char * name) {
        ASSERT (!locked);

        dword index = buf -> Index;
        buf -> PutS ((byte *)name, strlen(name)+1);
        return index;
    }

    Storage * getContents () {
        locked = true;
        return buf;
    }
};


/*----------------------------------------------------------------------------*/


static unsigned long elf_hash (const unsigned char * name) {
    unsigned long h = 0, g;
    while (*name) {
        h = (h << 4) + *name++;
        g = h & 0xf0000000;
        if (g)
            h ^= g >> 24;
        h &= ~g;
    }
    return h;
}

class ELFSymbolTable {
  private:
    ELFStringTable * strtab;
    Storage        * symtable;
    dword            count;

    Bool             computeHash;
    Elf32_Word     * hash;
    dword            hashSz;
    dword            hashCapacity;


    void updateHash (const char * name) {
        if (!computeHash) return;

        Elf32_Word * h;
        Grow (hashSz, hashCapacity, 16, hash, Elf32_Word, h);
        * h = elf_hash ((const unsigned char *)name);
        ASSERT (hashSz == count);
    }

  public:
    ELFSymbolTable (ELFStringTable * _strtab, Bool _computeHash = false) :
        strtab      (_strtab),
        symtable    (newStorage (1024)),
        count       (1),
        computeHash (_computeHash)
    {
        if (computeHash) {
            hashSz       = 0;
            hashCapacity = 16;
            hash         = (Elf32_Word *) xalloc (hashCapacity*sizeof(Elf32_Word));
        }

        // symbol table entry, index 0

        Elf32_Sym sym;

        sym.st_name  = 0;
        sym.st_value = 0;
        sym.st_size  = 0;
        sym.st_info  = 0;
        sym.st_other = 0;
        sym.st_shndx = SHN_UNDEF;

        symtable -> PutS (&sym, sizeof (Elf32_Sym));

        updateHash ("");
    }

    Storage * getHashTable () const {
        ASSERT (computeHash);

        Elf32_Word nbucket = (count / 4) * 2 + 1;

        Elf32_Word * bucket = (Elf32_Word *) xalloc (sizeof(Elf32_Word) * nbucket);
        Elf32_Word * chain  = (Elf32_Word *) xalloc (sizeof(Elf32_Word) * count);

        memset (bucket, 0, sizeof(Elf32_Word) * nbucket);
        memset (chain,  0, sizeof(Elf32_Word) * nbucket);

        for (dword i = 0; i < count; i++) {
            Elf32_Word _hash = hash[i];
            int j = _hash % nbucket;
            chain  [i] = bucket [j];
            bucket [j] = i;
        }

        Storage * hashtable = newStorage (sizeof (Elf32_Word) * (2 + nbucket + count));

        hashtable -> Put4 (nbucket);
        hashtable -> Put4 (count);
        hashtable -> PutS (bucket, sizeof(Elf32_Word) * nbucket);
        hashtable -> PutS (chain,  sizeof(Elf32_Word) * count);

        xfree (bucket);
        xfree (chain);

        return hashtable;
    }

    Storage * getSymTable () const {
        return symtable;
    }

    dword Sym (const char * name,
               dword        value,
               dword        size,
               byte         binding,
               byte         type,
               Elf32_Half   section)
    {
        Elf32_Sym sym;

        sym.st_name  = strtab -> ID (name);
        sym.st_value = (Elf32_Addr) value;
        sym.st_size  = size;
        sym.st_info  = (unsigned char) ELF32_ST_INFO (binding, type);
        sym.st_other = 0;
        sym.st_shndx = section;

        symtable -> PutS (&sym, sizeof (Elf32_Sym));
        dword index = count++;

        updateHash (name);

        return index;
    }
};

/*----------------------------------------------------------------------------*/

struct VerNeedAuxEntry {
    ident      id;
    Elf32_Half vna_other;
};

struct VerNeedEntry {
    ident                    dllname;
    Elf32_Word               dllnameID;
    dword                    cnt;
    dword                    auxSize;
    struct VerNeedAuxEntry * aux;
};

static dword                 NVerNeed    = 0;
static dword                 VerNeedSize = 0;
static struct VerNeedEntry * VerNeed     = NULL;

static dword NVerAux = 0;

static Elf32_Half symVersion (importNameInfo * sym) {
    ident ver = sym -> getVersion ();

    if (ver == INVALID_ID)
        return VER_NDX_GLOBAL;

    ident dllname = sym -> getModuleName();

    struct VerNeedEntry * vn = NULL;
    for (unsigned i = 0; i < NVerNeed; i++) {
        if (VerNeed[i].dllname == dllname) {
            vn = & (VerNeed [i]);
            break;
        }
    }

    if (vn == NULL) {
        Grow (NVerNeed, VerNeedSize, 16, VerNeed, struct VerNeedEntry, vn);

        struct dll * d = findDLL (dllname);
        ASSERT (d -> nameID != 0);

        vn -> dllname   = dllname;
        vn -> dllnameID = d -> nameID;
        vn -> cnt       = 0;
        vn -> auxSize   = 0;
        vn -> aux       = NULL;
    }

    for (unsigned j = 0; j < vn -> cnt; j++) {
        if (vn -> aux [j].id == ver) {
            return vn -> aux [j].vna_other;
        }
    }

    struct VerNeedAuxEntry * ae;
    Grow (vn -> cnt, vn -> auxSize, 16, vn -> aux, struct VerNeedAuxEntry, ae);
    ae -> id        = ver;
    ae -> vna_other = (Elf32_Half) (VER_NEED_NUM + NVerAux++);

    return ae -> vna_other;
}

/*----------------------------------------------------------------------------*/
static dword     entryPointVA = 0;
static dword     _init_VA = 0;
static dword     _fini_VA = 0;

static Storage * gotSection = NULL;
static dword     gotVA      = 0;
static dword     gotSize    = 0;

static Storage * pltSection = NULL;
static dword     pltVA      = 0;
static dword     pltSize    = 0;

static Storage * relpltSection = NULL;
static dword     relpltVA      = 0;
static dword     relpltSize    = 0;

static Storage * reldynSection = NULL;
static dword     reldynVA      = 0;
static dword     reldynSize    = 0;

static Storage * gnuverSection = NULL;
static dword     gnuverVA      = 0;
static dword     gnuverSize    = 0;

static Storage * gnuver_rSection = NULL;
static dword     gnuver_rVA      = 0;
static dword     gnuver_rSize    = 0;

static Storage * hashSection = NULL;
static dword     hashVA      = 0;

static Storage * dynsymSection = NULL;
static dword     dynsymVA      = 0;

static Storage * dynstrSection = NULL;
static dword     dynstrVA      = 0;

static Storage * dynamicSection = NULL;
static dword     dynamicVA      = 0;

#define PLT_ENTRY0_SZ       16
#define PLT_ENTRYi_SZ       16

#define GOT_N_RESERVED       3

static void CreateDynamicInfo (const char* outputname)
{
    if ((Ndlls == 0) && !xDLLFlag && !xJetComponent)
        return;

    unsigned long nImported = 0;
    unsigned long nImpFunc  = 0;
    unsigned long nImpData  = 0;

    // Calculate total number of imported entries,
    // number of imported functions and data

    int i;
    for (i = 0; i < NumberOfImportedNames; i++) {
        importNameInfo * s = (importNameInfo *) (NAMES.getInfo (ImportedNames[i]));
        if (s && ((s -> kind & (K_USED | K_MASK)) == K_USED + K_IMPORT)) {
            nImported++;
            if (s -> kind & K_IMPFUNC)
                nImpFunc++;
            else
                nImpData++;
        }
    }

    /*
        Allocate sections in virtual address space in order:
        .got
        .plt
        .rel.plt
        .rel.dyn
        .gnu.version
        .gnu.version_r         [optional]
        .hash
        .dynsym
        .dynstr
        .dynamic
    */

    gotVA      = VASA -> getVirtualAddr();
    gotSize    = (GOT_N_RESERVED + (xDLLFlag ? nImported : nImpFunc)) * sizeof (Elf32_Addr);
    gotSection = newStorage (gotSize);
    VASA -> allocateVirtualSpace (gotVA, gotSize);

    pltVA      = VASA -> getVirtualAddr();
    pltSize    = PLT_ENTRY0_SZ + nImpFunc*PLT_ENTRYi_SZ;
    pltSection = newStorage (pltSize);
    VASA -> allocateVirtualSpace (pltVA, pltSize);

    relpltVA      = VASA -> getVirtualAddr();
    relpltSize    = nImpFunc * sizeof (Elf32_Rel);
    relpltSection = newStorage (relpltSize);
    VASA -> allocateVirtualSpace (relpltVA, relpltSize);

    reldynVA      = VASA -> getVirtualAddr();
    reldynSize    = (nImpData + (xDLLFlag ? 2 + nImpFunc + NFixups : 0)) * sizeof (Elf32_Rel);
    reldynSection = newStorage (reldynSize);
    VASA -> allocateVirtualSpace (reldynVA, reldynSize);

    if (xDLLFlag) {
        // write R_386_RELATIVE relocations
        for (int i = 0; i < NFixups; i++) {
            Elf32_Rel rel;
            rel.r_info   = ELF32_R_INFO (0, R_386_RELATIVE);
            rel.r_offset = (Elf32_Addr) Fix[i];

            reldynSection -> PutS (&rel, sizeof(Elf32_Rel));
        }
    }

    ELFStringTable * dynstr = new ELFStringTable ();
    ELFSymbolTable * dynsym = new ELFSymbolTable (dynstr, true);

    dynamicSection = newStorage (1024);

    // write DT_SONAME
    {
        // strip path to make dllname
        const char * p = strrchr (outputname, OS->FileSep());
        const char * soname = (p == NULL) ? outputname : p + 1;

        Elf32_Dyn dyn;
        dyn.d_tag      = DT_SONAME;
        dyn.d_un.d_val = dynstr->ID (soname);
        dynamicSection -> PutS (&dyn, sizeof (Elf32_Dyn));
    }

    // record DT_NEEDED entries in .dynamic
    for (i = 0; i < Ndlls; i++) {
        Elf32_Dyn dyn;
        dyn.d_tag      = DT_NEEDED;
        dyn.d_un.d_val = dynstr->ID (NAMES.Index2Str (dlls[i].name));
        dynamicSection -> PutS (&dyn, sizeof (Elf32_Dyn));

        dlls[i].nameID = dyn.d_un.d_val;
    }

    // write reserved entries in .got
    gotSection -> Put4 (pltVA);
    gotSection -> Put4 (0);
    gotSection -> Put4 (0);

    // write 0 entry in .plt
    pltSection -> Put2 (0x35FF);        // push dword ptr [.got + 4]
    pltSection -> Put4 (gotVA + 4);
    pltSection -> Put2 (0x25FF);        // jmp  dword ptr [.got + 8]
    pltSection -> Put4 (gotVA + 8);
    pltSection -> Put4 (0x90909090);    // nop; nop; nop; nop

    if (xDLLFlag) {
        // place R_386_RELATIVE relocations to "dword ptr [***]"
        Elf32_Rel rel;

        rel.r_info   = ELF32_R_INFO (0, R_386_RELATIVE);
        rel.r_offset = (Elf32_Addr) (pltVA + 2);
        reldynSection -> PutS (&rel, sizeof(Elf32_Rel));

        rel.r_info   = ELF32_R_INFO (0, R_386_RELATIVE);
        rel.r_offset = (Elf32_Addr) (pltVA + 8);
        reldynSection -> PutS (&rel, sizeof(Elf32_Rel));
    }

    // define symbols (in .dynsym), fill .rel.plt, .rel.dyn, .plt and .got

    dword nFuncEntry = 0;
    dword nEntry     = 0;

#ifdef LinuxExtensions
    gnuverSection = newStorage ((NumberOfImportedNames+NumberOfExports+16)*sizeof(Elf32_Half));
    gnuverSection -> Put2 (0);  // version definition for 0 symbol table entry
#endif

    for (i = 0; i < NumberOfImportedNames; i++) {
        importNameInfo * s = (importNameInfo *) (NAMES.getInfo (ImportedNames[i]));
        if (s && ((s -> kind & (K_USED | K_MASK)) == K_USED + K_IMPORT))
        {

#ifdef LinuxExtensions
            gnuverSection -> Put2 (symVersion (s));
#endif
            // VA of corresponding .got entry
            dword gotEntryVA = gotVA + (GOT_N_RESERVED + (xDLLFlag ? nEntry : nFuncEntry)) * sizeof(Elf32_Addr);

            if (s -> kind & K_IMPFUNC) {
                // VA of corresponding .plt entry
                dword pltEntryVA = pltVA + PLT_ENTRY0_SZ + nFuncEntry*PLT_ENTRYi_SZ;

                // define symbol (section undefined, va -> .plt)
                dword symIdx = dynsym -> Sym (NAMES.Index2Str(ImportedNames[i]),
                                              pltEntryVA,
                                              0,  // ?? - symbol size
                                              STB_GLOBAL,
                                              STT_FUNC,
                                              SHN_UNDEF);

                // place R_386_JMP_SLOT relocation to .got entry
                Elf32_Rel rel;
                rel.r_info   = ELF32_R_INFO (symIdx, R_386_JMP_SLOT);
                rel.r_offset = (Elf32_Addr) gotEntryVA;

                relpltSection -> PutS (&rel, sizeof(Elf32_Rel));

                // write .plt entry
                pltSection -> Put2 (0x25FF);        // jmp dword ptr [.got entry]
                pltSection -> Put4 (gotEntryVA);
                pltSection -> PutB (0x68);          // push 8*nFuncEntry
                pltSection -> Put4 (nFuncEntry*8);
                pltSection -> PutB (0xE9);          // jmp (rel32) 0_.plt_entry
                pltSection -> Put4 (0xFFFFFFFF - ((nFuncEntry+1)*PLT_ENTRYi_SZ + PLT_ENTRY0_SZ) + 1);

                if (xDLLFlag) {
                    // place R_386_RELATIVE relocation to "dword ptr [.got entry]"
                    Elf32_Rel rel;
                    rel.r_info   = ELF32_R_INFO (0, R_386_RELATIVE);
                    rel.r_offset = (Elf32_Addr) (pltEntryVA + 2);
                    reldynSection -> PutS (&rel, sizeof(Elf32_Rel));
                }

                // write correct value into .got entry
                gotSection -> Put4 (pltEntryVA + 6);

                // write rel. jmp to import jumps section (pointing to .plt)
                dword jump_offset = s -> offset - CodeStart;
                *(( byte *) (CodePtr + jump_offset    )) = 0xE9;
                *((dword *) (CodePtr + jump_offset + 1)) = pltEntryVA - s->offset - 5;

                nFuncEntry++;
            } else {
                if (xDLLFlag) {
                    // define symbol (section undefined, va -> .got)
                    dword symIdx = dynsym -> Sym (NAMES.Index2Str(ImportedNames[i]),
                                                  gotEntryVA,
                                                  0,  // ?? - symbol size
                                                  STB_GLOBAL,
                                                  STT_OBJECT,
                                                  SHN_UNDEF);

                    // place R_386_GLOB_DAT relocation to .got entry
                    Elf32_Rel rel;
                    rel.r_info   = ELF32_R_INFO (symIdx, R_386_GLOB_DAT);
                    rel.r_offset = (Elf32_Addr) gotEntryVA;

                    reldynSection -> PutS (&rel, sizeof(Elf32_Rel));

                    // put a value into .got entry
                    gotSection -> Put4 (0);

                    // set redirection to .got
                    s -> patchFixups (gotEntryVA);
                } else {
                    // define symbol (section BSS)
                    ASSERT (s -> seg != NULL);
                    ASSERT (s -> seg -> address > 0);

                    dword va = s -> seg -> address;
                    Elf32_Half bssSecIdx = (Elf32_Half) (1 + xUseProgramInterpreter + (CodeLen > 0) + (DataLen > 0));

                    dword symIdx = dynsym -> Sym (NAMES.Index2Str(ImportedNames[i]),
                                                  va,
                                                  s -> seg -> getLen(),
                                                  STB_GLOBAL,
                                                  STT_OBJECT,
                                                  bssSecIdx);

                    // place R_386_COPY relocation
                    Elf32_Rel rel;
                    rel.r_info   = ELF32_R_INFO (symIdx, R_386_COPY);
                    rel.r_offset = (Elf32_Addr) va;

                    reldynSection -> PutS (&rel, sizeof(Elf32_Rel));
                }
            }
            nEntry++;
        }
    }
    ASSERT (relpltSize == relpltSection->GetLen());
    ASSERT (reldynSize == reldynSection->GetLen());


    // Write export to dynamic symbol table
    for (int n = 0; n < NumberOfExports; n ++) {
        Export * s = ExportsTable[n];
        dword va = 0;

        if (s -> seg) {
            ASSERT ((s -> seg -> isProcessed ()) && (s -> seg -> address > 0));
            ASSERT ((int)(s -> offset) <= (s -> seg -> getLen()));
            va = s -> seg -> address + s -> offset;
        } else {
            nameInfo * info = (nameInfo *) (NAMES.getInfo (s -> intname));
            ASSERT (info != NULL);
            ASSERT (info->kind & K_USED);
            ASSERT ((dword)(info->offset) >= xImageBase);
            va = info -> offset;
        }

        dword secIdx = 0;
        if ((CodeStart <= va) && (va <= CodeStart + CodeLen))
            secIdx = 1 + xUseProgramInterpreter + 0;
        else if ((DataStart <= va) && (va <= DataStart + DataLen))
            secIdx = 1 + xUseProgramInterpreter + 1;
        else if ((BSSStart <= va) && (va <= BSSStart + BSSLen))
            secIdx = 1 + xUseProgramInterpreter + 2; 
        else if ((RDataStart <= va) && (va <= RDataStart + RDataLen))
            secIdx = 1 + xUseProgramInterpreter + 3;
        else {
            VerboseMessage ("Warning: name %s (va %XH) not exported\n", NAMES.Index2Str(s->extname), va);
        }

        if (secIdx > 0) {
            dword symIdx = dynsym -> Sym (NAMES.Index2Str(s->extname),
                                          va,
                                          0,  // ?? - symbol size
                                          STB_GLOBAL,
                                          ((s->flag & EFLAG_VARIABLE) ? STT_OBJECT : STT_FUNC),
                                          (Elf32_Half) secIdx);
#ifdef LinuxExtensions
            // .gnu.version is parallel to .dynsym, add entry
            gnuverSection -> Put2 (VER_NDX_GLOBAL);
#endif
        }
    }
    xfree (ExportsTable);

    // add dynamic symbols corresponding to sections

    int nUserSections =   (CodeLen  > 0)     // .text
                        + (DataLen  > 0)     // .data
                        + (BSSLen   > 0)     // .bss
                        + (RDataLen > 0)     // .rodata
                        + (JImportLen > 0)   // jimport
                        + (JExportLen > 0)   // jexport
                        + (CPB != NULL);     // config

    Elf32_Half secIdx = (Elf32_Half) (1 + xUseProgramInterpreter);

    // .text
    if (CodeLen > 0) {
        dynsym -> Sym (".text",
                       CodeStart,
                       CodeLen,
                       STB_LOCAL,
                       STT_SECTION,
                       (Elf32_Half)(secIdx++));
#ifdef LinuxExtensions
        gnuverSection -> Put2 (VER_NDX_LOCAL);
#endif
    }

    // .data
    if (DataLen > 0) {
        dynsym -> Sym (".data",
                       DataStart,
                       DataLen,
                       STB_LOCAL,
                       STT_SECTION,
                       (Elf32_Half)(secIdx++));
#ifdef LinuxExtensions
        gnuverSection -> Put2 (VER_NDX_LOCAL);
#endif
    }

    secIdx += (BSSLen > 0) + (RDataLen > 0);

    // jimport
    if (JImportLen > 0) {
        dynsym -> Sym (".jidata",
                       JImportStart,
                       JImportLen,
                       STB_LOCAL,
                       STT_SECTION,
                       (Elf32_Half)(secIdx++));
#ifdef LinuxExtensions
        gnuverSection -> Put2 (VER_NDX_LOCAL);
#endif
    }

    // jexport
    if (JExportLen > 0) {
        dynsym -> Sym (".jedata",
                       JExportStart,
                       JExportLen,
                       STB_LOCAL,
                       STT_SECTION,
                       (Elf32_Half)(secIdx++));
#ifdef LinuxExtensions
        gnuverSection -> Put2 (VER_NDX_LOCAL);
#endif
    }

/*
    // config
    if (CPB != NULL) {
        dynsym -> Sym (".config",
                       CPBStart,
                       CPB->Index,
                       STB_LOCAL,
                       STT_SECTION,
                       (Elf32_Half)(secIdx++));
#ifdef LinuxExtensions
        gnuverSection -> Put2 (VER_NDX_LOCAL);
#endif
    }
*/


#ifdef LinuxExtensions
    if (NVerNeed != 0) {
        gnuverSize = gnuverSection->GetLen();

        gnuverVA = VASA -> getVirtualAddr();
        VASA -> allocateVirtualSpace (gnuverVA, gnuverSize);

        gnuver_rVA      = VASA -> getVirtualAddr();
        gnuver_rSize    = NVerNeed * sizeof (Elf32_Verneed) + NVerAux * sizeof (Elf32_Vernaux);
        gnuver_rSection = newStorage (gnuver_rSize);
        VASA -> allocateVirtualSpace (gnuver_rVA, gnuver_rSize);

        for (unsigned v = 0; v < NVerNeed; v++) {
            struct VerNeedEntry * vn = &(VerNeed[v]);
            dword cnt = vn -> cnt;

            Elf32_Verneed verneed;
            verneed.vn_version = VER_NEED_CURRENT;
            verneed.vn_cnt     = (Elf32_Half) cnt;
            verneed.vn_file    = vn -> dllnameID;
            verneed.vn_aux     = sizeof (Elf32_Verneed);
            verneed.vn_next    = (v == NVerNeed-1) ? 0 : sizeof (Elf32_Verneed) + cnt*sizeof (Elf32_Vernaux);

            gnuver_rSection -> PutS (&verneed, sizeof (Elf32_Verneed));

            for (dword j = 0; j < cnt; j++) {
                struct VerNeedAuxEntry * ae = &(vn -> aux[j]);

                Elf32_Vernaux vernaux;
                vernaux.vna_hash  = elf_hash ((unsigned char *) (NAMES.Index2Str (ae -> id)));
                vernaux.vna_flags = 0;
                vernaux.vna_other = ae -> vna_other;
                vernaux.vna_name  = dynstr -> ID (NAMES.Index2Str (ae -> id));
                vernaux.vna_next  = (j == cnt-1) ? 0 : sizeof (Elf32_Vernaux);

                gnuver_rSection -> PutS (&vernaux, sizeof (Elf32_Vernaux));
            }
        }

        ASSERT (gnuver_rSize == gnuver_rSection->GetLen());
    } else {
        // free useless .gnu.version section
        delete gnuverSection;
        gnuverSection = NULL;
    }
#endif

    // allocate .hash, .dynsym and .dynstr sections

    hashSection   = dynsym -> getHashTable ();
    dynsymSection = dynsym -> getSymTable ();
    dynstrSection = dynstr -> getContents ();

    hashVA = VASA -> getVirtualAddr();
    VASA -> allocateVirtualSpace (hashVA, hashSection->GetLen());

    dynsymVA = VASA -> getVirtualAddr();
    VASA -> allocateVirtualSpace (dynsymVA, dynsymSection->GetLen());

    dynstrVA = VASA -> getVirtualAddr();
    VASA -> allocateVirtualSpace (dynstrVA, dynstrSection->GetLen());

    // fill in .dynamic section entries

    Elf32_Dyn dyn;
    dyn.d_tag      = DT_HASH;
    dyn.d_un.d_ptr = (Elf32_Addr) hashVA;
    dynamicSection -> PutS (&dyn, sizeof (Elf32_Dyn));

    dyn.d_tag      = DT_STRTAB;
    dyn.d_un.d_ptr = (Elf32_Addr) dynstrVA;
    dynamicSection -> PutS (&dyn, sizeof (Elf32_Dyn));

    dyn.d_tag      = DT_SYMTAB;
    dyn.d_un.d_ptr = (Elf32_Addr) dynsymVA;
    dynamicSection -> PutS (&dyn, sizeof (Elf32_Dyn));

    dyn.d_tag      = DT_STRSZ;
    dyn.d_un.d_val = dynstrSection -> GetLen ();
    dynamicSection -> PutS (&dyn, sizeof (Elf32_Dyn));

    dyn.d_tag      = DT_SYMENT;
    dyn.d_un.d_val = sizeof (Elf32_Sym);
    dynamicSection -> PutS (&dyn, sizeof (Elf32_Dyn));

    dyn.d_tag      = DT_DEBUG;
    dyn.d_un.d_val = 0;
    dynamicSection -> PutS (&dyn, sizeof (Elf32_Dyn));

    dyn.d_tag      = DT_PLTGOT;
    dyn.d_un.d_ptr = (Elf32_Addr) gotVA;
    dynamicSection -> PutS (&dyn, sizeof (Elf32_Dyn));

    dyn.d_tag      = DT_PLTRELSZ;
    dyn.d_un.d_val = relpltSize;
    dynamicSection -> PutS (&dyn, sizeof (Elf32_Dyn));

    dyn.d_tag      = DT_PLTREL;
    dyn.d_un.d_val = DT_REL;
    dynamicSection -> PutS (&dyn, sizeof (Elf32_Dyn));

    dyn.d_tag      = DT_JMPREL;
    dyn.d_un.d_val = relpltVA;
    dynamicSection -> PutS (&dyn, sizeof (Elf32_Dyn));

    if (xDLLFlag && (NFixups > 0)) {
        // some relocations might modify .text
        dyn.d_tag      = DT_TEXTREL;
        dyn.d_un.d_val = 0;
        dynamicSection -> PutS (&dyn, sizeof (Elf32_Dyn));
    }

    dyn.d_tag      = DT_REL;
    dyn.d_un.d_val = reldynVA;
    dynamicSection -> PutS (&dyn, sizeof (Elf32_Dyn));

    dyn.d_tag      = DT_RELSZ;
    dyn.d_un.d_val = reldynSize;
    dynamicSection -> PutS (&dyn, sizeof (Elf32_Dyn));

    dyn.d_tag      = DT_RELENT;
    dyn.d_un.d_val = sizeof (Elf32_Rel);
    dynamicSection -> PutS (&dyn, sizeof (Elf32_Dyn));

#ifdef LinuxExtensions
/*
    if (xDLLFlag && (NFixups > 0)) {
        dyn.d_tag      = DT_RELCOUNT;
        dyn.d_un.d_val = NFixups;
        dynamicSection -> PutS (&dyn, sizeof (Elf32_Dyn));
    }
*/

    if (gnuver_rSection != NULL) {
        dyn.d_tag      = DT_VERNEED;
        dyn.d_un.d_val = gnuver_rVA;
        dynamicSection -> PutS (&dyn, sizeof (Elf32_Dyn));

        dyn.d_tag      = DT_VERNEEDNUM;
        dyn.d_un.d_val = NVerNeed;
        dynamicSection -> PutS (&dyn, sizeof (Elf32_Dyn));

        dyn.d_tag      = DT_VERSYM;
        dyn.d_un.d_val = gnuverVA;
        dynamicSection -> PutS (&dyn, sizeof (Elf32_Dyn));
    }
#endif

    if (xDLLFlag) {
        if (_init_VA != 0) {
            // declare initialization routine
            dyn.d_tag      = DT_INIT;
            dyn.d_un.d_val = _init_VA;
            dynamicSection -> PutS (&dyn, sizeof (Elf32_Dyn));
        }
        if (_fini_VA != 0) {
            // declare finalization routine
            dyn.d_tag      = DT_FINI;
            dyn.d_un.d_val = _fini_VA;
            dynamicSection -> PutS (&dyn, sizeof (Elf32_Dyn));
        }
    }

    dyn.d_tag      = DT_NULL;
    dyn.d_un.d_val = 0;
    dynamicSection -> PutS (&dyn, sizeof (Elf32_Dyn));

    dynamicVA = VASA -> getVirtualAddr();
    VASA -> allocateVirtualSpace (dynamicVA, dynamicSection->GetLen());
}


/*----------------------------------------------------------------------------*/
/*                                                                            */
/*                    Main procedure - form and write ELF                     */
/*                                                                            */
/*----------------------------------------------------------------------------*/

static FILE * OutFile = NULL;

static dword nbytes = 0;
static char* output_file_name = NULL;

static void WriteOutFile (void * buf, dword size, dword fileOffset)
{
    VerboseMessage ("ELF: Write at %d, nbyte=%d, sz=%d\n", fileOffset, nbytes, size);
    ASSERT (fileOffset == nbytes);
    ASSERT (size != 0);
    nbytes += size;
    if (fwrite (buf, size, 1, OutFile) != 1) {
        fclose (OutFile);
        Message(xFATAL, msgUNABLE_TO_WRITE_FILE, output_file_name);
    }
}

static byte * filler     = NULL;
static dword  fillerSize = 0;


static void WriteZeroes (dword size) {
    if (size > fillerSize) {
        xfree (filler);
        filler     = (byte *) xalloc (size);
        fillerSize = size;
        memset (filler, 0, fillerSize);
    }
    WriteOutFile (filler, size, nbytes);
}


static void AlignOutFile (dword alignment) {
    dword sz = ((nbytes + alignment-1) & ~(alignment-1)) - nbytes;

    if (sz > 0) {
        WriteZeroes (sz);
    }

    ASSERT ((nbytes % alignment) == 0);
}

/*----------------------------------------------------------------------------*/

#define LOAD_SEGMENT_ALIGNMENT  4096

#define MAX_NUMBER_OF_SEGMENTS  32
#define MAX_NUMBER_OF_SECTIONS  32

static Elf32_Ehdr  ELFHeader;
static Elf32_Phdr  ProgramHeader [MAX_NUMBER_OF_SEGMENTS];
static Elf32_Shdr  SectionHeader [MAX_NUMBER_OF_SECTIONS];

static int numberOfSegments = 0;
static int numberOfSections = 0;

static dword FilePointer;


class ELFSection;
static ELFSection * Sections [MAX_NUMBER_OF_SECTIONS];


class ELFSegment {
  private:
    int index;

  public:
    ELFSegment (Elf32_Word type,
                Elf32_Off  offset,
                dword      vaddr,
                Elf32_Word filesz,
                Elf32_Word memsz,
                Elf32_Word flags,
                Elf32_Word align)
    {
        index = numberOfSegments++;
        ASSERT (numberOfSegments <= MAX_NUMBER_OF_SEGMENTS);

        ProgramHeader[index].p_type   = type;
        ProgramHeader[index].p_offset = offset;
        ProgramHeader[index].p_vaddr  = (Elf32_Addr) vaddr;
        ProgramHeader[index].p_paddr  = (Elf32_Addr) vaddr;
        ProgramHeader[index].p_filesz = filesz;
        ProgramHeader[index].p_memsz  = memsz;
        ProgramHeader[index].p_flags  = flags;
        ProgramHeader[index].p_align  = align;
    }
};


class ELFSection {
  private:
    int       index;
    Storage * contents;
    dword     fileOffset;

  public:

    ELFSection (Storage    * _contents,
                dword        v_addr,
                dword        v_len,
                Elf32_Word   name,
                Elf32_Word   type,
                Elf32_Word   flags,
                Bool         createSeg,
                Elf32_Word   align,
                Elf32_Word   sh_link = SHN_UNDEF,
                Elf32_Word   sh_info = 0,
                Elf32_Word   sh_entsize = 0)
    {
        index = numberOfSections++;
        ASSERT (numberOfSections <= MAX_NUMBER_OF_SECTIONS);

        Sections [index] = this;

        contents = _contents;

        fileOffset = FilePointer;

        ASSERT ((v_addr % align) == 0);

        if (createSeg) {
            ASSERT (flags & SHF_ALLOC);

            new ELFSegment (PT_LOAD,
                            fileOffset,
                            v_addr,
                            (contents != NULL) ? (contents->GetLen() + xObjectOffset-1) & ~(xObjectOffset-1) : xObjectOffset,
                            (v_len + xObjectOffset-1) & ~(xObjectOffset-1),
                            PF_R +
                            ((flags & SHF_WRITE)     ? PF_W : 0) +
                            ((flags & SHF_EXECINSTR) ? PF_X : 0),
                            LOAD_SEGMENT_ALIGNMENT);
        }

        SectionHeader [index].sh_name      = name;
        SectionHeader [index].sh_type      = type;
        SectionHeader [index].sh_flags     = flags;
        SectionHeader [index].sh_addr      = (Elf32_Addr) v_addr;
        SectionHeader [index].sh_offset    = fileOffset;
        SectionHeader [index].sh_size      = (contents != NULL) ? contents->GetLen() : 0;
        SectionHeader [index].sh_link      = sh_link;
        SectionHeader [index].sh_info      = sh_info;
        SectionHeader [index].sh_addralign = align;
        SectionHeader [index].sh_entsize   = sh_entsize;

        if (contents != NULL) {
            FilePointer = (FilePointer + contents->GetLen() + xObjectOffset-1) & ~(xObjectOffset-1);
        } else {
            FilePointer += xObjectOffset;
        }
    }

    void WriteSection (void)
    {
        if (contents) {
            if (contents->GetLen() != 0) {
                WriteOutFile (contents->GetData(), contents->GetLen(), fileOffset);
            }
            AlignOutFile (xObjectOffset);

            delete contents;
            contents = NULL;
        } else {
            WriteZeroes (xObjectOffset);
        }
    }

    Elf32_Half getIndex () const {
        return (Elf32_Half) index;
    }
};


static void WriteSections ()
{
    for (int i = 1; i < numberOfSections; i++) {
        VerboseMessage ("ELF: Write SEC %d\n", i);
        Sections[i]->WriteSection();
    }
}

/*----------------------------------------------------------------------------*/

static Elf32_Half va2segIdx (dword va)
{
    // here we ignore PHDR segment

    for (int i = 1; i < numberOfSegments; i++) {
        dword vaddr = (dword) (ProgramHeader[i].p_vaddr);

        if ((vaddr <= va) &&
            (va < vaddr + ProgramHeader[i].p_memsz))
        {
            // program header found
            return (Elf32_Half) i;
        }
    }

    return 0; // not found
}


static Elf32_Half va2secIdx (dword va)
{
    for (int i = 0; i < numberOfSegments; i++) {
        dword vaddr = (dword) (ProgramHeader[i].p_vaddr);

        if ((vaddr <= va) &&
            (va < vaddr + ProgramHeader[i].p_memsz))
        {
            // program header found, find the section

            // NOTE: here we assume that for each user section there is
            //       only ONE segment

            for (int j = 0; j < numberOfSections; j++) {
                if ((dword) (SectionHeader[j].sh_addr) == vaddr) {
                    return (Elf32_Half) j;
                }
            }
        }
    }
    return SHN_UNDEF;
}

static ELFSymbolTable * symtab;

static void add2symtab (ident id, void *nfo)
{
    if (nfo != NULL) {
        nameInfo * info = (nameInfo *) nfo;
        if (((info -> kind & K_MASK) == K_PUBDEF) && (info -> offset > 0)) {
            dword va = (dword)(info -> offset);

            Elf32_Half secIdx = va2secIdx (va);
            if (secIdx != SHN_UNDEF) {

                // try to guess symbol type
                byte type;
                if (kind2type(info->kind) == T_UNKNOWN) {
                    if ((CodeStart <= va) && (va < CodeStart + CodeLen))
                        type = STT_FUNC;
                    else
                        type = STT_OBJECT;
                } else {
                    type = (kind2type(info->kind) == T_CODE) ? STT_FUNC : STT_OBJECT;
                }

                // define symbol in symbol table
                symtab -> Sym (NAMES.Index2Str (id),
                               va,
                               0,
                               STB_GLOBAL,
                               type,
                               secIdx);
            }
        }
    }
}

/*----------------------------------------------------------------------------*/

// object number for debug info = index of segment + 1

dword ELF_getOffset (dword vadr, word object) {
    ASSERT ((object > 0) && (object <= numberOfSegments+1));
    ASSERT (vadr >= ((dword) (ProgramHeader[object-1].p_vaddr)));
    ASSERT (vadr <= ((dword) (ProgramHeader[object-1].p_vaddr) + ProgramHeader[object-1].p_memsz));
    return vadr - (dword) (ProgramHeader[object-1].p_vaddr);
}

word ELF_getObjectNumber (dword vadr) {
    Elf32_Half segIdx = va2segIdx (vadr);
    ASSERT (segIdx != 0);
    ASSERT (segIdx < 65534);
    return (word) (segIdx + 1);
}

/*----------------------------------------------------------------------------*/

void WriteELF (char * name)
{
    if (numberOfResourceDirectories != 0) {
        Message (xFATAL, msgELF_NOT_SUPPORT_RESOURCES);
    }

    VASA->discardVirtualSpace (IdataStart);

    // Form J Export Image
    if (xJetComponent || (nJExportGroups > 0)) {
        JExportStart = VASA->getVirtualAddr();
        FormJExportImage ();
        JExportLen = JExportImage -> Index;
        VASA->allocateVirtualSpace (JExportStart, JExportLen);
    }

    // Determine Entry Point Address
    if (xWasEntryPoint) {
        ASSERT (EntryPoint != NULL);

        ident i;
        switch (EntryPoint->k_target) {
            case TK_SEG:
                entryPointVA = ((Segment *) EntryPoint->target) -> address;
                break;
            case TK_ID:
                i = (ident) (EntryPoint->target);
                entryPointVA = ((nameInfo *) (NAMES.getInfo (i))) -> offset;
                break;
            default:
                Message (xFATAL, msgINVALID_ENTRY_POINT_TARGET);
        }
        entryPointVA += EntryPoint->fx_offset;
    }

    if (xDLLFlag && !xNoEntryPoint) {
        // look for _init, _fini
        nameInfo * _init_nfo = (nameInfo *) (NAMES.getInfo (NAMES.Str2Index ("_init")));
        if (_init_nfo != NULL) {
            ASSERT (_init_nfo -> offset > 0);
            _init_VA = _init_nfo -> offset;
        }
        nameInfo * _fini_nfo = (nameInfo *) (NAMES.getInfo (NAMES.Str2Index ("_fini")));
        if (_fini_nfo != NULL) {
            ASSERT (_fini_nfo -> offset > 0);
            _fini_VA = _fini_nfo -> offset;
        }
    }

    Bool doDynamic = ((Ndlls > 0) || xDLLFlag || xJetComponent);
    xUseProgramInterpreter = xUseProgramInterpreter && (xProgramInterpreter != NULL);

    if (doDynamic) {
        CreateDynamicInfo (name);
    }

    ELFStringTable * shstrtab = new ELFStringTable ();

    ELFStringTable * strtab = new ELFStringTable ();
    symtab = new ELFSymbolTable (strtab);

    int nUserSections =   (CodeLen  > 0)     // .text
                        + (DataLen  > 0)     // .data
                        + (BSSLen   > 0)     // .bss
                        + (RDataLen > 0)     // .rodata
                        + (JImportLen > 0)   // jimport
                        + (JExportLen > 0)   // jexport
                        + (CPB != NULL);     // config

    int nDynamicSections = 0;

    if (doDynamic) {
        nDynamicSections = 7
                           + (reldynSection != NULL)   // .rel.dyn
#ifdef LinuxExtensions
                           + (gnuverSection   != NULL) // .gnu.version
                           + (gnuver_rSection != NULL) // .gnu.version_r
#endif
                           ;
    }

    int nSections =   1                         // 0
                    + xUseProgramInterpreter    // .interp
                    + nUserSections             // user sections
                    + nDynamicSections          // dynamic sections
                    + 1                         // .strtab
                    + 1                         // .symtab
                    + 1;                        // .shstrtab

    int nSegments =   1                         // PHDR
                    + xUseProgramInterpreter    // INTERP
                    + 1                         // LOAD for header
                    + nUserSections             // LOAD for user sections
                    + nDynamicSections          // LOAD for dynamic sections
                    + 1                         // LOAD for .strtab
                    + 1                         // LOAD for .symtab
                    + doDynamic;                // DYNAMIC

    dword intrepSize = xUseProgramInterpreter ? strlen (xProgramInterpreter) + 1 : 0;

    dword headerSize = sizeof (Elf32_Ehdr) +
                       nSegments*sizeof (Elf32_Phdr) +
                       intrepSize;

    // PHDR Segment
    new ELFSegment (PT_PHDR,
                    sizeof (Elf32_Ehdr),
                    xImageBase + sizeof (Elf32_Ehdr),
                    nSegments*sizeof (Elf32_Phdr),
                    nSegments*sizeof (Elf32_Phdr),
                    PF_R,
                    4);

    if (xUseProgramInterpreter) {
        // INTERP Segment
        new ELFSegment (PT_INTERP,
                        sizeof (Elf32_Ehdr) + nSegments * sizeof(Elf32_Phdr),
                        xImageBase + sizeof (Elf32_Ehdr) + nSegments * sizeof(Elf32_Phdr),
                        intrepSize,
                        intrepSize,
                        PF_R,
                        1);
    }

    // LOAD Segment for header
    new ELFSegment (PT_LOAD,
                    0,
                    xImageBase,
                    (headerSize + xObjectOffset-1) & ~(xObjectOffset-1),
                    (headerSize + xObjectOffset-1) & ~(xObjectOffset-1),
                    PF_R,
                    LOAD_SEGMENT_ALIGNMENT);

    FilePointer = sizeof (Elf32_Ehdr) +
                  nSegments*sizeof (Elf32_Phdr);

    // 0 Section
    SectionHeader [0].sh_name      = 0;
    SectionHeader [0].sh_type      = SHT_NULL;
    SectionHeader [0].sh_flags     = 0;
    SectionHeader [0].sh_addr      = 0;
    SectionHeader [0].sh_offset    = 0;
    SectionHeader [0].sh_size      = 0;
    SectionHeader [0].sh_link      = SHN_UNDEF;
    SectionHeader [0].sh_info      = 0;
    SectionHeader [0].sh_addralign = 0;
    SectionHeader [0].sh_entsize   = 0;
    numberOfSections++;

    if (xUseProgramInterpreter) {
        const char * ProgramInterpreter = xProgramInterpreter;
        VerboseMessage ("ELF: Use program interpreter %s\n", ProgramInterpreter);

        Storage * interpSection = newStorage (strlen (ProgramInterpreter) + 1);
        interpSection -> PutS (ProgramInterpreter, strlen (ProgramInterpreter) + 1);

        // Program Interpreter Section
        new ELFSection (interpSection,
                        xImageBase + sizeof (Elf32_Ehdr) + nSegments * sizeof(Elf32_Phdr),
                        interpSection -> GetLen(),
                        shstrtab->ID (".interp"),
                        SHT_PROGBITS,
                        SHF_ALLOC,
                        false, 1);
    }

    FilePointer = (FilePointer + xObjectOffset-1) & ~(xObjectOffset-1);

    // Code Section
    if (CodeLen > 0) {
        new ELFSection (newStorage (CodePtr, CodeLen),
                        CodeStart, CodeLen,
                        shstrtab->ID (".text"),
                        SHT_PROGBITS,
                        SHF_ALLOC + SHF_EXECINSTR,
                        true, xObjectOffset);
    }
 
    // Data Section
    if (DataLen > 0) {
        new ELFSection (newStorage (DataPtr, DataLen),
                        DataStart, DataLen,
                        shstrtab->ID (".data"),
                        SHT_PROGBITS,
                        SHF_WRITE + SHF_ALLOC,
                        true, xObjectOffset);
    }

    // BSS Section
    if (BSSLen > 0) {
        new ELFSection (NULL,
                        BSSStart, BSSLen,
                        shstrtab->ID(".bss"),
                        SHT_NOBITS,
                        SHF_WRITE + SHF_ALLOC,
                        true, xObjectOffset);
    }

    // Read-only data Section
    if (RDataLen > 0) {
        new ELFSection (newStorage (RDataPtr, RDataLen),
                        RDataStart, RDataLen,
                        shstrtab->ID (".rodata"),
                        SHT_PROGBITS,
                        SHF_ALLOC,
                        true, xObjectOffset);
    }

    // J Import Section
    if (JImportLen > 0) {
        new ELFSection (JImportImage,
                        JImportStart, JImportLen,
                        shstrtab->ID ("jimport"),
                        SHT_PROGBITS,
                        SHF_WRITE + SHF_ALLOC,
                        true, xObjectOffset);
    }

    // J Export Section
    if (JExportLen > 0) {
        new ELFSection (JExportImage,
                        JExportStart, JExportLen,
                        shstrtab->ID ("jexport"),
                        SHT_PROGBITS,
                        SHF_ALLOC,
                        true, xObjectOffset);
    }

    dword dynamicSecOffset = 0;

    if (doDynamic) {
        // Dynamic Sections

        Elf32_Half gotSecIdx = 
         (new ELFSection (gotSection,
                          gotVA, gotSize,
                          shstrtab->ID (".got"),
                          SHT_PROGBITS,
                          SHF_ALLOC + SHF_WRITE,
                          true, xObjectOffset)) -> getIndex ();

        symtab -> Sym ("_GLOBAL_OFFSET_TABLE_",
                       gotVA,
                       0,
                       STB_GLOBAL,
                       STT_OBJECT,
                       gotSecIdx);

        Elf32_Half pltSecIdx = 
         (new ELFSection (pltSection,
                          pltVA, pltSize,
                          shstrtab->ID (".plt"),
                          SHT_PROGBITS,
                          SHF_ALLOC + SHF_EXECINSTR,
                          true, xObjectOffset)) -> getIndex ();

        Elf32_Half dynsymSecIdx0 = (Elf32_Half) (pltSecIdx + 3 + (reldynSection != NULL) + (gnuverSection != NULL) + (gnuver_rSection != NULL));
        Elf32_Half dynstrSecIdx0 = (Elf32_Half) (pltSecIdx + 4 + (reldynSection != NULL) + (gnuverSection != NULL) + (gnuver_rSection != NULL));

        new ELFSection (relpltSection,
                        relpltVA, relpltSize,
                        shstrtab->ID (".rel.plt"),
                        SHT_REL,
                        SHF_ALLOC,
                        true, xObjectOffset,
                        dynsymSecIdx0, pltSecIdx,
                        sizeof (Elf32_Rel));

        if (reldynSection != NULL) {
            new ELFSection (reldynSection,
                            reldynVA, reldynSize,
                            shstrtab->ID (".rel.dyn"),
                            SHT_REL,
                            SHF_ALLOC,
                            true, xObjectOffset,
                            dynsymSecIdx0, gotSecIdx,
                            sizeof (Elf32_Rel));
        }

#ifdef LinuxExtensions
        if (gnuverSection != NULL) {
            new ELFSection (gnuverSection,
                            gnuverVA, gnuverSize,
                            shstrtab->ID (".gnu.version"),
                            SHT_GNU_versym,
                            SHF_ALLOC,
                            true, xObjectOffset,
                            0, 0,
                            sizeof (Elf32_Half));
        }

        if (gnuver_rSection != NULL) {
            new ELFSection (gnuver_rSection,
                            gnuver_rVA, gnuver_rSize,
                            shstrtab->ID (".gnu.version_r"),
                            SHT_GNU_verneed,
                            SHF_ALLOC,
                            true, xObjectOffset,
                            dynstrSecIdx0, NVerNeed);
        }
#endif

        new ELFSection (hashSection,
                        hashVA, hashSection->GetLen(),
                        shstrtab->ID (".hash"),
                        SHT_HASH,
                        SHF_ALLOC,
                        true, xObjectOffset,
                        dynsymSecIdx0);

        Elf32_Half dynsymSecIdx =
         (new ELFSection (dynsymSection,
                          dynsymVA, dynsymSection->GetLen(),
                          shstrtab->ID (".dynsym"),
                          SHT_DYNSYM,
                          SHF_ALLOC,
                          true, xObjectOffset,
                          dynstrSecIdx0, 1,
                          sizeof (Elf32_Sym))) -> getIndex();

        ASSERT (dynsymSecIdx == dynsymSecIdx0);

        Elf32_Half dynstrSecIdx =
         (new ELFSection (dynstrSection,
                          dynstrVA, dynstrSection->GetLen(),
                          shstrtab->ID (".dynstr"),
                          SHT_STRTAB,
                          SHF_ALLOC,
                          true, xObjectOffset))->getIndex();

        ASSERT (dynstrSecIdx == dynstrSecIdx0);

        dynamicSecOffset = FilePointer;

        Elf32_Half dynamicSecIdx = 
         (new ELFSection (dynamicSection,
                          dynamicVA, dynamicSection->GetLen(),
                          shstrtab->ID (".dynamic"),
                          SHT_DYNAMIC,
                          SHF_ALLOC + SHF_WRITE,
                          true, xObjectOffset,
                          dynstrSecIdx)) -> getIndex();

        symtab -> Sym ("_DYNAMIC",
                       dynamicVA,
                       0,
                       STB_GLOBAL,
                       STT_OBJECT,
                       dynamicSecIdx);
    }

    if (xDoDebug) {
        // write all public symbols to .symtab
        NAMES.Iterate (add2symtab);
    }

    // emit .strtab and .symtab

    dword strtabVA = VASA -> getVirtualAddr();
    VASA -> allocateVirtualSpace (strtabVA, strtab->getContents()->GetLen());

    Elf32_Half strtabSecIdx =
     (new ELFSection (strtab->getContents (),
                      strtabVA, strtab->getContents ()->GetLen(),
                      shstrtab->ID (".strtab"),
                      SHT_STRTAB,
                      SHF_ALLOC,
                      true, xObjectOffset)) -> getIndex();

    dword symtabVA = VASA -> getVirtualAddr();
    VASA -> allocateVirtualSpace (symtabVA, symtab->getSymTable()->GetLen());

    new ELFSection (symtab->getSymTable(),
                    symtabVA, symtab->getSymTable()->GetLen(),
                    shstrtab->ID (".symtab"),
                    SHT_SYMTAB,
                    SHF_ALLOC,
                    true, xObjectOffset,
                    strtabSecIdx, 1,
                    sizeof (Elf32_Sym));

    if (doDynamic) {
        // DYNAMIC Segment
        new ELFSegment (PT_DYNAMIC,
                        dynamicSecOffset,
                        dynamicVA,
                        dynamicSection->GetLen(),
                        dynamicSection->GetLen(),
                        PF_R + PF_W,
                        4);
    }

    // Allocate CPB
    if (CPB != NULL) {
        CPBStart = VASA->getVirtualAddr();
        VASA->allocateVirtualSpace (CPBStart, CPB->Index);
    }

    // Control Parameter Block Segment (the last)
    if (CPB != NULL) {
        new ELFSection (CPB,
                        CPBStart, CPB->GetLen(),
                        shstrtab->ID ("config"),
                        SHT_PROGBITS,
                        SHF_ALLOC,
                        true, xObjectOffset);

        ASSERT (nSegments == numberOfSegments);
    }

    Elf32_Word shstrtab_ID = shstrtab->ID (".shstrtab");

    // .shstrtab Section
    ELFSection * shStrTabSec = new ELFSection (shstrtab -> getContents(),
                                               0, 0,
                                               shstrtab_ID,
                                               SHT_STRTAB,
                                               0,
                                               false, 1);

    Elf32_Half shstrtab_index = shStrTabSec->getIndex();

    dword secHeadersOffset = FilePointer;

    ASSERT (nSegments == numberOfSegments);
    ASSERT (nSections == numberOfSections);

    // Form ELF header

    memset (&ELFHeader, 0, sizeof(ELFHeader));

    ELFHeader.e_ident [EI_MAG0]    = ELFMAG0;
    ELFHeader.e_ident [EI_MAG1]    = ELFMAG1;
    ELFHeader.e_ident [EI_MAG2]    = ELFMAG2;
    ELFHeader.e_ident [EI_MAG3]    = ELFMAG3;
    ELFHeader.e_ident [EI_CLASS]   = ELFCLASS32;
    ELFHeader.e_ident [EI_DATA]    = ELFDATA2LSB;
    ELFHeader.e_ident [EI_VERSION] = EV_CURRENT;

    ELFHeader.e_type      = xDLLFlag ? ET_DYN : ET_EXEC;
    ELFHeader.e_machine   = EM_386;
    ELFHeader.e_version   = EV_CURRENT;
    ELFHeader.e_entry     = (xWasEntryPoint && !xDLLFlag) ? ((Elf32_Addr) entryPointVA) : 0;
    ELFHeader.e_phoff     = sizeof (Elf32_Ehdr);
    ELFHeader.e_shoff     = secHeadersOffset;
    ELFHeader.e_flags     = 0;
    ELFHeader.e_ehsize    = sizeof (Elf32_Ehdr);
    ELFHeader.e_phentsize = sizeof (Elf32_Phdr);
    ELFHeader.e_phnum     = (Elf32_Half) nSegments;
    ELFHeader.e_shentsize = sizeof (Elf32_Shdr);
    ELFHeader.e_shnum     = (Elf32_Half) nSections;
    ELFHeader.e_shstrndx  = shstrtab_index;

    // Write output file

    VerboseMessage("Write ELF: start writing output file %s\n", name);

    FILE * fp;
    OutFile = fp = fopen (name, "wb");
    if (OutFile == NULL)
        Message(xFATAL, msgUNABLE_TO_OPEN_FILE, name);

    output_file_name = dup(name, strlen(name)+1);

    WriteOutFile (&ELFHeader,    sizeof(Elf32_Ehdr), 0);
    WriteOutFile (ProgramHeader, sizeof(Elf32_Phdr)*nSegments, sizeof(Elf32_Ehdr));

    if (!xUseProgramInterpreter) {
        AlignOutFile (xObjectOffset);
    }

    WriteSections ();

    WriteOutFile (SectionHeader, sizeof(Elf32_Shdr)*nSections, secHeadersOffset);

    if (xDoDebug) {
        CreateDebugInfo();

        AlignOutFile (xObjectOffset);
        WriteOutFile (DebugInfo->GetData(), DebugInfo->GetLen(), nbytes);
        delete DebugInfo;
    }

    if (fclose (OutFile))
        Message(xFATAL, msgUNABLE_TO_WRITE_FILE, name);

    OS->ChMod (name, FMODE_EXECUTABLE);

    xfree (filler);
    xfree (output_file_name);
}

close_namespace

