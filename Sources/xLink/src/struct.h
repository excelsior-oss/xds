#include "xdefs.h"

#include "args.h"
#include "idents.h"
#include "xmem.h"

/*-------------------------------------------------------------------------*/
/*                           PUBLICS                                       */
/*-------------------------------------------------------------------------*/

#define K_PUBDEF        1
#define K_COMDEF        2
#define K_WEAKDEF       3
#define K_IMPORT        4
#define K_JIMPORT       5

#define K_MASK          7

#define K_IMPFUNC       16     /* 0 - variable, 1 - function */
#define K_BY_ORDINAL    32
#define K_PROCESSED     64
#define K_USED          128

#define K_TYPE_MASK     0xFF00
#define K_TYPE_SHIFT    8

#define T_UNKNOWN       0
#define T_DATA          1
#define T_CODE          2
#define T_TYPEDESC      5

#define type2kind(type)  (((type) << K_TYPE_SHIFT) & K_TYPE_MASK)
#define kind2type(kind)  (((kind) & K_TYPE_MASK) >> K_TYPE_SHIFT)

class Segment;

class nameInfo : public ForeverObject {
  public:

    int       kind;

    Segment * seg;
    int       offset;
};


class publicNameInfo : public nameInfo {
  public:
    publicNameInfo (Segment *s, int offs, int type) {
        kind   = K_PUBDEF | type2kind (type);
        seg    = s;
        offset = offs;
    }
};


class commonNameInfo : public nameInfo {
  private:
    int length;

  public:
    commonNameInfo (int len) {
        kind   = K_COMDEF;
        seg    = NULL;
        offset = 0;
        length = len;
    }

    void mergeCommon (int len) {
        length = max (length, len);
    }

    void createSegment ();
};


class weakNameInfo : public nameInfo {
  private:
    ident fwdname;

  public:
    weakNameInfo (ident name) {
        kind    = K_WEAKDEF;
        seg     = NULL;
        offset  = 0;
        fwdname = name;
    }

    inline ident getForwardName () {
        return fwdname;
    }
};

struct fixupLocationList {
    dword * location;
    struct fixupLocationList * next;
};

class importNameInfo : public nameInfo {
  private:

    struct fixupLocationList * fixups;
    ident modulename;
    ident ver;

    union {
        ident name;
        dword ordinal;
    };

  public:

    inline Bool byOrdinal () { return ((kind & K_BY_ORDINAL) == K_BY_ORDINAL); }

    importNameInfo (int _kind, ident _modulename, int _entry, ident _ver = INVALID_ID) {
        kind       = _kind;
        seg        = NULL;
        offset     = 0;
        fixups     = NULL;
        modulename = _modulename;
        ver        = _ver;
        if (byOrdinal ())
            ordinal = (dword)_entry;
        else
            name    = (ident)_entry;
    }

    inline dword getOrdinal () {
        ASSERT (byOrdinal ());
        return ordinal;
    }

    inline ident getName () {
        ASSERT (! byOrdinal ());
        return name;
    }

    inline ident getModuleName () {
        return modulename;
    }

    inline ident getVersion () {
        return ver;
    }

    inline Bool equalTo (int _kind, ident _modulename, int _entry) {
        return (kind       == _kind)       &&
               (modulename == _modulename) &&
               (byOrdinal () ? (ordinal == (dword)_entry) : (name == (ident)_entry));
    }

    void newFixup (dword * location) {
        struct fixupLocationList * fl = (struct fixupLocationList *) allocateForever (sizeof(struct fixupLocationList));
        fl -> location = location;
        fl -> next     = fixups;
        fixups = fl;
    }

    void patchFixups (dword value) {
        for (struct fixupLocationList * fl = fixups; fl; fl = fl->next)
            *(fl -> location) = value;
    }
};

class jimportNameInfo : public nameInfo {
  private:

    struct fixupLocationList * fixups;

  public:
    struct JImportGroup      * group;
    dword                      jidataOffset;

    jimportNameInfo (int _kind, struct JImportGroup *_group) {
        kind         = _kind;
        seg          = NULL;
        offset       = 0;
        fixups       = NULL;
        group        = _group;
        jidataOffset = (dword) (-1);
    }

    void newFixup (dword * location) {
        struct fixupLocationList * fl = (struct fixupLocationList *) allocateForever (sizeof(struct fixupLocationList));
        fl -> location = location;
        fl -> next     = fixups;
        fixups = fl;
    }

    void patchFixups (dword value) {
        for (struct fixupLocationList * fl = fixups; fl; fl = fl->next)
            *(fl -> location) = value;
    }
};


/*-------------------------------------------------------------------------*/

extern ident * ImportedNames;
extern int NumberOfImportedNames;

extern void NewPublicName (ident name, Segment * s, int offset, int type = T_DATA);
extern void NewCommonName (ident name, int length);
extern importNameInfo * NewImportName (ident name, int kind, ident modulename, int entry, ident version = INVALID_ID);
extern void NewWeakName   (ident name, ident defname);

/*-------------------------------------------------------------------------*/
/*                           GROUPS                                        */
/*-------------------------------------------------------------------------*/

struct elem {
        ident           name;
        struct elem *   next;
};

struct group {
        ident           name;
        struct elem  *  segs;
        struct group *  next;
};

extern struct group * GroupList;

/*-------------------------------------------------------------------------*/
/*                            SEGMENTS                                     */
/*-------------------------------------------------------------------------*/

struct Line {
        word   line;
        int    offset;
};

class  OBJFile;
class  LocalTypesTable;
struct collection;
struct fixup;

// segment flags
#define sf_processed   0x00000001  /* segment was processed             */
#define sf_16bit       0x00000002  /* 16 bit segment                    */
#define sf_collected   0x00000004  /* segment was added into collection */
#define sf_linkable    0x00000008  /* segment can be added to image     */


class Segment : public ForeverObject {
  private:
    int   flags;    // flags of segment, bitset of sf_*
    ident grpname;  // name of segment group
    ident keyname;  // key name of segment (used in writing/reading link info)

    LocalTypesTable* localTypesTable;

    int                 length;
    byte                * text;

  public:
    Segment (ident _name,
             ident _grpname,
             ident _clazz,
             Bool  __16bit,
             dword _length,
             dword _alignment = 1,
             LocalTypesTable* localTypesTable = NULL);

    void allocateFixups (int nfixups);

    inline Bool isProcessed () const {
        return ((flags & sf_processed) != 0);
    }

    inline Bool is16bit () const {
        return ((flags & sf_16bit) != 0);
    }

    inline Bool isCollected () const {
        return ((flags & sf_collected) != 0);
    }

    inline Bool isLinkable () const {
        return ((flags & sf_linkable) != 0);
    }

    inline void markAsProcessed () {
        flags |= sf_processed;
    }

    inline void markAsCollected () {
        flags |= sf_collected;
    }

    inline ident getName () const {
        return name;
    }

    inline ident getClazz () const {
        return clazz;
    }

    inline ident getGroupName () const {
        return grpname;
    }

    const LocalTypesTable* getLocalTypesTable() const {
        return localTypesTable;
    }

    inline int getLen () const {
        return length;
    }

    inline void setLen (int len) {
        ASSERT ((text == NULL) || (len <= length));
        this->length = len;
    }

    virtual byte* getText ();
    virtual void setText (byte* text, int len);
    virtual void copyTextTo (byte* dest);
    virtual void freeText ();

    ident getKeyName ();

    OBJFile           * file;
    struct collection * collection;
    ident               name;
    ident               clazz;
    ident               overlay;
    dword               address;

    struct fixup *      fixups;
    int                 nfixups;
    int                 fixupssize;

    struct Line *       lines;
    int                 nlines;
    int                 linessize;

    Segment *           next;           /* В объектном файле */
    Segment *           link;           /* В collection      */
    dword               alignment;

#if defined (OBSOLETE)
    char                combination;
    char                attributes;
#endif
};


class DataSource {
public:
    virtual int getLen() = 0;
    virtual void copyDataTo(byte* dest) = 0;
    virtual ~DataSource() {}
};


class CustomDataSourceSegment : public Segment {
private:
    DataSource* dataSource;

public:
    CustomDataSourceSegment (ident name_,
                             ident grpname_,
                             ident clazz_,
                             Bool _16bit_,
                             dword length_,
                             dword alignment_ = 1,
                             LocalTypesTable* localTypesTable_ = NULL)
        : Segment(name_, grpname_, clazz_, _16bit_, 0, alignment_, localTypesTable_)
        , dataSource(NULL)
    {
        ASSERT(length_ == 0);
    }

    void setDataSource(DataSource* src) {
        dataSource = src;
        setLen(src->getLen());
    }

    virtual byte* getText ();
    virtual void setText (byte* text, int len);
    virtual void copyTextTo (byte* dest);
    virtual void freeText ();
};

/*-------------------------------------------------------------------------*/
/*                              FIXUPS                                     */
/*-------------------------------------------------------------------------*/

#define JUMP_CODE 0x25FF

//  Fixup kinds

#define FIXUP_ADDRESS32       1
#define FIXUP_SELFRELOFFS32   2
#define FIXUP_OFFSET32NB      3
#define FIXUP_FAR16_16        4
#define FIXUP_FAR16_32        5
#define FIXUP_TDINDEX16       6
#define FIXUP_TDINDEX32       7
#define FIXUP_JAVASTRING32    8
#define FIXUP_BYTESTRING32    9
#define FIXUP_CONSTADDRESS32  10


//  Target kinds

#define TK_SEG          0
#define TK_GROUP        1
#define TK_ID           2
#define TK_FWD_SEG      3
#define TK_IMPORT_ID    10
#define TK_INVALID      11
#define TK_JIMPORT_ID   12

#define TK_TARGET32     30


// Source level fixups (object file level)

struct fixup {
        int     offset;                     /* Offset in the location's seg */
        void *  target;                     /* segment / group / ident      */
        int     fx_offset;                  /* Extra addendum               */
        byte    kind;                       /* Fixup kind,  FIXUP_*         */
        byte    k_target;                   /* Target kind, TK_*            */
};

extern void addFixup (Segment * seg, byte kind, dword offset, byte k_target, void * target, int fx_offset);

// Relocations (executable file level fixups)

// FAR16:xx relocations

struct fixupFAR16_xx {
        struct fixupFAR16_xx * next;
        dword                  source;
        dword                  target;
        byte                   kind;
};

extern struct fixupFAR16_xx * FAR16_xxFixups;

extern int NFixups;          /* Общее их количество в выдаваемой программе */

extern dword * Fix;
extern dword * FixRTrg;
extern dword * FixTrg;
extern dword   NFix;

struct impVarFixup {
        struct impVarFixup * next;
        importNameInfo     * name;
        dword              * address;
        dword                vadr;
        byte                 kind;
};

extern struct impVarFixup * ImpVarFixups;


class FixupTarget32 {
  private:
    dword target32;

  protected:
    FixupTarget32 () : target32 (0) {}

  public:
    inline dword getTargetValue () const {
        return target32;
    }

    inline void setTargetValue (dword value) {
        target32 = value;
    }
};


extern struct fixup * EntryPoint;

extern void setEntryPoint (byte   kind,
                           byte   k_target,
                           void * target,
                           int    fx_offset,
                           const char * fileName);

/*-------------------------------------------------------------------------*/
/*                           COLLECTIONS                                   */
/*-------------------------------------------------------------------------*/

struct collection {
        dword               base_vadr;
        ident               name;
        Segment           * segs;
        Segment           * last;       /* Undefined for sorted collection
                                               (e.g. idata) */
        struct collection * next;
};


extern struct collection * code, * code16, * data, * idata, * bss, * rdata;

#if defined (STACK_SEG)
extern struct collection * stack;
#endif

extern dword CodeStart, Code16Start, DataStart, BSSStart, IdataStart, RDataStart, JImportStart, JExportStart, CPBStart;
extern dword CodeLen,   Code16Len,   DataLen,   BSSLen,   IdataLen, RDataLen, JImportLen, JExportLen;
extern byte   * CodePtr, * Code16Ptr, * DataPtr, * IdataPtr, * RDataPtr;
extern Storage * JImportImage, * JExportImage;

extern dword ImportStart; /* Таблица переходов на импортированные имена -
                              внутри сегмента кода и прижато к его концу */

/*-------------------------------------------------------------------------*/
/*                              FILES                                      */
/*-------------------------------------------------------------------------*/

struct linnum {
        struct linnum * next;
        byte          * text;
        dword           length;
        Segment       * seg;
};

struct pub {
        struct pub    * next;
        ident           name;
        Segment       * seg;
        dword           offset;
};

extern StringTable * FileNames;

class OBJFile : public ForeverObject {
  private:
    char * filename;
    char * source;
    ident  filenameID;
    ident  uidName;

    Bool   processed;

    Bool   common;
    byte * commonData;
    dword  commonLen;

  public:
    OBJFile (const char * filename, const char * libname = NULL);

    const char * getFilename () const {
        return filename;
    }

    const char * getSource () const {
        return source;
    }

    inline void setFilename (char * _filename) {
        filename   = _filename;
        ident newFilenameID = FileNames->Str2Index (filename);

        if (uidName == filenameID) {
            uidName = newFilenameID;
        }
        filenameID = newFilenameID;
    }

    void setSource (char * source_) {
        xfree (source);
        source = source_;
    }

    inline Bool equalFilename (OBJFile * anotherFile) {
        return (filenameID == anotherFile->filenameID);
    }

    inline void setUIDName (const char * _uidName) {
        uidName = FileNames->Str2Index (_uidName);
    }

    inline const char * getUIDName () const {
        return FileNames->Index2Str (uidName);
    }

    OBJFile * findFileByUIDName (char * _uidName) {
        ident _uid = FileNames->Str2Index (_uidName);
        for (OBJFile * file = this; file; file = file->next) {
            if (file -> uidName == _uid)
                return file;
        }
        return NULL;
    }

    inline Bool isProcessed () {
        return processed;
    }

    inline void markAsProcessed () {
        processed = true;
    }

    void setCommon (const byte* commonData, dword commonLen) {
        this->common     = true;
        this->commonData = (byte *) xalloc (commonLen);
        this->commonLen  = commonLen;
        memcpy (this->commonData, commonData, commonLen);
    }

    Bool isCommon() {
        return common;
    }

    const byte * getCommonData() {
        ASSERT (isCommon());
        return commonData;
    }

    dword getCommonLen() {
        ASSERT (isCommon());
        return commonLen;
    }

    char *          lib;
    Segment       * segs;
    int             nsegs;
    OBJFile       * next;
    Segment       * types;
    Segment       * symbols;
    struct linnum * linnums;
    struct pub    * publics;
    int             nextrnls;        /* For COFF files only */
    ident *         extrnls;
    char *          dbgcomment;
    Bool            smart;
    Bool            idata_processed; /* For COFF files only, see specail hack to
                                        handle COFF import */
};

extern OBJFile  * FileList;
extern OBJFile  * CurrentFile;

extern void SetFile (char * filename, char * lib);

inline const char * GetCurrentOBJFileName () {
    return CurrentFile -> getFilename ();
}

/*-------------------------------------------------------------------------*/
/*                               DLLs                                      */
/*-------------------------------------------------------------------------*/

struct dll {
        ident   name;
        int     nentries;
        int     lookups;
        int     addresses;
        dword   nameID;       // for ELF writer: string table offset
};

extern int Ndlls, dllsSize;
extern struct dll * dlls;

extern struct dll * findDLL (ident dllname);
extern struct dll * getDLL (ident dllname);

/*-------------------------------------------------------------------------*/
/*                            PROCESSING                                   */
/*-------------------------------------------------------------------------*/

extern void InitIR ();
extern void ProcessIR (void);

/*-------------------------------------------------------------------------*/

extern char * Description;

/*-------------------------------------------------------------------------*/

extern Bool SymbolInExe (struct fixup * f, dword * address);

extern void PrintFileBySeg (char *, Segment *);
extern void PrintFile (char *, OBJFile * );
extern int  Alignment (int size);

/*-------------------------------------------------------------------------*/
/*                         Resources                                       */
/*-------------------------------------------------------------------------*/

#define RES_OS_TYPE_NT  0
#define RES_OS_TYPE_OS2 1

class Resource {
  public:
    unichar  * resourceName;
    word       resourceID;   // if resourceName == NULL

    dword      datasize;
    byte     * data;
    word       MemoryFlags;
    byte       OSType;
  
    dword      DataVersion;
    word       LanguageId;
    dword      Version;
    dword      Characteristics;
  
    Resource * next;

    int compareNameID (Resource * aRes);
};

class ResourceDirectory {
  public:
    unichar           * typeName;
    word                typeID;    // if typeName == NULL
    Resource          * resList;
    ResourceDirectory * next;

    Bool addResource (unichar * resName, word resID, dword datasize, byte * data,
                      dword DataVersion, word MemoryFlags, word LanguageId, dword Version,
                      dword Characteristics, byte OSType);
};

extern ResourceDirectory * getResourceDirectory (unichar *typeName, word typeID);

extern ResourceDirectory * resourceDirectoryList;
extern int numberOfResources;
extern int numberOfResourceDirectories;

/*----------------------------------------------------------------------------*/
/*                           EXPORTS                                          */
/*----------------------------------------------------------------------------*/

/* Export -> flag */
#define EFLAG_SOURCE_OBJMOD       0x0001
#define EFLAG_SOURCE_CMDLINE      0x0002
#define EFLAG_SOURCE_EDFMOD       0x0004
#define EFLAG_SOURCE_SYNTHETIC    0x0080

#define EFLAG_NAME_EXPORTED       0x0008
#define EFLAG_VARIABLE            0x0010
#define EFLAG_NOIMPLIB            0x0020
#define EFLAG_VAR153              0x0040

#define EMASK_SOURCE     (EFLAG_SOURCE_EDFMOD | EFLAG_SOURCE_CMDLINE | EFLAG_SOURCE_SYNTHETIC | EFLAG_SOURCE_OBJMOD)
#define EMASK_SOURCE_EDF (EFLAG_SOURCE_EDFMOD | EFLAG_SOURCE_CMDLINE | EFLAG_SOURCE_SYNTHETIC)

extern int EXPORT_SOURCE_MASK;

class Export {
    public:
        Export * next;          // list link

        ident           extname;
        ident           intname;       // if seg == NULL
        ident           modname;
        int             flag;

        Segment       * seg;           // if seg != NULL
        dword           offset;

        Export * l, * r;        // AVL-tree by extname links
        short           bal;           // height(r)-height(l) == (-1,0,+1)

        word            ordinal;
};

extern Export *  Exports;
extern int              NExports;

extern Export * NewExport (ident extname, ident intname, Segment *seg, dword offset, word ordinal, short exp_def_src, ident modname);
extern Export * FindExportByExtName (ident extname);

// Export prepared for writer
extern Export ** ExportsTable;
extern int NumberOfExports;
extern int MaxOrdinal;

// Export prepared for implib
extern Export ** ILExportsTable;
extern int ILNumberOfExports;

/*----------------------------------------------------------------------------*/
/*                           Version control                                  */
/*----------------------------------------------------------------------------*/

/*
   VersionStamp = <Minor compiler version byte>
                  <Major compiler version byte>
                  <Internal compiler version byte>
                  <Edition>
*/

class VersionStamp {
  private:
    byte minorCompilerVersion;
    byte majorCompilerVersion;
    byte internalCompilerVersion;
    byte edition;

    const OBJFile * versionFile;

    Bool versionInitialized;

    VersionStamp(byte minorCompilerVersion,
                 byte majorCompilerVersion,
                 byte internalCompilerVersion,
                 byte edition,
                 const OBJFile* versionFile,
                 Bool versionInitialized);

    const char* getVersionSource() const;
    void check (const VersionStamp* ver);
    void merge (const VersionStamp* ver);

  public:
    VersionStamp ();

    void setVersionStamp (dword stamp);
    void setVersion (byte minor, byte major);
    void setEdition (byte edition);

    byte getMinorCompilerVersion() const {
        ASSERT (versionInitialized);
        return minorCompilerVersion;
    }

    byte getMajorCompilerVersion() const {
        ASSERT (versionInitialized);
        return majorCompilerVersion;
    }

    byte getEdition() const {
//        ASSERT (edition != JET_EDITION_UNKNOWN);
        return edition;
    }
};

extern VersionStamp* VerStamp;

/*----------------------------------------------------------------------------*/
/*                         MAP file                                           */
/*----------------------------------------------------------------------------*/

extern void CreateMapFile (char * name);

/*----------------------------------------------------------------------------*/
/*                          STRINGS MERGING                                   */
/*----------------------------------------------------------------------------*/

#define DEFAULT_OPTSTR_LEVEL 1
#define MAX_OPTSTR_LEVEL     3

#define MIN_OVERLAPPING_LEN_FOR_MERGE 1

template<class Char> class StringChars {
  public:
    Char * str;
    int len;
    int globalpos;
    dword spectrum;

    StringChars (int _len, Char * _str) {
        str       = (Char *) xalloc (_len * sizeof (Char));
        len       = _len;
        globalpos = -1;

        memcpy (str, _str, len * sizeof (Char));

        spectrum  = calcSpectrum ();
    }

    int hashCode (int offset, int count);

    int hashCode () {
        return hashCode (0, len);
    }

    dword calcSpectrum (int offset, int count);

    dword calcSpectrum () {
        return calcSpectrum (0, len);
    }

    int isSubstring   (StringChars<Char> * subchars);
    int isOverlapping (StringChars<Char> * subchars);

    ~StringChars () {
        xfree (str);
    }
};

#define I_OBJ_MERGED    0x01
#define I_CHARS_MERGED  0x02

template<class Char> class StringNode : public FixupTarget32 {
  private:
    void mergeNode (StringNode<Char> * _fwd, byte mergeKind, int posIncrement);

  public:
    union {
        StringChars<Char> * chars;
        StringNode<Char>  * fwd;
    };

    int pos;
    int len;
    int hash;

    dword spectrum;

    StringNode<Char> * next;
    StringNode<Char> * prev;
    StringNode<Char> * merged;

    byte ikind;

    StringNode () : chars (NULL), pos (0), len (0), hash (0), next (NULL), prev (NULL), merged(NULL), spectrum(0), ikind (0) {}

    inline Bool isMerged () {
        return ((ikind & (I_OBJ_MERGED | I_CHARS_MERGED)) != 0);
    }

    inline Bool isObjectMerged () {
        return ((ikind & I_OBJ_MERGED) != 0);
    }

    inline Bool areCharsMerged () {
        return ((ikind & I_CHARS_MERGED) != 0);
    }

    inline void mergeObject (StringNode<Char> * _fwd) {
        mergeNode (_fwd, I_OBJ_MERGED, 0);
    }

    inline void mergeChars (StringNode<Char> * _fwd, int posIncrement) {
        mergeNode (_fwd, I_CHARS_MERGED, posIncrement);
    }

    void mergeOverlapping (StringNode<Char> * subnode, int overLen);
};


template<class Char> class StringOptimizer {
  private:

  public:
    StringNode<Char> * List;
    int Nodes;
    int TotalLen;
    int ListLen;

    int getNodes    () { return Nodes;    }
    int getTotalLen () { return TotalLen; }

    StringOptimizer ();
    StringNode<Char> * AddString (int len, Char *string);
    void Optimize  (int Level);
};

typedef StringNode<unichar> JavaStringNode;

extern JavaStringNode * NewJavaString (int len, unichar *string);

typedef StringNode<byte> ByteStringNode;

extern ByteStringNode * NewByteString (int len, byte *string);

/*----------------------------------------------------------------------------*/
/*                             JEXPORT                                        */
/*----------------------------------------------------------------------------*/

#define JE_MAGIC  0x5058454A          /* 'JEXP' */

#define JExportSectionName ".jedata"

struct JExportGroup {
    int     hash;
    ident   groupname;
    int     entries;
    ident * itemnames;
};

extern struct JExportGroup ** JExportGroups;
extern int nJExportGroups;

extern int NewJExportGroup (int hash, ident groupname, int entries);

extern void NewJExportItem  (int groupIndex, int itemIndex, ident intname);

extern void FormJExportImage (void);

/*----------------------------------------------------------------------------*/
/*                             JIMPORT                                        */
/*----------------------------------------------------------------------------*/

#define JI_MAGIC  0x504D494A          /* 'JIMP' */

struct JImportDLL {
    ident name;
    struct JImportGroup * groups;
    struct JImportDLL * next;
    dword cKey;
    Bool used;
};

struct JImportGroup {
    struct JImportGroup * next;

    int     hash;
    ident   groupname;
    int     entries;
    int     entriesfilled;
    int     usedEntries;
    int   * itemindexes;
    ident * itemnames;
};

extern JImportDLL * JImportDLLs;

extern struct JImportDLL * getJImportDLL (ident dllname);

extern struct JImportGroup * GetJImportGroup (ident dllname, int hash, ident groupname, int entries);

extern void NewJImportItem  (struct JImportGroup * g, int itemIndex, ident intname, int type);

extern void FormJImportImage (void);

/*----------------------------------------------------------------------------*/
/*                    Virtual Address Space Allocation                        */
/*----------------------------------------------------------------------------*/

#define HIGH_MEMORY_LIMIT 0x80000000     /* 2 Gb - virtual address space limit */
#define WARN_MEMORY_LIMIT 0x40000000     /* 1 Gb - issue warning               */

class VASAllocator {
  private:
    dword base;
    dword addr;
    Bool  allocated;
  public:
    VASAllocator (dword baseAddr) {
        base  = baseAddr;
        addr  = baseAddr;
        allocated = false;
    }

    dword getVirtualAddr (void) {
        ASSERT (!allocated);
        addr = (addr + xObjectOffset - 1) & ~ (xObjectOffset - 1);
        allocated = true;
        return addr;
    }

    void allocateVirtualSpace (dword vaddr, dword size) {
        ASSERT (allocated);
        ASSERT (addr == vaddr);
        if ((addr + size < addr) || /* overflow */
            (addr + size >= HIGH_MEMORY_LIMIT)) 
        {
            Message (xFATAL, msgUNABLE_ALLOC_VIRTUAL_SPACE);
        }
        addr += size;
        allocated = false;
    }

    void discardVirtualSpace (dword vaddr) {
        ASSERT (allocated);
        ASSERT (addr == vaddr);
        allocated = false;
    }

    void checkVirtualSpaceSize () {
        ASSERT (!allocated);
        if ((addr - base) >= WARN_MEMORY_LIMIT) {
            Message (xWARNING, msgTOO_BIG_IMAGE);
        }
    }
};

extern VASAllocator * VASA;


/*----------------------------------------------------------------------------*/
/*                     Control Parameter Block (CPB)                          */
/*----------------------------------------------------------------------------*/

extern void NewCPBOption (int optionIndex, dword  value);
extern void NewCPBOption (int optionIndex, char * value);
extern void NewEncryptedCPBOption (int optionIndex, char * value); 

extern Storage * CPB;

/*----------------------------------------------------------------------------*/
/*                          DEFAULT IMAGE FORMAT                              */
/*----------------------------------------------------------------------------*/

#define xPE_IMAGE_FORMAT  0
#define xLX_IMAGE_FORMAT  1
#define xELF_IMAGE_FORMAT 2

#if defined(xos_OS2)

  #define xDEFAULT_IMAGE_FORMAT xLX_IMAGE_FORMAT

#elif defined(xos_WINNT)

  #define xDEFAULT_IMAGE_FORMAT xPE_IMAGE_FORMAT

#elif defined(xos_LINUX)

  #define xDEFAULT_IMAGE_FORMAT xELF_IMAGE_FORMAT

#else
  #error Unknown system
#endif

/*----------------------------------------------------------------------------*/
/*                                 LINK Info                                  */
/*----------------------------------------------------------------------------*/

extern void WriteLinkInfo ();
extern void ReadLinkInfo  ();

/*----------------------------------------------------------------------------*/
/*                      Extra thunks for JET method test                      */
/*----------------------------------------------------------------------------*/

extern ident newExtraThunk (byte k_target, void* target);
extern void generateExtraThunks ();

/*----------------------------------------------------------------------------*/
/*                   Linking of files as data/rdata segments                  */
/*----------------------------------------------------------------------------*/

extern void LinkFileIntoSegment (ident segmentKind,
                                 ident symbol,
                                 const char* filename);

/*----------------------------------------------------------------------------*/
