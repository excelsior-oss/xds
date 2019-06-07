
#define K_LOCAL     128

struct lname {
        ident name;
        char  kind;
};

struct pubname {
        ident            name;
        Segment        * seg;
        struct group   * group;
        int              offset;
        struct pubname * next;
        char             kind;
};

struct expdef {
        ident           intname, extname;
        struct expdef * next;
        short           ordinal;
        Bool            intname_present;
};

/*----------------------------------------------------------------------------*/

class OMF0Reader {
  protected:
    char * filename;
    char * libname;
    byte * rawdata;
    byte * cur_pos;
    long   size;

    /* Record-local info */
    byte   rec_type;
    dword  rec_len;
    dword  rec_idx;
    byte * rec_start;

    int    LibPageSize;
    byte   DebugInfo_Format;

    /* LNAMES/LLNAMES Table */
    struct lname * LNamesTable;
    int            NLNames;
    int            LNamesSize;

    struct linnum * last_linnum;

    inline ident getLName (int index) {
        if (index == 0) {
            return INVALID_ID;
        }
        if ((index < 0) || (index > NLNames)) {
            VerboseMessage ("Incorrect LNAME index %d in record at offset %XH\n", index, rec_start -  rawdata);
            Message(xFATAL, msgINVALID_FILE_FORMAT, filename);
        }
        return LNamesTable[index - 1].name;
    }

    /* EXTDEFs, LEXTDEFs, COMDEFs, LCOMDEFs Names Table */
    GrowingArray<ident> ExtNamesTable;

    /* PUBDEFs & LPUBDEFs Table */
    struct pubname * PubNamesTable;
    int              NPubNames;
    int              PubNamesSize;

    /* Segments Table */
    Segment ** SegsTable;
    int        NSegs;
    int        SegsSize;
    Segment *  LedataSeg;
    int        LastOffset;
    
    /* Targets */
    byte   target_kinds [4];
    void * targets [4];

    /* EXPDEFs List */
    struct expdef * ExpdefsList;
    void NewExpdef (ident intname, ident extname, word ordinal, Bool intname_present);
    void ClearExpdefsList(void);

    inline byte GetByte() {
        rec_idx++;
        return * cur_pos ++;
    }
    inline word GetWord() {
        word w = * (word *) cur_pos;
        rec_idx +=2;
        cur_pos +=2;
        return w;
    }
    inline dword GetWordOrDword() {
        if (rec_type & REC32)
            return GetDword ();
        else
            return GetWord ();
    }
    inline dword GetDword() {
        dword d = * (dword *) cur_pos;
        rec_idx +=4;
        cur_pos +=4;
        return d;
    }
    inline ident GetIdent() {
        ident id  =  NAMES.Str2Index( (char *)(cur_pos + 1), *cur_pos );
        rec_idx += (*cur_pos) + 1;
        cur_pos += (*cur_pos) + 1;
        return id;
    }
    inline ident GetZStringIdent (void) {
        char * end = (char *) memchr (cur_pos, '\0', rec_len - rec_idx);
        if (end == NULL) {
            Message (xFATAL, msgINVALID_ZERO_TERM_STRING, GetCurrentOBJFileName (), rec_start -  rawdata, rec_idx);
            return INVALID_ID;
        }
        int len = (end - ((char *) cur_pos));
        ident id  = NAMES.Str2Index ((char *) cur_pos, len);
        rec_idx += (len + 1);
        cur_pos += (len + 1);
        return id;
    }

    inline char * GetZString (int sizeToReserve = 0) {
        char * end = (char *) memchr (cur_pos, '\0', rec_len - rec_idx);
        if (end == NULL) {
            Message (xFATAL, msgINVALID_ZERO_TERM_STRING, GetCurrentOBJFileName (), rec_start -  rawdata, rec_idx);
            return NULL;
        }
        int len = (end - ((char *) cur_pos));
        char * str = (char *) xalloc (len + 1 + sizeToReserve);
        memcpy (str, cur_pos, len + 1);
        rec_idx += (len + 1);
        cur_pos += (len + 1);
        return str;
    }

    inline word GetIndex() {
        word index = * cur_pos ++;
        rec_idx++;
        if (index & 0x80) {
            index = (word) ((index & 0x7f) << 8) | (* cur_pos ++);
            rec_idx++;
        }
        return index;
    }
    inline ident GetSegName() {
        return SegsTable [ GetIndex() - 1 ] -> name;
    }
    inline void GetRawMem(byte * mem, int len) {
        memcpy(mem, cur_pos, len);
        rec_idx += len;
        cur_pos += len;
    }
    inline void Advance (dword len) {
        rec_idx += len;
        cur_pos += len;
    }
    inline int Available () {
        if (rec_idx < rec_len)
            return (rec_len - rec_idx);
        else
            return 0;
    }
    inline char * GetString (int len, Bool allocate_forever = false) {
        char * str = (char *) (allocate_forever ? allocateForever (len + 1) : xalloc (len + 1));
        GetRawMem ((byte *)str, len);
        str [len] = 0;
        return str;
    }

    void NewModule (void);
    void EndModule (void);

    void CollectExpdefs (void);

    void lnames (char kind);
    void ledata (void);
    void lidata_block (int * offset, Segment * s);
    void lidata (void);
    void linnum (void);

  public:

    OMF0Reader();
    ~OMF0Reader();
};
