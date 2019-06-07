#ifndef __sf_lib
#define __sf_lib

#define INI_LISTS 1 // some dirty functions

#define RUS 1   // To understand the russian symbols

  #ifndef CR
    #define CR  '\r'
  #endif
  #ifndef LF
    #define LF  '\n'
  #endif
  #ifndef TAB
    #define TAB '\t'
  #endif

#define CTG_ALPHA    0x0001
#define CTG_DIGIT    0x0002
#define CTG_HEXDIGIT 0x0004
#define CTG_SPACE    0x0008
extern  int          aCharTags [256];  // CTG_ tags for each char
extern  char         achToUp   [256];  // achToUp[ch] == ToUpper(ch)

inline BOOL sf_isdigit    (char ch)  {return !!(CTG_DIGIT    & aCharTags[ch]); }
inline BOOL sf_ishexdigit (char ch)  {return !!(CTG_HEXDIGIT & aCharTags[ch]); }
inline BOOL sf_isspace    (char ch)  {return !!(CTG_SPACE    & aCharTags[ch]); }
inline BOOL sf_isalpha    (char ch)  {return !!(CTG_ALPHA    & aCharTags[ch]); }
inline BOOL sf_isalnum    (char ch)  {return !!((CTG_ALPHA|CTG_DIGIT) & aCharTags[ch]); }
inline BOOL sf_iswordbreak(char ch)  {return ! ((CTG_DIGIT|CTG_ALPHA) & aCharTags[ch]); }
inline char sf_toupper    (char ch)  {return achToUp[ch];}
inline char*sf_skipspaces (char*pch) {while (sf_isspace(*pch)) pch++; return pch;}
int         sf_strnicmp   (char *s1, char *s2, int n);
int         sf_stricmp    (char *s1, char *s2);

char      *sf_fname2short (char *szFName); // Returns the short name OR "" when szFName=NULL

  #ifndef min
    #define min(a,b)      ((a)<(b) ? (a) : (b))
  #endif
  #ifndef max
    #define max(a,b)      ((a)>(b) ? (a) : (b))
  #endif
  #ifndef abs
    #define abs(a)        ((a)<0   ? -(a) : (a))
  #endif


  typedef struct _LINELIST{
    _LINELIST  *next;
    LONG        textlen;
    char        text[1];
  } LINELIST;
  typedef LINELIST *PLINELIST;

  void          sf_freelist     (LINELIST *pll);
  LINELIST     *sf_applist      (LINELIST *pll, char *szText);
  void          sf_cutlist      (LINELIST **ppll);

  /* sf_fgets: В отличие от стандарта, строка будет без CR+LF, с нулем */

  char         *sf_fgets        (char *pch, ULONG buflen, HFILE hf);
  
  char         *sf_mallocstr    (char *psz);
  char         *sf_mallocconcat (char *pszMalloced, char *psz);

#if INI_LISTS

  /* --- sf_fskipbl: ---------------------------------------------------
  /
  /  Читает файл построчно во внут. буфер.
  /  Скипает пробелы, переводы строк и %,#-комментарии.
  /
  /  pch: In
  /    Если pch = SF_FSKIPBEG то скипнет с начала файла и вернет адр. первого
  /      непробельного символа;
  /    Если pch = SF_FSKIPLINE то начнет скипать с начала очередной строки;
  /    Иначе pch должен быть позицией после разобранной части в ранее
  /      считаном буфере. Тогда скипнет и вернет адрес для дальнейшего разбора.
  /
  /  Return:
  /    Адрес - sz строка (всегда со значимым символом в начале)
  /     0     - конец файла.
  */

  #define       SF_FSKIPBEG     ((char*)(LONG)0)
  #define       SF_FSKIPLINE    ((char*)(LONG)1)

  char         *sf_fskipbl      (HFILE hf, char *pch);

  /* --- sf_ini_read -------------------------------------------
  /  Reads from hf list named as szListName
  /
  /  File format is:
  /   {<list_name> = "<list_item>" {,"<list_item>"};}
  /  '#' - comment line
  /
  /  List read terminates when error and returns
  /    read part of the list.
  /------------------------------------------------------------*/

  LINELIST     *sf_ini_read     (HFILE hf, char *szListName);

#endif //INI_LISTS

  /*--- SymTable class -----------------------------------------
  /  AddName(char *sz, LONG id) - добавить имя sz с идентификатором
  /                               id (не равным -1)
  /  SearchName(char *sz)       - вернет ид-р имени или -1,
  /                               если имя не найдено.
  /------------------------------------------------------------*/

  class SymTable
  {
  public:
                  SymTable        ()  {pelem = 0;};
                 ~SymTable        ()  {ClearTree(this->pelem);}
    void          AddName         (char *sz, LONG id);
    void          Clear           ();
    inline LONG   SearchName      (char *sz)
    {
      ELEM         *p = pelem;
      int           i;
      while(p)
      {
        if (!(i=strcmp(sz,p->text))) return p->id;
        if (i<0) p = p->left;
        else     p = p->right;
      }
      return -1;
    }
  private:
    struct        ELEM
    {
      ELEM       *left;
      ELEM       *right;
      LONG        id;
      char        text[1];
    };
    void          ClearTree       (ELEM *p);
    ELEM         *pelem;
  };

//-------- I n i t i a l i z a t i o n   f i l e s (windows-like) -----------//

LONG sf_GetPrivateProfileInt(PSZ  szSectionName, // [section] name
                             PSZ  szKeyName,     // keyname = ...
                             LONG lDefault,      // value to return when error
                             PSZ  szFileName);   // full source filename

int sf_GetPrivateProfileString(PSZ  szSectionName,   // [section] name
                               PSZ  szKeyName,       // keyname = ...
                               PCSZ szDefault,       // string to return when error
                               PSZ  szReturnedString,// result string buffer
                               int  nSize,           // result string size
                               PSZ  szFileName);     // full source filename
                               // returns: strlen(szReturnedString) when success
                               //          0 when error

int sf_GetPrivateProfileStringLen
                              (PSZ  szSectionName,   // [section] name
                               PSZ  szKeyName,       // keyname = ...
                               PSZ  szFileName);     // full source filename
                               // returns: strlen(string) when success
                               //          -1             when error

BOOL sf_TestPrivateProfileSection(PSZ  szSectionName,   // [section] name
                                  PSZ  szFileName);     // full source filename
                                  // returns: TRUE  - section exists
                                  //          FALSE - no such section

#endif // ifndef __sf_lib


