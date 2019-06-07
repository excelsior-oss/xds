#define INCL_DOS
#include <os2.h>
#include <stdlib.h>
#include <string.h>

#include "sf_lib.h"


/* --- String/Char functions --------------------------------*/
int  aCharTags[256];
char achToUp  [256];

struct INITMODULE
{
  INITMODULE();
} initmodule;
INITMODULE::INITMODULE()
{
  char c;
  memset(aCharTags, 0, sizeof(aCharTags));
  memset(achToUp,   0, sizeof(achToUp));
  for (c='0'; c<='9'; c++) aCharTags[c] |= CTG_DIGIT|CTG_HEXDIGIT;
  for (c='A'; c<='F'; c++) aCharTags[c] |= CTG_HEXDIGIT;
  for (c='a'; c<='f'; c++) aCharTags[c] |= CTG_HEXDIGIT;
  for (c='A'; c<='Z'; c++) aCharTags[c] |= CTG_ALPHA;
  for (c='a'; c<='z'; c++) aCharTags[c] |= CTG_ALPHA;
  aCharTags['_']                        |= CTG_ALPHA;
  #if RUS                                                   //
    for (c=0x80; c<=0xaf; c++) aCharTags[c] |= CTG_ALPHA;   // Rus (0x80..0xaf & 0xe0..0xef)
    for (c=0xe0; c<=0xef; c++) aCharTags[c] |= CTG_ALPHA;   //
  #endif                                                    //
  aCharTags[' '] |= CTG_SPACE;
  aCharTags[TAB] |= CTG_SPACE;
  aCharTags[CR]  |= CTG_SPACE;
  aCharTags[LF]  |= CTG_SPACE;
  for (c=255; c>0;    c--) achToUp[c] = c;
  for (c='a'; c<='z'; c++) achToUp[c] = c-'a'+'A';
  #if RUS                                               //
    for (c=0xa0; c<=0xaf; c++) achToUp[c] = c-0x20;     // Rus 0xan -> 0x8n
    for (c=0xe0; c<=0xef; c++) achToUp[c] = c-0x50;     //     0xen -> 0x9n
  #endif                                                //
}

int sf_stricmp (char*s1, char *s2)
{
  int i,j;
  for (i=0;;i++)
  {
    if ( j=sf_toupper(s1[i])-sf_toupper(s2[i]) )
      return j;
    if (!s1[i]) return 0;
  }
}

int sf_strnicmp  (char *s1, char *s2, int n)
{
  int i,j;
  for (i = 0 ; i < n; i++)
  {
    if ( j=sf_toupper(s1[i])-sf_toupper(s2[i]) ) return j;
    if (!s1[i]) return 0;
  }
  return 0;
}



/* --- LINELIST suppor --------------------------------------*/

void sf_freelist (LINELIST *pll)
{
  LINELIST *pll1;
  while (pll){
    pll1 = pll->next;
    free(pll);
    pll = pll1;
  }
}

LINELIST *sf_applist (LINELIST *pll, char *szText)
{
  LINELIST     *pllNew;
  LINELIST     *pllRet;
  LONG          textlen = strlen(szText);

  pllNew =  pllRet = (LINELIST*)malloc(textlen+sizeof(LINELIST));
  pllNew->next          = 0;
  pllNew->textlen       = textlen;
  strcpy(pllNew->text,szText);

  if (pll){
    pllRet = pll;
    while (pll->next) pll = pll->next;
    pll->next = pllNew;
  }
  return pllRet;
}

void sf_cutlist (LINELIST **ppll)
{
  PLINELIST pll = *ppll;
  if (pll)
  {
    *ppll = pll->next;
    free(pll);
  }
}

/* ---  File management ----------------------------------*/

char *sf_fname2short (char *szFName)
{
  if (!szFName) return "";
  char *pch;
  if ((pch=strrchr(szFName,'\\')) || (pch=strrchr(szFName,'/')) || (pch=strrchr(szFName,':')))
    pch++;
  else 
    pch = szFName;
  return pch;
}

char *sf_fgets(char *pch, ULONG buflen, HFILE hf)
// Out line is 0-terminatad w/o [CR]+LF
{
  ULONG         cb;
  ULONG         i = 0;

  while (i+1<buflen)
  {
    do{
      if (DosRead(hf,pch+i,1,&cb)) return 0; // Error
    } while(cb & pch[i]==CR);

    if (!cb)
      if (i){
        pch[i] = '\0';
        return pch;
      }
      else return 0;
    if (pch[i] == LF){
      pch[i] = '\0';
      return pch;
    }
    i++;
  }
  pch[i] = 0;
  return pch;
}

char *sf_mallocstr(char *psz)
{
  int len = psz ? strlen(psz) : 0;
  char *p = (char*)malloc(len+1);
  if (p) strcpy(p,psz?psz:"");
  return p;
}
char *sf_mallocconcat(char *pszMalloced, char *psz)
{
  int len0 = psz         ? strlen(pszMalloced) : 0;
  int len1 = pszMalloced ? strlen(psz)         : 0;
  
  char *p = (char*)realloc(pszMalloced, len0 + len1 + 1);
  if (p) strcpy(p+len0,psz?psz:"");
  return p;
}

#if INI_LISTS

/* --- sf_fskipbl: ---------------------------------------------------
/
/  Читает файл построчно во внут. буфер.
/  Скипает пробелы, переводы строк и #,%-комментарии.
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
/    0     - конец файла.
*/

char *sf_fskipbl(HFILE hf, char *pch)
{
  static char   achBuf[512];
  ULONG         ul;

  if (pch==SF_FSKIPBEG) DosSetFilePtr(hf,0,FILE_BEGIN,&ul);
  while(1){
    if (pch==SF_FSKIPBEG || pch==SF_FSKIPLINE){
      if (!sf_fgets(achBuf,sizeof(achBuf),hf)) return 0;
      pch = sf_skipspaces(achBuf);
      if (*pch=='#' || *pch=='%' || !*pch){
        pch = SF_FSKIPLINE;
        continue;
      }
    }
    else{
      pch = sf_skipspaces(pch);
      if (!*pch){
        pch = SF_FSKIPLINE;
        continue;
      }
    }
    return pch;
  }
}


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

LINELIST *sf_ini_read (HFILE hf, char *szListName)
{
  ULONG         ul;
  char         *pch;
  LINELIST     *pll = 0;
  char          szElem[512];

  DosSetFilePtr(hf,0,FILE_BEGIN,&ul);

  ul = strlen(szListName);
  while(1){ // Searching for the list start
    if (!(pch = sf_fskipbl(hf,SF_FSKIPLINE))) return 0;
    if (strncmp(pch,szListName,ul)) continue;
    pch = sf_fskipbl(hf,pch+ul);
    if (*(pch++)='=')           break; // Found
  }

  while(1){
    pch = sf_fskipbl(hf,pch);
    if (!pch || *pch!='"') return pll;  // Error
    pch++;
    for (ul=0;;ul++){
      szElem[ul] = *(pch++);
      if (szElem[ul] == '"'){
        szElem[ul] = 0;
        break; // Normal continue
      }
      if (!szElem[ul] || ul >= sizeof(szElem)-1) return pll; // Error
    }
    pll = sf_applist(pll,szElem);  //Append this element
    pch = sf_fskipbl(hf,pch);
    if (*pch!=',') return pll;     // Normal return :)
    pch++;
  }
}

#endif // INI_LISTS

/*------------- Symbol table -----------------------*/


void SymTable::ClearTree(ELEM *p)
{
  if (p)
  {
    if (p->left)  ClearTree(p->left);
    if (p->right) ClearTree(p->right);
    free(p);
  }
}


void SymTable::AddName(char *sz, LONG id)
{
  ELEM        **pp = &pelem;
  int           i;
  while(*pp)
  {
    i=strcmp(sz,(*pp)->text);
    if (i<0) pp = &(*pp)->left;
    else     pp = &(*pp)->right;
  }
  *pp = (ELEM*)malloc(sizeof(ELEM)+strlen(sz));
  (*pp)->left = (*pp)->right = 0;
  (*pp)->id   = id;
  strcpy((*pp)->text,sz);
}

void SymTable::Clear()
{
  ClearTree(pelem);
  pelem = 0;
}


//-------- I n i t i a l i z a t i o n   f i l e s (windows-like) -----------//

char    szIniFile[CCHMAXPATH] = "";   // Current ini file name
char   *pchIniBuf             = 0;    // It's contents (0-terminated)
char    szSection[100]        = "";   // Known section name
char   *pchSection            = 0;    //   1st section line

static BOOL prf_open(PSZ szFileName)
{
  ULONG         ulAction;
  HFILE         hf=0;
  ULONG         ul,filelen;
  if (szIniFile[0] && !sf_stricmp(szFileName,szIniFile)) return TRUE;
  free(pchIniBuf);
  pchIniBuf      = 0;
  pchSection     = 0;
  szIniFile[0]   = '\0';
  szSection[0]   = '\0';
  if (DosOpen(szFileName,&hf,&ulAction,0,FILE_NORMAL,FILE_OPEN,
              OPEN_ACCESS_READONLY|OPEN_SHARE_DENYNONE, (PEAOP2)0))
    return FALSE;
  DosSetFilePtr(hf,0,FILE_END,&filelen);
  pchIniBuf = (char*)malloc(filelen+1);
  if (pchIniBuf)
  {
    DosSetFilePtr(hf,0,FILE_BEGIN,&ul);
    if (DosRead(hf,pchIniBuf,filelen,&ul)) {free(pchIniBuf); pchIniBuf=0;}
    else pchIniBuf[ul] = '\0';
  }
  if (pchIniBuf) strcpy(szIniFile,szFileName);
  DosClose(hf);
  return !!pchIniBuf;
}

static BOOL prf_seeksection(PSZ szSectionName)
{
  char *pch         = pchIniBuf;
  BOOL  fFound      = FALSE;
  char  szFind[102] = "[";
  int   nFindLen;

  strcat(szFind,szSectionName);
  strcat(szFind,"]");           // szFind == '[section]'
  nFindLen = strlen(szFind);

  if (pchSection && !sf_stricmp(szSectionName,szSection)) return TRUE;

  if (szSectionName[0])
    while(pch && !fFound)
    {
      pch = sf_skipspaces(pch);
      fFound = !sf_strnicmp(szFind, pch, nFindLen);

      if (pch = strchr(pch,LF)) while(*pch==CR || *pch==LF) pch++;
    }

  pchSection = pch;
  strcpy(szSection,szSectionName);
  return !!pch;
}

char *prf_getkey(PSZ szKeyName)
{
  char       *pch = pchSection;
  int         nKeyLen = strlen(szKeyName);

  while(pch)
  {
    pch = sf_skipspaces(pch);
    if (*pch=='[') return 0;
    if (!sf_strnicmp(pch,szKeyName,nKeyLen))
      if (*(pch=sf_skipspaces(pch+nKeyLen)) == '=')
        return sf_skipspaces(pch+1);
    if (pch = strchr(pch,LF)) while(*pch==CR || *pch==LF) pch++;
  }
  return 0;
}

LONG sf_GetPrivateProfileInt(PSZ  szSectionName, // [section] name
                             PSZ  szKeyName,     // keyname = ...
                             LONG lDefault,      // value to return when error
                             PSZ  szFileName)    // full source filename
{
  LONG    lVal;
  char   *pchKey,*pchE;

  if (!prf_open(szFileName))             return lDefault;
  if (!prf_seeksection(szSectionName))   return lDefault;
  if (!(pchKey = prf_getkey(szKeyName))) return lDefault;

  lVal = strtol(pchKey,&pchE,0);
  if (pchKey==pchE)                      return lDefault;

  return lVal;
}


int sf_GetPrivateProfileString(PSZ  szSectionName,   // [section] name
                               PSZ  szKeyName,       // keyname = ...
                               PCSZ szDefault,       // string to return when error
                               PSZ  szReturnedString,// result string
                               int  nSize,           // result string size
                               PSZ  szFileName)      // full source filename
{
// returns min(result line length ,  nSize-1)
//         OR 0 when error occured
  LONG    nRet = 0;
  char   *pchKey,*pchE,*pchEOL;
  char    chTerm;

  if (!nSize) return 0;
  strncpy(szReturnedString,szDefault,nSize);
  szReturnedString[nSize-1] = 0;

  if (!prf_open(szFileName))             return nRet;
  if (!prf_seeksection(szSectionName))   return nRet;
  if (!(pchKey = prf_getkey(szKeyName))) return nRet;

  chTerm = pchKey[0];
  pchEOL = strpbrk(pchKey,"\r\n");
  if ((chTerm!='"' && chTerm!='\'')      ||
      !(pchE = strchr(pchKey+1,chTerm))  ||
      (pchEOL && pchE>pchEOL))
    return nRet;

  nRet = min(nSize-1, pchE-pchKey-1);
  strncpy(szReturnedString, pchKey+1, nRet);

  return nRet;
}


int sf_GetPrivateProfileStringLen
                              (PSZ  szSectionName,   // [section] name
                               PSZ  szKeyName,       // keyname = ...
                               PSZ  szFileName)      // full source filename
// returns: strlen(string) when success
//          -1             when error
{
  LONG    nRet = -1;
  char   *pchKey,*pchE,*pchEOL;
  char    chTerm;

  if (!prf_open(szFileName))             return nRet;
  if (!prf_seeksection(szSectionName))   return nRet;
  if (!(pchKey = prf_getkey(szKeyName))) return nRet;

  chTerm = pchKey[0];
  pchEOL = strpbrk(pchKey,"\r\n");
  if ((chTerm!='"' && chTerm!='\'')      ||
      !(pchE = strchr(pchKey+1,chTerm))  ||
      (pchEOL && pchE>pchEOL))
    return nRet;
  return pchE-pchKey-1;
}

BOOL sf_TestPrivateProfileSection(PSZ  szSectionName,   // [section] name
                                  PSZ  szFileName)      // full source filename
// returns: TRUE  - section exists
//          FALSE - no such section
{
  if (!prf_open(szFileName))             return FALSE;
  return !!prf_seeksection(szSectionName);
}
