#include <windows.h>
#include <stdio.h>

#include "rcparse.h"
#include "rcexpr.h"
#include "rcio.h"
#include "rcscan.h"
#include "rcsup.h"
#include "rcerr.h"

#define SETD(type,value) *((type *)optr)++=(type)(value)

#define HBUFSIZE 4000
#define ABUFSIZE 32768

static char hbuf[HBUFSIZE];
static char *abuf;
static char fbuf[2000];

static char *optr;

static char Zero[6]={0,0,0,0,0,0};
static long hpos,dpos;

static int IconId=1;
static LPCSTR ResID,ResTP;
static DWORD ResVER,ResCH;
static WORD ResLI,ResAT;
static int ExRes;
static char ResName[2000];
static char ResType[2000];
int ResCP=CP_ACP;

static int was_d_font,was_d_class,was_d_menu;
static DWORD d_st,d_exst;
static WORD d_x,d_y,d_w,d_h;
static WORD d_point;
static LPCSTR d_menu,d_class;
static char d_caption[2000];
static char d_font[2000];
static char d_classbuf[2000];
static char d_menubuf[2000];
static WORD nfonts=0;
typedef struct
   {
      WORD ordinal;
      char entry[0x96];
   } fontentry;

static fontentry *fontdir=0;

void SetLI(int li)
{
   ResLI=li;
}

void SetCP(int cp)
{
   ResCP=cp;
}

static void WriteProlog(void)
{
DWORD fheader[8]={0,32,0x0000ffff,0x0000ffff,0,0,0,0};
   OutputWrite(fheader,sizeof(fheader));
}

static void wrord(LPCSTR d)
{
int len;
   if ((DWORD)d&0xffff0000UL)
   {
      len=MultiByteToWideChar(ResCP,MB_ERR_INVALID_CHARS,
                              d,-1,(LPWSTR)optr,(HBUFSIZE-(optr-hbuf+10))/2);
      if (!len)
      {
         syserror(MSG_UNICODE,(DWORD)d);
         return;
      }
      optr+=len*2;
   } else
   {
      SETD(WORD,0xffff);
      SETD(WORD,(DWORD)d&0x0ffffUL);
   }
}

static void wrstrnz(LPCSTR d,int slen)
{
int len;
static char buf[8192];
   if (!slen)
      return;
   len=MultiByteToWideChar(ResCP,MB_ERR_INVALID_CHARS,
                           d,slen,(LPWSTR)buf,4096);
   if (!len)
   {
      syserror(MSG_UNICODE,(DWORD)d);
      return;
   }
   OutputWrite(buf,len*2);
}

static void WriteHeader(void)
{
int len;
   hpos=OutputGetPos();
   optr=hbuf+8;
   wrord(ResTP);
   wrord(ResID);
   if ((optr-hbuf)%4)
      SETD(WORD,0); // padding
   SETD(DWORD,0);
   SETD(WORD,ResAT);
   SETD(WORD,ResLI);
   SETD(DWORD,ResVER);
   SETD(DWORD,ResCH);
   len=optr-hbuf;
   *(DWORD *)(hbuf+4)=(DWORD)len;
   OutputWrite(hbuf,len);
   dpos=OutputGetPos();
}

static void WriteTrailer(void)
{
long pos;
DWORD dlen;
   pos=OutputGetPos();   
   OutputSetPos(hpos);
   dlen=(DWORD)(pos-dpos);
   OutputWrite((char *)&dlen,4);
   OutputSetPos(pos);
   if ((pos-dpos)%4)
      OutputWrite(Zero,4-(pos-dpos)%4);
}

static void PFileName(void)
{
   if (CurToken==S_T_STRING)
      strcpy(fbuf,CurStrValue);
   else
   {
      memcpy(fbuf,TokenB,TokenE-TokenB);
      fbuf[TokenE-TokenB]=0;
   }
}

static void PUser(void)
{
int len;
   WriteHeader();
   if (CurToken==S_T_KEYWORD&&CurKeyVal==K_BEGIN)
      for (;;)
      {
         GetToken(S_P_DELIMITERS|S_P_LQUOTES);
         if (CurToken==S_T_NUMBER)
            OutputWrite(&CurValue,LFlag?4:2);
         else if (CurToken==S_T_STRING)
            if (LFlag)
               wrstrnz(CurStrValue,CurStrLen);
            else
               OutputWrite(CurStrValue,CurStrLen);
         else
         {
            error(MSG_NUMBER_EXPECTED);
            return;
         }
         GetToken(S_P_DELIMITERS);
         if (CurToken==S_T_KEYWORD&&CurKeyVal==K_END)
            return;
         if (CurToken==S_T_DELIM&&CurDlmVal==D_COMMA)
            continue;
         error(MSG_EXPECTED,",");
         return;
      }
   else
   {
      PFileName();
      AuxOpen(fbuf);
      while ((len=AuxRead(abuf,ABUFSIZE))>0)
         OutputWrite(abuf,len);
      AuxClose();
   }
}

static void PBitmap(void)
{
BITMAPFILEHEADER fhd;
int len;
   WriteHeader();
   PFileName();
   AuxOpen(fbuf);
   if (AuxRead((char *)&fhd,sizeof(fhd))!=sizeof(fhd)||fhd.bfType!=0x4d42)
   {
      error(MSG_BITMAP,fbuf);
      AuxClose();
      return;
   }
   for (;;)
   {
      len=AuxRead(abuf,ABUFSIZE);
      if (len<=0)
         break;
      OutputWrite(abuf,len);
   }
   AuxClose();
   return;
}
   
void PCharacteristics(void)
{
   GetToken(0);
   if (CurToken==S_T_NUMBER)
      ResCH=CurValue;
   else
      error(MSG_NUMBER_EXPECTED);
}

void PVersion(void)
{
   GetToken(0);
   if (CurToken==S_T_NUMBER)
      ResVER=CurValue;
   else
      error(MSG_NUMBER_EXPECTED);
}

void PLanguage(void)
{
UINT mj,mn;
   GetToken(S_P_DELIMITERS);
   if (CurToken==S_T_NUMBER)
      mj=CurValue;
   else
      error(MSG_NUMBER_EXPECTED);
   GetToken(S_P_DELIMITERS);
   if (!(CurToken==S_T_DELIM&&CurDlmVal==D_COMMA))
      error(MSG_EXPECTED,",");
   GetToken(S_P_DELIMITERS);
   if (CurToken==S_T_NUMBER)
      mn=CurValue;
   else
      error(MSG_NUMBER_EXPECTED);
   ResLI=MAKELANGID(mj,mn);
}

static void DoCaption(void)
{
   GetToken(S_P_DELIMITERS|S_P_QUOTES|S_P_ESCAPES);
   if (CurToken!=S_T_STRING)
   {
      error(MSG_STRING_EXPECTED);
      return;
   }
   strcpy(d_caption,CurStrValue);
}

static void DoClass(void)
{
   GetToken(S_P_DELIMITERS|S_P_QUOTES|S_P_ESCAPES);
   if (CurToken==S_T_NUMBER)
      d_class=(LPCSTR)CurValue;
   else if (CurToken==S_T_STRING)
   {
      strcpy(d_classbuf,CurStrValue);
      d_class=d_classbuf;
   } else
   {
      error(MSG_CLASS);
      return;
   }
   was_d_class=1;
}

static void DoDMenu(void)
{
   GetToken(0);
   if (CurToken==S_T_NUMBER)
      d_menu=(LPCSTR)CurValue;
   else
   {
      memcpy(d_menubuf,TokenB,TokenE-TokenB);
      d_menubuf[TokenE-TokenB]=0;
      d_menu=d_menubuf;
   }
   was_d_menu=1;
}

static void DoFont(void)
{
   d_point=(WORD)CalcExpr(0);
   if (!(CurToken==S_T_DELIM&&CurDlmVal==D_COMMA))
   {
      error(MSG_EXPECTED,",");
      return;
   }
   GetToken(S_P_DELIMITERS|S_P_QUOTES);
   if (CurToken!=S_T_STRING)
   {
      error(MSG_STRING_EXPECTED);
      return;
   }
   strcpy(d_font,CurStrValue);
   was_d_font=1;
}




static void DoDialog(void)
{
WORD nitem;
long lpos,npos;
WORD c_x,c_y,c_w,c_h,c_id;
DWORD c_st,c_exst;
const keyType *c_rec;
static char c_text[2000];
LPCSTR c_class;
int len;
int IsControl;

   WriteHeader();
   npos=OutputGetPos();
   if (npos%4)
      OutputWrite(Zero,npos%4);
   npos+=npos%4;
   optr=hbuf;

   if (was_d_font)
      d_st|=DS_SETFONT;
   SETD(DWORD,d_st);
   SETD(DWORD,d_exst);
   SETD(WORD,0);
   SETD(WORD,d_x);
   SETD(WORD,d_y);
   SETD(WORD,d_w);
   SETD(WORD,d_h);
   if (was_d_menu)
      wrord(d_menu);
   else
      SETD(WORD,0);
   if (was_d_class)
      wrord(d_class);
   else
      SETD(WORD,0);
   wrord(d_caption);
   if (was_d_font)
   {
      SETD(WORD,d_point);
      wrord(d_font);
   }
   if ((optr-hbuf)%4)
      SETD(WORD,0); // padding
   len=optr-hbuf;
   OutputWrite(hbuf,len);
   nitem=0;
   GetToken(S_P_DELIMITERS);
   for (;;)
   {
      if (CurToken!=S_T_KEYWORD)
      {
         error(MSG_EXPECTED,"END");
         return;
      }
      if (CurKeyVal==K_END)
         break;
      if (!(CurKeyRec->type&KEY_TYPE_CONTROL))
      {
         error(MSG_EXPECTED,"END");
         return;
      }
      ++nitem;
      IsControl=CurKeyVal==K_CONTROL;
      c_rec=CurKeyRec;
      c_st=c_rec->style|WS_CHILD|WS_VISIBLE;
      c_class=(LPCSTR)c_rec->sclass;
      c_text[0]=0;
      GetToken(S_P_DELIMITERS|S_P_QUOTES|S_P_ESCAPES);
      if (CurToken==S_T_STRING)
      {
         strcpy(c_text,CurStrValue);
         GetToken(S_P_DELIMITERS);
         if (CurToken==S_T_DELIM&&CurDlmVal==D_COMMA)
            GetToken(S_P_DELIMITERS);
      }
      c_id=(WORD)CalcExpr(1);
      if (!(CurToken==S_T_DELIM&&CurDlmVal==D_COMMA))
         error(MSG_EXPECTED,",");
      if (IsControl)
      {
         DoClass();
         GetToken(S_P_DELIMITERS);
         if (!(CurToken==S_T_DELIM&&CurDlmVal==D_COMMA))
            error(MSG_EXPECTED,",");
         c_class=d_class;
         c_st=CalcExprVal(0,0,c_st);
         if (!(CurToken==S_T_DELIM&&CurDlmVal==D_COMMA))
            error(MSG_EXPECTED,",");
      }
      c_x=(WORD)CalcExpr(0);
      if (!(CurToken==S_T_DELIM&&CurDlmVal==D_COMMA))
         error(MSG_EXPECTED,",");
      c_y=(WORD)CalcExpr(0);
      if (!(CurToken==S_T_DELIM&&CurDlmVal==D_COMMA))
         error(MSG_EXPECTED,",");
      c_w=(WORD)CalcExpr(0);
      if (!(CurToken==S_T_DELIM&&CurDlmVal==D_COMMA))
         error(MSG_EXPECTED,",");
      c_h=(WORD)CalcExpr(0);
      c_exst=0;
      if (CurToken==S_T_DELIM&&CurDlmVal==D_COMMA)
      {
         if (!IsControl)
            c_st=CalcExprVal(0,0,c_st);
         if (CurToken==S_T_DELIM&&CurDlmVal==D_COMMA)
            c_exst=CalcExpr(0);
      }
      optr=hbuf;
      SETD(DWORD,c_st);
      SETD(DWORD,c_exst);
      SETD(WORD,c_x);
      SETD(WORD,c_y);
      SETD(WORD,c_w);
      SETD(WORD,c_h);
      SETD(WORD,c_id);
      wrord(c_class);
      wrord(c_text);
      SETD(WORD,0);
      len=optr-hbuf;
      lpos=OutputGetPos();
      if (lpos%4)
         OutputWrite(Zero,2);
      OutputWrite(hbuf,len);
   }
   lpos=OutputGetPos();
   OutputSetPos(npos+8);
   OutputWrite(&nitem,2);
   OutputSetPos(lpos);
}

static void PDialog(void)
{
DWORD lResVER,lResCH;
WORD lResLI;
int ddone;
int dontget;

   d_exst=0;
   if (CurToken==S_T_KEYWORD&&CurKeyVal==K_EXSTYLE)
   {
      GetToken(S_P_DELIMITERS);
      if (!(CurToken==S_T_DELIM&&CurDlmVal==D_ASSIGN))
         error(MSG_EXPECTED,",");
      d_exst=CalcExpr(0);
      if (!(CurToken==S_T_DELIM&&CurDlmVal==D_COMMA))
         error(MSG_EXPECTED,",");
      GetToken(S_P_DELIMITERS);
   }
   d_x=(WORD)CalcExpr(1);
   if (!(CurToken==S_T_DELIM&&CurDlmVal==D_COMMA))
      error(MSG_EXPECTED,",");
   d_y=(WORD)CalcExpr(0);
   if (!(CurToken==S_T_DELIM&&CurDlmVal==D_COMMA))
      error(MSG_EXPECTED,",");
   d_w=(WORD)CalcExpr(0);
   if (!(CurToken==S_T_DELIM&&CurDlmVal==D_COMMA))
      error(MSG_EXPECTED,",");
   d_h=(WORD)CalcExpr(0);
   lResVER=ResVER;
   lResCH=ResCH;
   lResLI=ResLI;
   d_st=WS_POPUP|WS_BORDER|WS_SYSMENU;
   was_d_font=0;
   was_d_menu=0;
   was_d_class=0;
   d_caption[0]=0;

   ddone=0;
   dontget=1;
   for (;;)
   {
      if (!dontget)
         GetToken(S_P_DELIMITERS);
      dontget=0;
      if (CurToken==S_T_KEYWORD)
         switch (CurKeyVal)
         {
            case K_LANGUAGE: PLanguage();continue;
            case K_VERSION: PVersion();continue;
            case K_CHARACTERISTICS: PCharacteristics();continue;
            case K_CAPTION: DoCaption();continue;
            case K_CLASS: DoClass();continue;
            case K_MENU: DoDMenu();continue;
            case K_FONT: DoFont();continue;
            case K_STYLE: d_st=CalcExpr(0);dontget=1;continue;
            case K_EXSTYLE: d_exst=CalcExpr(0);dontget=1;continue;
            case K_BEGIN: DoDialog(); ddone=1;break;
         }
      break;
   }
   if (!ddone)
      error(MSG_EXPECTED,"BEGIN");
   ResVER=lResVER;
   ResCH=lResCH;
   ResLI=lResLI;
}

WORD GetMenuOption(void)
{
int wascomma;
WORD flags;
   wascomma=0;
   flags=0;
   for (;;)
   {
      GetToken(S_P_DELIMITERS);
      if (CurToken==S_T_DELIM&&CurDlmVal==D_COMMA)
         if (wascomma)
         {
            error(MSG_COMMA);
            return 0;
         } else
         {
            wascomma=1;
            continue;
         }
      if (CurToken==S_T_KEYWORD)
      {
         switch (CurKeyVal)
         {
            case K_GRAYED:  flags|=0x1;wascomma=0;continue;
            case K_INACTIVE:  flags|=0x2;wascomma=0;continue;
            case K_BITMAP:  flags|=0x4;wascomma=0;continue;
            case K_OWNERDRAW:  flags|=0x100;wascomma=0;continue;
            case K_CHECKED:  flags|=0x8;wascomma=0;continue;
            case K_MENUBARBREAK:  flags|=0x20;wascomma=0;continue;
            case K_MENUBREAK:  flags|=0x40;wascomma=0;continue;
         }
      }
      break;
   }
   if (wascomma)
      error(MSG_COMMA);
   return flags;
}


static void DoMenuLoop(void);

static WORD DoPopup(void)
{
WORD flags;
int len;
   GetToken(S_P_DELIMITERS|S_P_QUOTES|S_P_ESCAPES);
   if (CurToken!=S_T_STRING)
   {
      error(MSG_STRING_EXPECTED);
      return 0;
   }
   optr=hbuf+2;
   wrord(CurStrValue);
   len=optr-hbuf;
   optr=hbuf;
   GetToken(S_P_DELIMITERS);
   flags=0;
   if (CurToken==S_T_DELIM&&CurDlmVal==D_COMMA)
      flags=GetMenuOption();
   flags|=0x10;
   SETD(WORD,flags);
   OutputWrite(hbuf,len);
   if (!(CurToken==S_T_KEYWORD&&CurKeyVal==K_BEGIN))
   {
      error(MSG_EXPECTED,"BEGIN");
      return 0;
   }
   DoMenuLoop();
   GetToken(S_P_DELIMITERS);
   return flags;
}

static WORD DoMenuItem(void)
{
WORD flags,id;
int len;
   GetToken(S_P_DELIMITERS|S_P_QUOTES|S_P_ESCAPES);
   if (CurToken==S_T_KEYWORD&&CurKeyVal==K_SEPARATOR)
   {
      GetToken(S_P_DELIMITERS);
      OutputWrite(Zero,6);
      return 0;
   }
   if (CurToken!=S_T_STRING)
   {
      error(MSG_STRING_EXPECTED);
      return 0;
   }
   optr=hbuf+4;
   wrord(CurStrValue);
   len=optr-hbuf;
   optr=hbuf;
   GetToken(S_P_DELIMITERS);
   if (!(CurToken==S_T_DELIM&&CurDlmVal==D_COMMA))
   {
      error(MSG_EXPECTED,",");
      return 0;
   }
   id=(WORD)CalcExpr(0);
   flags=0;
   if (CurToken==S_T_DELIM&&CurDlmVal==D_COMMA)
      flags=GetMenuOption();
   SETD(WORD,flags);
   SETD(WORD,id);
   OutputWrite(hbuf,len);
   return flags;
}
      
static void DoMenuLoop(void)
{
long pos,tpos;
WORD f;
   GetToken(S_P_DELIMITERS);
   for (;;)
   {
      if (CurToken!=S_T_KEYWORD)
      {
         error(MSG_EXPECTED,"END");
         return;
      }
      if (CurKeyVal==K_END)
         break;
      if (CurKeyVal==K_POPUP)
      {
         pos=OutputGetPos();
         f=DoPopup();
      } else if (CurKeyVal==K_MENUITEM)
      {
         pos=OutputGetPos();
         f=DoMenuItem();
      } else
      {
         error(MSG_EXPECTED,"MENUITEM");
         return;
      }
   }
   tpos=OutputGetPos();
   OutputSetPos(pos);
   f|=0x80;
   OutputWrite((char *)&f,2);
   OutputSetPos(tpos);
}

static void DoMenu(void)
{
   WriteHeader();
   OutputWrite(Zero,4);
   DoMenuLoop();
}


static void PMenu(void)
{
DWORD lResVER,lResCH;
WORD lResLI;
int ddone;
int first;

   lResVER=ResVER;
   lResCH=ResCH;
   lResLI=ResLI;
   ddone=0;
   first=1;
   for (;;)
   {
      if (!first)
         GetToken(S_P_DELIMITERS);
      first=0;
      if (CurToken==S_T_KEYWORD)
         switch (CurKeyVal)
         {
            case K_LANGUAGE: PLanguage();continue;
            case K_VERSION: PVersion();continue;
            case K_CHARACTERISTICS: PCharacteristics();continue;
            case K_BEGIN: DoMenu(); ddone=1;break;
         }
      break;
   }
   if (!ddone)
      error(MSG_EXPECTED,"BEGIN");
   ResVER=lResVER;
   ResCH=lResCH;
   ResLI=lResLI;
}

static void DoAccelerators(void)
{
WORD event;
WORD id;
WORD flags;
long pos;
int wascomma;
   WriteHeader();
   GetToken(S_P_DELIMITERS|S_P_QCHAR);
   for (;;)
   {
      if (CurToken==S_T_KEYWORD&&CurKeyVal==K_END)
         break;
      if (CurToken==S_T_QCHAR)
      {
         event=(WORD)CurValue;
         GetToken(S_P_DELIMITERS);
      }else
         event=(WORD)CalcExpr(1);
      if (!(CurToken==S_T_DELIM&&CurDlmVal==D_COMMA))
      {
         error(MSG_EXPECTED,",");
         return;
      }
      id=(WORD)CalcExpr(0);
      wascomma=0;
      flags=0;
      for (;;)
      {
         if (CurToken==S_T_DELIM&&CurDlmVal==D_COMMA)
            if (wascomma)
            {
               error(MSG_COMMA);
               return;
            } else
            {
               wascomma=1;
               GetToken(S_P_DELIMITERS);
               continue;
            }
         if (!wascomma)
            break;
         wascomma=0;
         if (CurToken==S_T_KEYWORD)
            switch (CurKeyVal)
            {
               case K_ASCII: break;
               case K_VIRTKEY: flags|=FVIRTKEY; break;
               case K_NOINVERT: flags|=FNOINVERT; break;
               case K_ALT: flags|=FALT; break;
               case K_SHIFT: flags|=FSHIFT; break;
               case K_CONTROL: flags|=FCONTROL; break;
               default: error(MSG_COMMA);
            }
         else
            error(MSG_COMMA);
         GetToken(S_P_DELIMITERS|S_P_QCHAR);
      }
      optr=hbuf;
      SETD(WORD,flags);
      SETD(WORD,event);
      SETD(WORD,id);
      SETD(WORD,0);
      OutputWrite(hbuf,8);
   }
   pos=OutputGetPos();
   OutputSetPos(pos-8);
   flags|=0x80;
   OutputWrite(&flags,2);
   OutputSetPos(pos);
}

static void PAccelerators(void)
{
DWORD lResVER,lResCH;
WORD lResLI;
int ddone;
int first;

   lResVER=ResVER;
   lResCH=ResCH;
   lResLI=ResLI;
   ddone=0;
   first=1;
   for (;;)
   {
      if (!first)
         GetToken(S_P_DELIMITERS);
      first=0;
      if (CurToken==S_T_KEYWORD)
         switch (CurKeyVal)
         {
            case K_LANGUAGE: PLanguage();continue;
            case K_VERSION: PVersion();continue;
            case K_CHARACTERISTICS: PCharacteristics();continue;
            case K_BEGIN: DoAccelerators(); ddone=1;break;
         }
      break;
   }
   if (!ddone)
      error(MSG_EXPECTED,"BEGIN");
   ResVER=lResVER;
   ResCH=lResCH;
   ResLI=lResLI;
}

typedef struct
{
   int id;
   WORD len;
   char *wstr;
} STEL;

static int l_cmp(const STEL *e1,const STEL *e2)
{
   if (e1->id<e2->id)
      return -1;
   return e1->id>e2->id;
}

static void DoString(void)
{
int last;
int base;
STEL *l;
int nel;
int sel;
int i;

   nel=0;
   sel=100;
   l=Malloc(sizeof(STEL)*100);
   GetToken(S_P_DELIMITERS);
   for (;;)
   {
      if (CurToken==S_T_KEYWORD&&CurKeyVal==K_END)
         break;
      if (nel>=sel)
      {
         sel+=100;
         l=Realloc(l,sel*sizeof(STEL));
      }
      l[nel].id=(int)CalcExpr(1);
      if (!(CurToken==S_T_DELIM&&CurDlmVal==D_COMMA))
      {
         error(MSG_EXPECTED,",");
         return;
      }
      GetToken(S_P_DELIMITERS|S_P_QUOTES|S_P_LQUOTES| S_P_ESCAPES);
      if (CurToken!=S_T_STRING)
      {
         error(MSG_STRING_EXPECTED);
         return;
      }
      l[nel].len=CurStrLen16;
      if (CurStrLen16)
      {
         l[nel].wstr=Malloc(CurStrLen16*sizeof(wchar_t));
         memcpy(l[nel].wstr,CurStrValue16,CurStrLen16*sizeof(wchar_t));
      }else
         l[nel].wstr=0;
      ++nel;
      GetToken(S_P_DELIMITERS);
   }
   if (!nel)
   {
      error(MSG_EMPTY_BLOCK);
      Free(l);
      return;
   }
   qsort(l,nel,sizeof(STEL),l_cmp);
   last=-1;
   base=-1;
   for (i=0;i<nel;++i)
   {
      if (l[i].id>=base+16||base<0)
      {
         if (base>=0)
         {
            while (++last<base+16)
               OutputWrite(Zero,2);
            WriteTrailer();
         }
         base=l[i].id&~0x0f;
         ResID=MAKEINTRESOURCE((base>>4)+1);
         WriteHeader();
         last=base-1;
      }
      while (++last<l[i].id)
         OutputWrite(Zero,2);
      OutputWrite(&l[i].len,2);
      if (l[i].len) {
         OutputWrite(l[i].wstr,l[i].len*sizeof(wchar_t));
         Free(l[i].wstr);
      }
   }
   Free(l);
   while (++last<base+16)
      OutputWrite(Zero,2);
}

static void PString(void)
{
DWORD lResVER,lResCH;
WORD lResLI;
int ddone;
int first;

   lResVER=ResVER;
   lResCH=ResCH;
   lResLI=ResLI;
   ddone=0;
   first=1;
   for (;;)
   {
      if (!first)
         GetToken(S_P_DELIMITERS);
      first=0;
      if (CurToken==S_T_KEYWORD)
         switch (CurKeyVal)
         {
            case K_LANGUAGE: PLanguage();continue;
            case K_VERSION: PVersion();continue;
            case K_CHARACTERISTICS: PCharacteristics();continue;
            case K_BEGIN: DoString(); ddone=1;break;
         }
      break;
   }
   if (!ddone)
      error(MSG_EXPECTED,"BEGIN");
   ResVER=lResVER;
   ResCH=lResCH;
   ResLI=lResLI;
}

static void PIcon(int IsIco)
{
LPCSTR gResID;
int len,size,rsize;

struct IH
{
   WORD res;
   WORD tp;
   WORD cn;
} hd;

struct RD
{
   byte bw,bh,bc,br;
   WORD wp,wb;
   DWORD lb;
   DWORD wo;
} *rd;

#define IBUFSIZE 1024
static char ibuf[IBUFSIZE];
int i;

   gResID=ResID;
   PFileName();
   AuxOpen(fbuf);
   len=AuxRead((char *)&hd,6);
   if (len!=6||hd.res||hd.tp!=(1+!IsIco)||!hd.cn)
   {
      error(MSG_FORMAT_FILE,IsIco?"ICON":"CURSOR");
      AuxClose();
      return;
   }
   rd=Malloc(sizeof(struct RD)*hd.cn);
   len=AuxRead((char *)rd,sizeof(struct RD)*hd.cn);
   if (len!=(int)sizeof(struct RD)*hd.cn)
   {
      error(MSG_FORMAT_FILE,IsIco?"ICON":"CURSOR");
      AuxClose();
      return;
   }
   for (i=0;i<hd.cn;++i)
   {
      AuxSetPos(rd[i].wo);
      ResID=MAKEINTRESOURCE(IconId);
      rd[i].wo=IconId;
      ++IconId;
      WriteHeader();
      rsize=rd[i].lb;
      if (!IsIco)
         OutputWrite(&rd[i].wp,4);
      while (rsize)
      {
         size=rsize<IBUFSIZE?rsize:IBUFSIZE;
         len=AuxRead(ibuf,size);
         if (len!=size)
         {
            error(MSG_FORMAT_FILE,IsIco?"ICON":"CURSOR");
            AuxClose();
            return;
         }
         OutputWrite(ibuf,len);
         rsize-=len;
      }
      WriteTrailer();
   }
   ResID=gResID;
   ResTP=MAKEINTRESOURCE(IsIco?14:12);
   WriteHeader();
   OutputWrite(&hd,6);
   for (i=0;i<hd.cn;++i)
   {
      if (!IsIco)
      {
         rd[i].wb=4;
         rd[i].wp=rd[i].bc;
         rd[i].bc=rd[i].bh*2;
         rd[i].bh=rd[i].br=0;
         rd[i].lb+=4;
      }
      OutputWrite(rd+i,sizeof(struct RD)-2);
   }
   Free(rd);
   AuxClose();
}

void Get4int(DWORD *w)
{
DWORD v1,v2;
   v1=CalcExpr(0);
   if (!(CurToken==S_T_DELIM&&CurDlmVal==D_COMMA))
   {
      error(MSG_EXPECTED,",");
      return;
   }
   v2=CalcExpr(0);
   if (!(CurToken==S_T_DELIM&&CurDlmVal==D_COMMA))
   {
      error(MSG_EXPECTED,",");
      return;
   }
   *w++=(v1<<16)|(v2&0x0ffffUL);
   v1=CalcExpr(0);
   if (!(CurToken==S_T_DELIM&&CurDlmVal==D_COMMA))
   {
      error(MSG_EXPECTED,",");
      return;
   }
   v2=CalcExpr(0);
   *w=(v1<<16)|(v2&0x0ffffUL);
}


static long WrVerR(const char *Key,int IsStr,const char *val,int sval)
{
long pos;
   pos=OutputGetPos();
   if (pos%4)
   {
      OutputWrite(Zero,2);
      pos+=2;
   }
   optr=hbuf+2;
   SETD(WORD,IsStr?sval/2:sval);
   SETD(WORD,IsStr);
   wrord(Key);
   if (sval)
   {
      if ((optr-hbuf)%4)
         SETD(WORD,0);
      memcpy(optr,val,sval);
      optr+=sval;
   }
   *(WORD *)hbuf=optr-hbuf;
   OutputWrite(hbuf,optr-hbuf);
   return pos;
}

static void PValue(void)
{
char *p;
int IsStr;
int len;
   GetToken(S_P_DELIMITERS|S_P_QUOTES|S_P_ESCAPES);
   if (CurToken!=S_T_STRING)
   {
      error(MSG_STRING_EXPECTED);
      return;
   }
   strcpy(fbuf,CurStrValue);
   GetToken(S_P_DELIMITERS);
   if (!(CurToken==S_T_DELIM&&CurDlmVal==D_COMMA))
   {
      error(MSG_EXPECTED,",");
      return;
   }
   p=abuf;
   IsStr=1;
   for (;;)
   {
      GetToken(S_P_DELIMITERS|S_P_QUOTES|S_P_ESCAPES);
      if (CurToken==S_T_STRING)
      {
         len=MultiByteToWideChar(ResCP,MB_ERR_INVALID_CHARS,
                                 CurStrValue,-1,(LPWSTR)p,2000);
         if (!len)
         {
            syserror(MSG_UNICODE,(DWORD)CurStrValue);
            return;
         }
         p+=len*2;
      } else if (CurToken==S_T_NUMBER)
      {
         if (LFlag)
            *((DWORD *)p)++=CurValue;
         else
            *((WORD *)p)++=(WORD)CurValue;
         IsStr=0;
      } else
      {
         error(MSG_STRING);
         return;
      }
      GetToken(S_P_DELIMITERS);
      if (CurToken==S_T_DELIM&&CurDlmVal==D_COMMA)
         continue;
      break;
   }
   WrVerR(fbuf,IsStr,abuf,p-abuf);
}

static void PBlock(void)
{
long pos,lpos;
WORD len;
   GetToken(S_P_DELIMITERS|S_P_QUOTES|S_P_ESCAPES);
   if (CurToken!=S_T_STRING)
   {
      error(MSG_STRING_EXPECTED);
      return;
   }
   pos=WrVerR(CurStrValue,1,0,0);
   GetToken(S_P_DELIMITERS);
   if (!(CurToken==S_T_KEYWORD&&CurKeyVal==K_BEGIN))
   {
      error(MSG_EXPECTED,"BEGIN");
      return;
   }
   GetToken(S_P_DELIMITERS);
   for (;;)
   {
      if (CurToken==S_T_KEYWORD)
         if (CurKeyVal==K_BLOCK)
         {
            PBlock();
            continue;
         } else if (CurKeyVal==K_END)
            break;
         else if (CurKeyVal==K_VALUE)
         {
            PValue();
            continue;
         }
      error(MSG_EXPECTED,"BEGIN");
      return;
   }
   lpos=OutputGetPos();
   len=(WORD)(lpos-pos);
   OutputSetPos(pos);
   OutputWrite(&len,2);
   OutputSetPos(lpos);
   GetToken(S_P_DELIMITERS);
}

static void PVersionInfo(void)
{
VS_FIXEDFILEINFO vf;
long pos,lpos;
WORD len;
   WriteHeader();
   memset(&vf,0,sizeof(vf));
   vf.dwSignature=VS_FFI_SIGNATURE;
   vf.dwStrucVersion=VS_FFI_STRUCVERSION;
   for (;;)
   {
      if (CurToken!=S_T_KEYWORD)
      {
         error(MSG_EXPECTED,"BEGIN");
         return;
      }
      if (CurKeyVal==K_BEGIN)
         break;
      switch (CurKeyVal)
      {
         case K_FILEVERSION: Get4int(&vf.dwFileVersionMS);continue;
         case K_PRODUCTVERSION: Get4int(&vf.dwProductVersionMS);continue;
         case K_FILEFLAGSMASK: vf.dwFileFlagsMask=CalcExpr(0);continue;   
         case K_FILEFLAGS: vf.dwFileFlags=CalcExpr(0);continue;   
         case K_FILEOS: vf.dwFileOS=CalcExpr(0);continue;   
         case K_FILETYPE: vf.dwFileType=CalcExpr(0);continue;   
         case K_FILESUBTYPE: vf.dwFileSubtype=CalcExpr(0);continue;
      }
      error(MSG_EXPECTED,"BEGIN");
      return;
   }
   pos=WrVerR("VS_VERSION_INFO",0,(char *)&vf,sizeof(vf));
   GetToken(S_P_DELIMITERS);
   for (;;)
   {
      if (CurToken==S_T_KEYWORD)
         if (CurKeyVal==K_END)
            break;
         else if (CurKeyVal==K_BLOCK)
         {
            PBlock();
            continue;
         }
      error(MSG_EXPECTED,"END");
      return;
   }
   lpos=OutputGetPos();
   len=(WORD)(lpos-pos);
   OutputSetPos(pos);
   OutputWrite(&len,2);
   OutputSetPos(lpos);
}

static void PFont(void)
{
int len;
   if ((DWORD)ResID&0xffff0000)
   {
      error(MSG_FONT_ID);
      return;
   }
   WriteHeader();
   PFileName();
   AuxOpen(fbuf);
   if (AuxRead(abuf,0x96)!=0x96)
   {
      error(MSG_FONT,fbuf);
      AuxClose();
      return;
   }
   OutputWrite(abuf,0x96);
   nfonts++;
   fontdir=Realloc(fontdir,nfonts*sizeof(fontentry));
   memcpy(fontdir[nfonts-1].entry,abuf,0x96);
   fontdir[nfonts-1].entry[0x94]=0;
   fontdir[nfonts-1].entry[0x95]=0;
   fontdir[nfonts-1].ordinal=(WORD)ResID;
   while ((len=AuxRead(abuf,ABUFSIZE))>0)
      OutputWrite(abuf,len);
   AuxClose();
}

static void PFontDir(void)
{
   ResAT=0x30;
   ResTP=MAKEINTRESOURCE(7);
   ResID="FONTDIR";
   WriteHeader();
   OutputWrite(&nfonts,2);
   OutputWrite(fontdir,sizeof(fontentry)*nfonts);
}

void DoRC()
{
int done;
int ResT;
   abuf=Malloc(ABUFSIZE);
   ScanInit();
   WriteProlog();
   if (!setjmp(eof_jmp))
      for (;;)
      {
         done=1;
         GetToken(0);
         done=0;
         if (CurToken==S_T_KEYWORD&&(CurKeyType&KEY_TYPE_STATEMENT))
            switch (CurKeyVal)
            {
               case K_LANGUAGE: PLanguage(); continue;
               case K_CHARACTERISTICS: PCharacteristics(); continue;
               case K_VERSION: PVersion(); continue;
            }
         ExRes=0;
         if (CurToken==S_T_KEYWORD&&CurKeyVal==K_STRINGTABLE)
            ResT=(int)RT_STRING;
         else
         {
            if (CurToken==S_T_NUMBER)
            {
               if ((unsigned long)CurValue>65535)
                  warning(MSG_NUMBER);
               ResID=MAKEINTRESOURCE(CurValue&0x0ffffUL);
            } else
            {
               memcpy(ResName,TokenB,TokenE-TokenB);
               ResName[TokenE-TokenB]=0;
               CharUpper(ResName);
               ResID=ResName;
            }
            ResT=0;
            GetToken(0);
            if (CurToken==S_T_KEYWORD&&(CurKeyType&KEY_TYPE_RESOURCE))
               switch (CurKeyVal)
               {
                  case K_ACCELERATORS: ResT=(int)RT_ACCELERATOR; break;
                  case K_ANICURSOR:    ResT=(int)RT_ANICURSOR; break;
                  case K_ANIICON:      ResT=(int)RT_ANIICON; break;
                  case K_BITMAP:       ResT=(int)RT_BITMAP; break;
                  case K_CURSOR:       ResT=(int)RT_CURSOR; break;
                  case K_DIALOGEX:     ExRes=1;
                  case K_DIALOG:       ResT=(int)RT_DIALOG; break;
                  case K_FONT:         ResT=(int)RT_FONT; break;
                  case K_ICON:         ResT=(int)RT_ICON; break;
                  case K_MENUEX:       ExRes=1;
                  case K_MENU:         ResT=(int)RT_MENU; break;
                  case K_MESSAGETABLE: ResT=(int)RT_MESSAGETABLE; break;
                  case K_RCDATA:       ResT=(int)RT_RCDATA; break;
                  case K_VERSIONINFO:  ResT=(int)RT_VERSION; break;
               }
         }
         if (ResT)
            ResTP=MAKEINTRESOURCE(ResT);
         else
         {
            memcpy(ResType,TokenB,TokenE-TokenB);
            ResType[TokenE-TokenB]=0;
            CharUpper(ResType);
            ResTP=ResType;
         }
         ResAT=0x30;
         for (;;)
         {
            GetToken(S_P_DELIMITERS|S_P_QUOTES);
            if (CurToken==S_T_KEYWORD&&(CurKeyType&KEY_TYPE_ATTRIBUTE))
               switch (CurKeyVal)
               {
                  case K_MOVEABLE:     ResAT|=0x10;   continue;
                  case K_FIXED:        ResAT&=~0x10;  continue;
                  case K_PURE:         ResAT|=0x20;   continue;
                  case K_IMPURE:       ResAT&=~0x20;  continue;
                  case K_PRELOAD:      ResAT|=0x40;   continue;
                  case K_LOADONCALL:   ResAT&=~0x40;  continue;
                  case K_DISCARDABLE:  ResAT|=0x1000; continue;
                  default: error(MSG_ATTR); continue;
               }
            break;
         }
         switch (ResT)
         {
               case RT_ACCELERATOR: PAccelerators();break;
               case RT_ANICURSOR: error(MSG_IMPLEMENT);break;
               case RT_ANIICON: error(MSG_IMPLEMENT);break;
               case RT_BITMAP: PBitmap();break;
               case RT_CURSOR: ResAT|=0x1000;PIcon(0);;break;
               case RT_DIALOG: PDialog();break;
               case RT_FONT: ResAT|=0x1000;PFont();break;
               case RT_ICON: ResAT|=0x1000;PIcon(1);break;
               case RT_MENU: PMenu(); break;
               case RT_MESSAGETABLE: PUser();break;
               case RT_STRING: PString();break;
               case RT_VERSION: PVersionInfo();break;
               case RT_RCDATA:
               default: PUser();break;
         }
         WriteTrailer();
      }
   if (!done)
      error(MSG_EOF);
   else
   {
      if (nfonts)
      {
         PFontDir();
         WriteTrailer();
      }
   }
}
