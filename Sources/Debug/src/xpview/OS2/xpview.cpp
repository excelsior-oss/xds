#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#define  INCL_DOS
#define  INCL_WIN
#define  INCL_WINSTDFONT
#define  INCL_GPI
#include <os2.h>
#include "xpview.h"
#include "headwin.h"
#include "profile.h"

#define VPCLIENTCLASS  "VPClWinClass"
#define MM_SUBCLASS    WM_USER+2000
#define MM_PROFILE     WM_USER+2001
#define MM_APPLYWIDTHS WM_USER+2002

#define GCAUTO        1
#define HEAPLIMIT     4000000
#define GCTHRESHOLD   2000000
extern "C" void Profile_BEGIN(void);


//
// Globals:
//
HAB            hAB;
VIEWPORT       VPortComponents (VP_COMPONENTS);
VIEWPORT       VPortModPub     (VP_MODULES);
VIEWPORT       VPortProcs      (VP_PROCEDURES);
VIEWPORT       VPortLines      (VP_LINES);
LISTLINE       llBottom;
HISTORY        History;
HSWITCH        hSwitch;
PVIEWPORT      apVPorts[4] = {&VPortComponents, &VPortModPub, &VPortProcs, &VPortLines};
int            nComponents = 0;
BOOL           fClearDBI   = FALSE;

MRESULT EXPENTRY wpVPFrame   (HWND hWnd, ULONG ulMsg, MPARAM m1, MPARAM m2);
MRESULT EXPENTRY wpVPClient  (HWND hWnd, ULONG ulMsg, MPARAM m1, MPARAM m2);

PSZ GetSourceLine(PSZ szFile, ULONG ulLine)
{
  ULONG  IOErMsg(APIRET err, HWND hWnd, char *szCapt, char *szErr, char *szFile, ULONG MBF);
  static char  szFileInBuf[CCHMAXPATH] = "";
  static ULONG ulLineNow               = 0;
  static PSZ   pszLineNow;
  static PSZ   pszBuf                  = 0;
  static PSZ   pszBufTop;
  static char  szError[CCHMAXPATH+100] = "";
  static char  szRetLine[4000]         = "";
  if (sf_stricmp(szFile,szFileInBuf))
  {
    free(pszBuf);
    //--- Установка заголовка окна тут, вообще говоря, примочка...
    {
      char sz[CCHMAXPATH+50];
      sprintf(sz,"Source: %s",szFile);
      VPortLines.SetCaption(sz);
    }
    pszBuf      = 0;
    HFILE       hf;
    ULONG       ulAction, fsize, ul;
    APIRET      rc = DosOpen((PSZ)szFile,&hf,&ulAction,0,FILE_NORMAL,FILE_OPEN,
                     OPEN_FLAGS_NO_CACHE |OPEN_FLAGS_SEQUENTIAL| // Fastest mode?
                     OPEN_ACCESS_READONLY|OPEN_SHARE_DENYNONE, (PEAOP2)0);
    if (rc)
    {
      PSZ pszEr = (PSZ)IOErMsg(rc, 0, "", "Can't open file", szFile, MB_OK);
      strcpy(szError,pszEr ? pszEr : "Can't open file");
    }
    else
    {
      DosSetFilePtr(hf,0L,FILE_END,&fsize);
      if (fsize>4*1024*1024) sprintf(szError,"File %s is too large (more than 4Mb)!",szFile);
      else
      {
        DosSetFilePtr(hf,0L,FILE_BEGIN,&ul);
        pszBuf = (PSZ)malloc(fsize+1);
        if (!pszBuf) sprintf(szError,"Can't allocate memory (file %s)",szFile);
        else
        {
          DosRead(hf,pszBuf,fsize,&ul);
          DosClose(hf);
          pszBuf[ul] = '\0';
          pszBufTop  = pszBuf+ul;
          ulLineNow  = 0;
          pszLineNow = pszBuf;
          for (int i=0; i<ul; i++) if (pszBuf[i]==0x0d) pszBuf[i]=' ';
        }
      }
      DosClose(hf);
    }
    strcpy(szFileInBuf,szFile);
  }
  //--- файл в malloc()ed pszBuf..pszBufTop or pszBuf==0
  if (!pszBuf) return szError;
  if (ulLineNow > ulLine)
  {
    ulLineNow  = 0;
    pszLineNow = pszBuf;
  }
  while(ulLineNow < ulLine  &&  pszLineNow < pszBufTop)
  {
    PSZ szLF   = (PSZ)memchr(pszLineNow,0x0a,pszBufTop-pszLineNow);
    if (!szLF) break;
    pszLineNow = szLF+1;
    ulLineNow  ++;
  }
  if (ulLine!=ulLineNow)
    sprintf(szRetLine,"Line %u : out of range (file %s)",ulLine,szFile);
  else
  {
    PSZ szLF  = (PSZ)memchr(pszLineNow,0x0a,pszBufTop-pszLineNow);
    if (!szLF) szLF = pszBufTop;
    int len   = min(szLF-pszLineNow, sizeof(szRetLine)-1);
    strncpy(szRetLine,pszLineNow,len);
    szRetLine[len] = 0;
  }
  return szRetLine;
}

/*-------------------------------------------------------------------------------------------*/
/*------------------------+-/                            \-+------- 1.*****   ---------------*/
/*-------------------------<  LISTLINE class realisation  >-------- 2. ** *** ---------------*/
/*------------------------+-\                            /-+--------3.*** **  ---------------*/
/*-------------------------------------------------------------------------------------------*/
LISTLINE::LISTLINE()
{
  memset(this,0,sizeof(*this));
  lineKind     = BOTTOM;
  nGlobRelTime = -1;
  nModRelTime  = -1;
  nSnapshots   = -1;
}
LISTLINE::~LISTLINE()
{
  free(pszText); pszText = 0;
}
PSZ LISTLINE::QueryText()
{
  PSZ psz = 0;
  if (!pszText)
  {
    switch (lineKind)
    {
      case COMPONENT: psz = ComponentName(nOrder);                                                 break;
      case PUBLIC:    psz = PublicName(pParentLine->nOrder,nOrder);                                break;
      case MODULE:    psz = ModuleName(pParentLine->nOrder,nOrder);                                break;
      case PROCEDURE: psz = ProcName(pParentLine->pParentLine->nOrder,pParentLine->nOrder,nOrder); break;
      case LINE:
      {
        int nModule = pParentLine->pParentLine->nOrder;
        int nCom    = pParentLine->pParentLine->pParentLine->nOrder;
        PSZ szFile  = SourceName(nCom,nModule);
        psz         = GetSourceLine(szFile,nOrder);
        break;
      }
    }
    if (!psz) psz = "<Wrong line: internal error>";
    pszText = sf_mallocstr(psz);
  }
  return pszText ? pszText : "<Error: can not allocate memory>";
}
int LISTLINE::GlobRelTime()
{
  if (nGlobRelTime < 0)
  {
    PLISTLINE pllBottom = 0;
    switch (lineKind)
    {
      case COMPONENT: pllBottom = pParentLine;                                        break;
      case PUBLIC:    pllBottom = pParentLine->pParentLine;                           break;
      case MODULE:    pllBottom = pParentLine->pParentLine;                           break;
      case PROCEDURE: pllBottom = pParentLine->pParentLine->pParentLine;              break;
      case LINE:      pllBottom = pParentLine->pParentLine->pParentLine->pParentLine; break;
    }
    int q        = pllBottom->q_snapshots();
    nGlobRelTime = q ? HIPERCENT*q_snapshots()/q : 0;
    nGlobRelTime = min(nGlobRelTime,HIPERCENT);
    nGlobRelTime = max(nGlobRelTime,0);
  }
  return nGlobRelTime;
}
int LISTLINE::ModRelTime()
{
  if (nModRelTime < 0)
  {
    int q       = pParentLine->q_snapshots();
    nModRelTime = (lineKind == BOTTOM) ? HIPERCENT : (q ? HIPERCENT*q_snapshots()/q : 0);
    nModRelTime = min(nModRelTime,HIPERCENT);
    nModRelTime = max(nModRelTime,0);
  }
  return nModRelTime;
}
int  LISTLINE::q_snapshots()
{
  if (nSnapshots<0)
  {
    switch (lineKind)
    {
      case BOTTOM:    nSnapshots = GetSnapshots();                                                             break;
      case COMPONENT: nSnapshots = ComponentSnapshots(nOrder);                                                 break;
      case PUBLIC:    nSnapshots = PublicSnapshots(pParentLine->nOrder,nOrder);                                break;
      case MODULE:    nSnapshots = ModuleSnapshots(pParentLine->nOrder,nOrder);                                break;
      case PROCEDURE: nSnapshots = ProcSnapshots(pParentLine->pParentLine->nOrder,pParentLine->nOrder,nOrder); break;
      case LINE:      nSnapshots = LineSnapshots(pParentLine->pParentLine->pParentLine->nOrder,
                                                 pParentLine->pParentLine->nOrder,nOrder);                     break;
    }
    if (nSnapshots<0) nSnapshots = 0;
  }
  return nSnapshots;
}
void LISTLINE::ClickLine()
{
  LONG         i=0,iHi;
  PLISTLINE    pll;
  PVIEWPORT    pVP;
  LINE_KIND    lkNew;
  PSZ          pszCapt;
  switch (lineKind)
  {
    case BOTTOM:
      pVP     = &VPortComponents;
      iHi     = nComponents;
      lkNew   = COMPONENT;
      pszCapt = 0;
      break;
    case COMPONENT:
      pVP       = &VPortModPub;
      if ((iHi  = N_Parts(nOrder))>0)
        lkNew   = MODULE,
        pszCapt = "Modules";
      else
        lkNew   = PUBLIC,
        pszCapt = "Publics";
      iHi       = abs(iHi);
      break;
    case MODULE:
      pVP     = &VPortProcs;
      iHi     = N_Proc(pParentLine->nOrder, nOrder);
      lkNew   = PROCEDURE;
      pszCapt = "Procedures";
      break;
    case PROCEDURE:
      pVP     = &VPortLines;
      ProcBounds(pParentLine->pParentLine->nOrder, pParentLine->nOrder, nOrder, &i, &iHi);
      iHi++;
      lkNew   = LINE;
      pszCapt = 0; // Set in GetSourceLine
      break;
    default:  return;
  }
  pVP->StartUpdate();
  for (i; i<iHi; i++)
  {
    pll              = new LISTLINE;
    pll->lineKind    = lkNew;
    pll->nOrder      = i;
    pll->pParentLine = this;
    pVP->AddLine(pll);
  }
  pVP->EndUpdate(pszCapt);
}

/*-------------------------------------------------------------------------------------------*/
/*------------------------+-/                            \-+------ !%%%%%%%%%!      ---------*/
/*-------------------------<  VIEWPORT class realisation  >------- !         !%%%%! ---------*/
/*------------------------+-\                            /-+------ !_________!    ! ---------*/
/*-------------------------------------------------------------------------------------------*/
VIEWPORT::VIEWPORT(VIEWPORT_KIND vpKind)
{
  memset(this,0,sizeof(*this));
  this->vpKind = vpKind;
}
VIEWPORT::~VIEWPORT()
{
  clear_list();
  memset(this,0,sizeof(*this));
}
void VIEWPORT::Init(int nPrfItem)
{
//
// Создает окна, для VP_COMPONENTS - показывает и регистрирует в тасклисте
// QWL_USER у окон - указатель на VIEWPORT
//
  if (hFrame) return;
  static    BOOL              f1st = TRUE;
  if (f1st)
  {
    InitHeadwin(hAB);
    WinRegisterClass(hAB,VPCLIENTCLASS,(PFNWP)wpVPClient, CS_SIZEREDRAW | CS_CLIPCHILDREN,4);
    f1st = FALSE;
  }

  ULONG     fcdata = FCF_NOBYTEALIGN | FCF_TITLEBAR   | FCF_SYSMENU | FCF_MINBUTTON |
                     FCF_MAXBUTTON   | FCF_SIZEBORDER | FCF_ICON    | FCF_MENU      | FCF_ACCELTABLE;
  hFrame = WinCreateStdWindow(HWND_DESKTOP,
                              0,
                              &fcdata,
                              VPCLIENTCLASS,
                              "Components",
                              WS_VISIBLE,
                              (HMODULE)NULL,
                              RES_MAIN,
                              &hClient);
  hList = WinCreateWindow(hClient, WC_LISTBOX, "", WS_VISIBLE | LS_NOADJUSTPOS | LS_HORZSCROLL | LS_OWNERDRAW,
                          -100, 0, 1, 1, hFrame, HWND_TOP, 1234, 0, NULL);
  hHead = WinCreateWindow(hClient, WC_HEADWIN, "", WS_VISIBLE,
                          -100, 0, 1, 1, hFrame,  HWND_TOP, 0, 0, NULL);
  if (!hList) exit(5);
  this->nPrfItem = nPrfItem;
  wpVPFrame(hFrame, MM_SUBCLASS, MPARAM(ULONG(WinSubclassWindow(hFrame, (PFNWP)wpVPFrame))),0);

  WinSetWindowULong(hFrame,  QWL_USER,ULONG(this));
  WinSetWindowULong(hClient, QWL_USER,ULONG(this));
  WinSetWindowULong(hList,   QWL_USER,ULONG(this));
  {
    ULONG rgb   = 0x00ffffff;
    char  szF[] = "8.Helv";
    WinSetPresParam(hList, PP_BACKGROUNDCOLOR, sizeof(rgb),   &rgb);
    WinSetPresParam(hList, PP_FONTNAMESIZE,    strlen(szF)+1, szF);
  }
  lNumFract    = 4;
  lTimeFract   = 8;
  lTime1Fract  = 10;
  lNameFract   = 28;
  dyHead       = (int)WinSendMsg(hHead,HM_QUERYOPTHEIGHT,0,0);
  HBTNCREATESTRUCT hbs;
  memset(&hbs,0,sizeof(hbs));
  hbs.lWidth  = 5;
  hbs.pszText = "#";
  hbs.usCmd   = HCMD_NUM;
  WinSendMsg(hHead,HM_ADDBUTTON,MPARAM(&hbs),0);
  hbs.pszText = "Time";
  hbs.usCmd   = HCMD_TIME;
  WinSendMsg(hHead,HM_ADDBUTTON,MPARAM(&hbs),0);
  hbs.pszText = "Time";
  hbs.usCmd   = HCMD_TIME1;
  WinSendMsg(hHead,HM_ADDBUTTON,MPARAM(&hbs),0);
  hbs.pszText = "Item name";
  hbs.usCmd   = HCMD_NAME;
  WinSendMsg(hHead,HM_ADDBUTTON,MPARAM(&hbs),0);

  if (vpKind==VP_COMPONENTS)
  {
    SWCNTRL  SwData;
    SwData.hwnd            = hFrame;
    SwData.hwndIcon        = 0;
    SwData.hprog           = 0;
    SwData.idProcess       = 0;
    SwData.idSession       = 0;
    SwData.uchVisibility   = SWL_VISIBLE;
    SwData.fbJump          = SWL_JUMPABLE;
    SwData.szSwtitle[0]    = '\0';
    hSwitch = WinAddSwitchEntry(&SwData);
  }
  WinSendMsg(hFrame,MM_PROFILE,MPARAM(TRUE),MPARAM(nPrfItem));
  if (!nPrfItem)
  {
    WinShowWindow(hFrame,TRUE);
    WinSetFocus(HWND_DESKTOP,hList);
  }
  else WinShowWindow(hFrame,FALSE);
}
void VIEWPORT::Kill()
{
  if (hFrame)
  {
    char sz[100];
    WinShowWindow(hFrame,FALSE);
    sprintf(sz,PRFKEYNAME "Frame%u",nPrfItem);
    WinStoreWindowPos(PRFAPPNAME, sz, hFrame);
    clear_list();
    WinDestroyWindow(hFrame);
    memset(this,0,sizeof(*this));
  }
}

void VIEWPORT::StartUpdate()
{
  clear_list();
  lock_update(TRUE);
}

void VIEWPORT::AddLine(PLISTLINE pLL)
{
  int nIt = (int)WinSendMsg(hList, LM_INSERTITEM,    MPARAM(LIT_END), MPARAM("???"));
                 WinSendMsg(hList, LM_SETITEMHANDLE, MPARAM(nIt),     MPARAM(pLL));
}
void VIEWPORT::EndUpdate(PSZ pszCapt)
{
  WinSendMsg(hList,  LM_SELECTITEM, MPARAM(0), MPARAM(TRUE));
  BOOL fCont    = !!WinSendMsg(hList,LM_QUERYITEMCOUNT,0,0);
//  BOOL fHidden  = !(WS_VISIBLE & (int)WinQueryWindowULong(hFrame,QWL_STYLE));
//  BOOL fHidNew  = fCont || vpKind==VP_COMPONENTS;
  if (pszCapt) WinSetWindowText(hFrame,pszCapt);
  if (!fCont)
    switch (vpKind)
    {
      case VP_COMPONENTS:   WinShowWindow(VPortModPub.hFrame,FALSE);
      case VP_MODULES:      WinShowWindow(VPortProcs.hFrame,FALSE);
      case VP_PROCEDURES:   WinShowWindow(VPortLines.hFrame,FALSE);
    }
  MM_ApplyWidths(hFrame,0,0);
  Sort(sortMode);
  WinShowWindow(hFrame, fCont || vpKind==VP_COMPONENTS);
  WinSetWindowPos(hFrame,HWND_TOP,0,0,0,0,SWP_ZORDER);
  lock_update(FALSE);
}
  SORT_MODE sm;
  int _Optlink compare (const void *arg1, const void *arg2)
  {
    int i=0;
    switch(sm)
    {
      case SORT_TIME:
        i = (*(PPLISTLINE)arg2)->nSnapshots - (*(PPLISTLINE)arg1)->nSnapshots;
        break;
      case SORT_TEXT:
        i = sf_stricmp((*(PPLISTLINE)arg1)->pszText ? (*(PPLISTLINE)arg1)->pszText : "",
                       (*(PPLISTLINE)arg2)->pszText ? (*(PPLISTLINE)arg2)->pszText : "");
        break;
    }
    return i ? i : ((*(PPLISTLINE)arg1)->nOrder - (*(PPLISTLINE)arg2)->nOrder);
  }

void VIEWPORT::Sort(SORT_MODE sMode)
{
  int        nTotal  = (int)WinSendMsg(hList,LM_QUERYITEMCOUNT,0,0);
  int        i       = (int)WinSendMsg(hList,LM_QUERYSELECTION,MPARAM(LIT_FIRST),0);
  PLISTLINE  pllSel  = (i==LIT_NONE) ? 0 : (PLISTLINE)WinSendMsg(hList,LM_QUERYITEMHANDLE,MPARAM(i),0);
  PLISTLINE *apll    = (PLISTLINE*)malloc(nTotal*sizeof(PLISTLINE));
  if (!nTotal || !apll) return;
  for (i=0; i<nTotal; i++)
    if (!(apll[i] = (PLISTLINE)WinSendMsg(hList,LM_QUERYITEMHANDLE,MPARAM(i),0)))
    {
      free(apll);
      return;
    }
  sortMode = sm  = sMode;
  qsort(apll,nTotal,sizeof(PLISTLINE),compare);
  lock_update(TRUE);
  // re-fill listbox
  WinSendMsg(hList,LM_DELETEALL,0,0);
  int nSel = LIT_NONE;
  for (i=0;i<nTotal;i++)
  {
    int nIt = (int)WinSendMsg(hList, LM_INSERTITEM,    MPARAM(LIT_END), MPARAM("???"));
                   WinSendMsg(hList, LM_SETITEMHANDLE, MPARAM(nIt),     MPARAM(apll[i]));
    if (apll[i]==pllSel) nSel = i;
  }
  free(apll);
  fLockLMSel++;
  if (nSel!=LIT_NONE) WinSendMsg(hList,LM_SELECTITEM,MPARAM(nSel),MPARAM(TRUE));
  fLockLMSel--;
  lock_update(FALSE);
}
void VIEWPORT::SetCaption (PSZ psz)
{
  WinSetWindowText(hFrame,psz);
}
void VIEWPORT::clear_list()
{
  if (hList)
  {
    lock_update(TRUE);
    int nIt = (int)WinSendMsg(hList,LM_QUERYITEMCOUNT,0,0);
    while (--nIt >= 0)
    {
      PLISTLINE pLL = (PLISTLINE)WinSendMsg(hList,LM_QUERYITEMHANDLE,MPARAM(nIt),0);
      delete pLL;
    }
    WinSendMsg(hList,LM_DELETEALL,0,0);
    lock_update(FALSE);
  }
}
void VIEWPORT::lock_update(BOOL fLock)
{
  if (fLock) nUpdateLocked++;
  else       nUpdateLocked--;
  if (!nUpdateLocked)
    WinInvalidateRect(hList,0,0);
}
MRESULT VIEWPORT::WM_DrawItem (HWND hWnd, MPARAM m1, MPARAM m2)
{
  POWNERITEM pow     = POWNERITEM(m2);
  PLISTLINE  pLL     = PLISTLINE(pow->hItem);
  PVIEWPORT  pVP     = (PVIEWPORT)WinQueryWindowULong(hWnd,QWL_USER);
  if (nUpdateLocked || !pLL || !pVP) return MRESULT(TRUE);
  RECTL      rc      = pow->rclItem;
  RECTL      rcDraw  = rc;
  LONG       clrFore = CLR_BLACK;
  LONG       clrBack = CLR_WHITE;

  if (pow->fsState){
    clrFore = CLR_WHITE;
    clrBack = CLR_DARKGRAY;
  }
  WinFillRect(pow->hps,&rc,clrBack);
  rc.yTop--;
  GpiSetColor    (pow->hps,clrFore);
  GpiSetBackColor(pow->hps,clrBack);

  int nAbs = pLL->GlobRelTime(); // of HIPERCENT%
  int nRel = pLL->ModRelTime();  // of HIPERCENT%

  // Номер (xNumFract):
  {
    rcDraw.xLeft  = rc.xLeft + 2;
    rcDraw.xRight = rc.xLeft + pVP->xNumFract;
    char sz[10]="";
    sprintf(sz,"%u.",pLL->QueryOrder()+1);  // От '1'
    WinDrawText(pow->hps,-1,sz,&rcDraw,0,0, DT_LEFT | DT_VCENTER | DT_TEXTATTRS);
  }
  // Текст процентов (xTimeFract):
  {
    rcDraw.xLeft  = rc.xLeft + pVP->xNumFract;
    rcDraw.xRight = rcDraw.xLeft + pVP->xTimeFract;
    char sz[30]="";
    if (nAbs<100) sprintf(sz,"%3.1f(",float(nAbs)/10.0);
    else          sprintf(sz,"%2u(",  nAbs/10);
    if (nRel<100) sprintf(sz+strlen(sz),"%3.1f)%%",float(nRel)/10.0);
    else          sprintf(sz+strlen(sz),"%2u)%%",  nRel/10);
    WinDrawText(pow->hps,-1,sz,&rcDraw,0,0, DT_LEFT | DT_VCENTER | DT_TEXTATTRS);
  }
  // Поставим градусник (xTime1Fract):
  {
    LONG dh        = (rc.yTop-rc.yBottom)/5 + 1;
    rcDraw.yBottom = rc.yBottom + dh;
    rcDraw.yTop    = rc.yTop-dh;
    rcDraw.xLeft   = rc.xLeft + pVP->xNumFract + pVP->xTimeFract;
    rcDraw.xRight  = rcDraw.xLeft + 1 + pVP->xTime1Fract*nAbs/1100;
    WinFillRect(pow->hps,&rcDraw,CLR_RED);
    rcDraw.xLeft   = rcDraw.xRight;
    rcDraw.xRight  = rcDraw.xLeft + pVP->xTime1Fract*(nRel-nAbs)/1100;
    WinFillRect(pow->hps,&rcDraw,CLR_BLUE);
    rcDraw         = rc;
  }
  // Текст (xNameFract):
  {
    rcDraw.xLeft   = rc.xLeft + pVP->xNumFract + pVP->xTimeFract + pVP->xTime1Fract;
    rcDraw.xRight  = rc.xRight; // rcDraw.xLeft + pVP->xNameFract;
    WinDrawText(pow->hps,-1,pLL->QueryText(),&rcDraw,0,0, DT_LEFT | DT_VCENTER | DT_TEXTATTRS);
  }
  pow->fsState = pow->fsStateOld = 0;
  return MRESULT(TRUE);
}
MRESULT VIEWPORT::WM_MeasureItem (HWND hWnd, MPARAM m1, MPARAM m2)
{
  /*+++ Speed it up! */
  FONTMETRICS  fm;
  HPS          hps  = WinGetPS(hList);
  GpiQueryFontMetrics(hps,sizeof(fm),&fm);
  WinReleasePS(hps);
  return MRFROM2SHORT((fm.lMaxBaselineExt+fm.lExternalLeading), 10000);
}
MRESULT VIEWPORT::WM_Control (HWND hWnd, MPARAM m1, MPARAM m2)
{
  //if (nLockCtrl) return 0;
  if (!fLockLMSel && SHORT2FROMMP(m1)==LN_SELECT)
  {
    int       nIt = (int)      WinSendMsg(hList,LM_QUERYSELECTION, MPARAM(LIT_FIRST),0);
    PLISTLINE pLL = (PLISTLINE)WinSendMsg(hList,LM_QUERYITEMHANDLE,MPARAM(nIt),      0);
    if (pLL) pLL->ClickLine();
  }
  else if (SHORT1FROMMP(m1)==HBN_TRACKING || SHORT1FROMMP(m1)==HBN_SIZE)
    MM_ApplyWidths(hWnd,m1,m2);
  return 0;
}
MRESULT VIEWPORT::MM_ApplyWidths (HWND hWnd, MPARAM m1, MPARAM m2)
{
  lNumFract   = xNumFract   = (int)WinSendMsg(hHead, HM_QUERYBTNWIDTH, MPFROM2SHORT(HCMD_NUM,TRUE),  0);
  lTimeFract  = xTimeFract  = (int)WinSendMsg(hHead, HM_QUERYBTNWIDTH, MPFROM2SHORT(HCMD_TIME,TRUE), 0);
  lTime1Fract = xTime1Fract = (int)WinSendMsg(hHead, HM_QUERYBTNWIDTH, MPFROM2SHORT(HCMD_TIME1,TRUE),0);
  lNameFract  = xNameFract  = (int)WinSendMsg(hHead, HM_QUERYBTNWIDTH, MPFROM2SHORT(HCMD_NAME,TRUE), 0);
  WinInvalidateRect(hList,0,0);
  return 0;
}
MRESULT VIEWPORT::WM_Command (HWND hWnd, MPARAM m1, MPARAM m2)
{
  int s1m1       = SHORT1FROMMP(m1);
  PSZ pszF       = History.Cmd2File(s1m1);
  if (pszF) s1m1 = IDM_FILEOPEN;
  switch(s1m1)
  {
    case IDM_FILEOPEN:
    {
      FILEDLG fild;
      memset(&fild, 0, sizeof(FILEDLG));
      if (!pszF)
      {
        strcpy(fild.szFullFile, "*.xpt");
        fild.cbSize   = sizeof(FILEDLG);
        fild.fl       =  FDS_CENTER | FDS_OPEN_DIALOG ;
        fild.pszTitle = "Open";
        if (!WinFileDlg(HWND_DESKTOP, hWnd, &fild) || fild.lReturn!=DID_OK) break;
      }
      else strcpy(fild.szFullFile,pszF);
      if (fClearDBI) ClearDebugInfo();
      int rc = LoadDebugInfo (fild.szFullFile);
      if (rc>0)
      {
        char szCapt[CCHMAXPATH+30];
        nComponents = rc;
        History.AppItem(fild.szFullFile);
        sprintf(szCapt,"Components: %s",fild.szFullFile);
        VPortComponents.SetCaption(szCapt);

//        fClearDBI = TRUE;
      }
      else
      {
        char aszErr[7][200] = {"Error","Open prodile data error","Read error profile data error","Read debuf info error",
                               "Wrong data format error","Not XDS profiler trace file","Unknown error"};
        char sz[200+CCHMAXPATH];
        sprintf(sz,"%s. \n(File: %s).",aszErr[rc>-6 ? -rc : 6],fild.szFullFile);
        WinMessageBox(HWND_DESKTOP, hWnd, sz, "ERROR", 0, MB_ERROR|MB_MOVEABLE|MB_OK);
        nComponents = 0;
      }
      llBottom.ClickLine();
      break;
    }
    case HCMD_NUM:   Sort(SORT_ORDER); break;
    case HCMD_TIME:
    case HCMD_TIME1: Sort(SORT_TIME);  break;
    case HCMD_NAME:  Sort(SORT_TEXT);  break;
    case IDM_WINPOPALL:
    {
      for (int i=3; i>=0; i--)
        if (apVPorts[i]!=this) WinSetWindowPos(apVPorts[i]->hFrame,HWND_TOP,0,0,0,0,SWP_ZORDER);
      WinSetWindowPos(hFrame,HWND_TOP,0,0,0,0,SWP_ZORDER);
      break;
    }
    case IDM_WINCASCADE:
    case IDM_WINTILE:
    {
      int cx   = WinQuerySysValue(HWND_DESKTOP,SV_CXSCREEN);
      int cy   = WinQuerySysValue(HWND_DESKTOP,SV_CYSCREEN);
      for (int i=3; i>=0; i--)
      {
        HWND hFr = apVPorts[i]->hFrame;
        if (s1m1==IDM_WINCASCADE)
        {
          int nPos = (i % 4) + 1;
          WinSetWindowPos(hFr,HWND_TOP,cx/10*nPos,cy/10*(5-nPos),cx/2,cy/2,
                          SWP_ZORDER|SWP_MOVE|SWP_SIZE);
        }
        else
          WinSetWindowPos(hFr,HWND_TOP, (i&1) ? cx/2 : 4, (i&2) ? 4 : cy/2, cx/2-4, cy/2-4,
                          SWP_ZORDER|SWP_MOVE|SWP_SIZE);
        WinSetFocus(HWND_DESKTOP,VPortComponents.hList);
      }
      break;
    }
    default:
      s1m1 -= IDM_WIN_MIN;
      if (s1m1<sizeof(apVPorts)/sizeof(apVPorts[0]))
        WinSetFocus(HWND_DESKTOP,apVPorts[s1m1]->hList);
  }
  return 0;
}

MRESULT VIEWPORT::WM_InitMenu (HWND hWnd, MPARAM m1, MPARAM m2)
{
  switch(SHORT1FROMMP(m1))
  {
    case IDM_WINDOW:
    {
      LONG      i;
      HWND      hm;
      MENUITEM  mi;
      if (!WinSendMsg(HWND(WinWindowFromID(hFrame,FID_MENU)),MM_QUERYITEM,MPFROM2SHORT(IDM_WINDOW,TRUE),(MPARAM)&mi)) return 0;
      if (hm = mi.hwndSubMenu)
        for (i=0; i<sizeof(apVPorts)/sizeof(apVPorts[0]);i++)
        {
          HWND hFr                = apVPorts[i]->hFrame;
          char sz[CCHMAXPATH+200] = "~1: ???"; sz[1] += i;
          BOOL fDisabled          = !(WS_VISIBLE & (ULONG)WinQueryWindowULong(hFr,QWL_STYLE));
          WinQueryWindowText(hFr,sizeof(sz)-4,sz+4);
          WinSendMsg(hm,MM_SETITEMTEXT,MPARAM(IDM_WIN_MIN+i),MPARAM(sz));
          WinSendMsg(hm,MM_SETITEMATTR,MPFROM2SHORT(IDM_WIN_MIN+i,TRUE),
                     MPFROM2SHORT(MIA_DISABLED|MIA_CHECKED, (fDisabled ? MIA_DISABLED : 0)|(this==apVPorts[i] ? MIA_CHECKED : 0)));
        }
      break;
    }
    case IDM_FILE:
    {
      MENUITEM  mi;
      if (!WinSendMsg(HWND(WinWindowFromID(hFrame,FID_MENU)),MM_QUERYITEM,MPFROM2SHORT(IDM_FILE,TRUE),(MPARAM)&mi)) return 0;
      History.InitMenu(mi.hwndSubMenu);
      break;
    }
    default: return WinDefWindowProc(hWnd,WM_INITMENU,m1,m2);
  }
  return 0;
}
MRESULT VIEWPORT::WM_Size (HWND hWnd, MPARAM m1, MPARAM m2)
{ // Client's size means
  RECTL rcl;
  WinQueryWindowRect(hClient,&rcl);
  int dx   = rcl.xRight;
  int dy   = rcl.yTop;
  LONG dyh = min(dyHead,dy);
  WinSetWindowPos(hList, 0, 0, 0,      dx, dy-dyh, SWP_MOVE|SWP_SIZE|SWP_HIDE);
  WinSetWindowPos(hHead, 0, 0, dy-dyh, dx, dyh,    SWP_MOVE|SWP_SIZE|SWP_HIDE);
  WinShowWindow(hList,TRUE);
  WinShowWindow(hHead,TRUE);
  int nFrSum;
  if (!(nFrSum = lNumFract + lTimeFract + lTime1Fract + lNameFract))
    nFrSum=4, lNumFract=lTimeFract=lTime1Fract=lNameFract=1;
  int nUsed;
  int nBit   = dx * lNumFract  / nFrSum;    nUsed  = nBit;
  WinSendMsg(hHead, HM_SETBTNWIDTH, MPFROM2SHORT(HCMD_NUM,TRUE),  MPARAM(nBit));
  nBit       = dx * lTimeFract / nFrSum;    nUsed += nBit;
  WinSendMsg(hHead, HM_SETBTNWIDTH, MPFROM2SHORT(HCMD_TIME,TRUE), MPARAM(nBit));
  nBit       = dx * lTime1Fract/ nFrSum;    nUsed += nBit;
  WinSendMsg(hHead, HM_SETBTNWIDTH, MPFROM2SHORT(HCMD_TIME1,TRUE),MPARAM(nBit));
  nBit       = dx - nUsed;
  WinSendMsg(hHead, HM_SETBTNWIDTH, MPFROM2SHORT(HCMD_NAME,TRUE), MPARAM(nBit));
  WinSendMsg(hList, MM_APPLYWIDTHS, 0,0);
  return 0;
}




/////////////////////////////////// M A I N ( ) ////////////////////////////////////////////////////

int main(int argc, char **argv)
{
  MRESULT EXPENTRY wpFrame  (HWND hWnd, ULONG ulMsg, MPARAM m1, MPARAM m2);
  MRESULT EXPENTRY wpClient (HWND hWnd, ULONG ulMsg, MPARAM m1, MPARAM m2);

  Profile_BEGIN();

  HMQ         hMsgQ;
  QMSG        qMsg;

  if ( ! (hAB   = WinInitialize(0)) ||
       ! (hMsgQ = WinCreateMsgQueue(hAB,32)))
    exit (4);

  History        .Init(IDM_FILEHIST_MIN, IDM_FILEHIST_MAX-1, IDM_FILEHIST_MAX);
  VPortComponents.Init(0); // Create windows, show, add switch entry
  VPortModPub    .Init(1); // Create windows
  VPortProcs     .Init(2); // Create windows
  VPortLines     .Init(3); // Create windows

  while(WinGetMsg(hAB,&qMsg,0,0,0))
    WinDispatchMsg( hAB,&qMsg);

  VPortLines     .Kill();
  VPortProcs     .Kill();
  VPortModPub    .Kill();
  VPortComponents.Kill();
  History        .Kill();
  if (fClearDBI) ClearDebugInfo();

  WinRemoveSwitchEntry(hSwitch);
  WinDestroyMsgQueue(hMsgQ);
  WinTerminate(hAB);
}

MRESULT EXPENTRY wpVPFrame(HWND hWnd,ULONG ulMsg, MPARAM m1, MPARAM m2)
{
  static PFNWP pWindowProc;
  PVIEWPORT    pVP = (PVIEWPORT)WinQueryWindowULong(hWnd,QWL_USER);

  switch (ulMsg){
    case MM_SUBCLASS:
      pWindowProc = PFNWP(ULONG(m1));
      break;
    case MM_PROFILE:
    // m1 = TRUE/FALSE - read/write
    // m2 = nPrfItem;
    {
      int  nWin = int(m2);
      char sz[100];
      sprintf(sz,PRFKEYNAME "Frame%u",nWin);
      if (!WinRestoreWindowPos(PRFAPPNAME, sz, hWnd))
      {
        int cx   = WinQuerySysValue(HWND_DESKTOP,SV_CXSCREEN);
        int cy   = WinQuerySysValue(HWND_DESKTOP,SV_CYSCREEN);
        int nPos = (nWin % 4) + 1;
        WinSetWindowPos(hWnd,HWND_TOP,cx/10*nPos,cy/10*(5-nPos),cx/2,cy/2,
                        SWP_MOVE|SWP_SIZE|(nWin ? 0 : SWP_SHOW|SWP_ACTIVATE|SWP_ZORDER));
      }
    }
    case WM_MEASUREITEM:
      return pVP ? pVP->WM_MeasureItem(hWnd,m1,m2) : (MRESULT)10;
    case WM_DRAWITEM:
      return pVP ? pVP->WM_DrawItem (hWnd, m1, m2) : 0;
    case WM_COMMAND:
      if (pVP) return pVP->WM_Command(hWnd,m1,m2);
      break;
    case WM_CONTROL:
      if (pVP) return pVP->WM_Control(hWnd,m1,m2);
      break;
    default:
      return (*pWindowProc)(hWnd,ulMsg,m1,m2);
  }
  return 0;
}


MRESULT EXPENTRY wpVPClient(HWND hWnd,ULONG ulMsg, MPARAM m1, MPARAM m2)
{
  PVIEWPORT    pVP = (PVIEWPORT)WinQueryWindowULong(hWnd,QWL_USER);

  switch (ulMsg){
    case WM_COMMAND:  if (pVP) return pVP->WM_Command(hWnd,m1,m2); break;
    case WM_CONTROL:  if (pVP) return pVP->WM_Control(hWnd,m1,m2); break;
    case WM_SIZE:     return pVP ? pVP->WM_Size(hWnd,m1,m2) : 0;
    case WM_INITMENU: return pVP ? pVP->WM_InitMenu(hWnd,m1,m2) : 0;
    default:          return WinDefWindowProc(hWnd,ulMsg,m1,m2);
  }
  return 0;
}


ULONG IOErMsg(APIRET err, HWND hWnd, char *szCapt, char *szErr, char *szFile, ULONG MBF)
// if (hWnd) show message, return MBID_*
// else      returns PSZ szError
{
  char    *psz   = szErr;

  switch((ULONG)err)
  {
  case 1:       psz = "Invalid funstion number";                                 break;
  case 2:       psz = "File not found";                                          break;
  case 3:       psz = "Path not found";                                          break;
  case 4:       psz = "Too many opened files (no handles left)";                 break;
  case 5:       psz = "Access denied";                                           break;
  case 6:       psz = "Invalid handle";                                          break;
  case 8:       psz = "Insufficient memory";                                     break;
  case 10:      psz = "Invalid environment";                                     break;
  case 11:      psz = "Invalid format";                                          break;
  case 12:      psz = "Invalid access";                                          break;
  case 13:      psz = "Invalid data";                                            break;
  case 19:      psz = "Disk is write protected";                                 break;
  case 26:      psz = "Uniknown media type";                                     break;
  case 29:      psz = "Write fault";                                             break;
  case 32:      psz = "Sharing violation";                                       break;
  case 33:      psz = "Lock violation";                                          break;
  case 36:      psz = "Sharing buffer overflov";                                 break;
  case 82:      psz = "Cannot make directory entry";                             break;
  case 84:      psz = "Too many pipes";                                          break;
  case 87:      psz = "Invalid parameter";                                       break;
  case 89:      psz = "No process slots available";                              break;
  case 95:      psz = "Interrupted system call";                                 break;
  case 99:      psz = "Device in use";                                           break;
  case 108:     psz = "Drive locked by another process";                         break;
  case 109:     psz = "Broken pipe";                                             break;
  case 110:     psz = "Open/create failed due to explicit fail command";         break;
  case 112:     psz = "No enough space on the disk";                             break;
  case 114:     psz = "Invalid target handle";                                   break;
  case 127:     psz = "Procedure address not found";                             break;
  case 182:     psz = "Invalid ordinal";                                         break;
  case 190:     psz = "Invalid module type";                                     break;
  case 191:     psz = "Invalid EXE signature";                                   break;
  case 192:     psz = "EXE marked invalid";                                      break;
  case 195:     psz = "Invalid minimum allocation size";                         break;
  case 196:     psz = "Dynamic link from invalid privilege level";               break;
  case 206:     psz = "File name or extention is greater than 8.3 characters";   break;
  case 231:     psz = "Pipe is busy";                                            break;
  }

  static char szErrMsg[CCHMAXPATH+150];
  if (psz != szErr) sprintf(szErrMsg, "Error %u: %s", int(err), psz);
  else              strcpy(szErrMsg, psz);
  if (szFile && szFile[0])
  {
    strcat(szErrMsg," (File ");
    strcat(szErrMsg,szFile);
    strcat(szErrMsg,")");
  }
  if (hWnd) return WinMessageBox(HWND_DESKTOP, hWnd, szErrMsg, szCapt, 0, MB_ERROR|MB_MOVEABLE|MBF);
  else      return ULONG(szErrMsg);
}

//-------------------------------------

HISTORY:: HISTORY() {memset(this,0,sizeof(*this));}
HISTORY::~HISTORY() {sf_freelist(pllHist); pllHist = 0;}

void HISTORY::Init(int nMinCmd, int nMaxCmd, int nSeparator)
{
  sf_freelist(pllHist);
  pllHist = 0;
  this->nMinCmd    = nMinCmd;
  this->nMaxCmd    = nMaxCmd;
  this->nSeparator = nSeparator;

  char     *pchBuf = 0;
  ULONG     ulSize = 0;
  if (   !PrfQueryProfileSize(HINI_USERPROFILE,PRFAPPNAME,PRFKEYNAME "Hist",&ulSize)
      || !ulSize
      || !(pchBuf=(char*)malloc(ulSize+1))
      || !PrfQueryProfileData(HINI_USERPROFILE,PRFAPPNAME,PRFKEYNAME "Hist",pchBuf,&ulSize)
     )
  {
    free(pchBuf);
    return;
  }
  pchBuf[ulSize] = '\0';
  LONG lN   = *(PLONG)pchBuf;
  int  nCmd = nMinCmd;
  for (PSZ psz=pchBuf+sizeof(lN); psz<pchBuf+ulSize && lN>0 && nCmd++<=nMaxCmd; psz += strlen(psz)+1, lN--)
    pllHist = sf_applist(pllHist,psz);
  free(pchBuf);
}
void HISTORY::Kill()
{
  // Writes the hispory as N,"text1",...,"textN"
  // N is a LONG, text(s) are z-terminated lines
  LINELIST *pll;
  char     *pchBuf;
  LONG      lSize = 0;
  LONG      lN    = 0;
  for (pll=pllHist; pll; pll=pll->next,lN++)
    lSize += strlen(pll->text)+1;
  if (pchBuf=(char*)malloc(lSize+sizeof(lN)))
  {
    *PLONG(pchBuf) = lN;
    char *pchTarg  = pchBuf+sizeof(lN);
    for (pll=pllHist; pll; pll=pll->next)
    {
      strcpy(pchTarg,pll->text);
      pchTarg += strlen(pchTarg)+1;
    }
    PrfWriteProfileData(HINI_USERPROFILE,PRFAPPNAME,PRFKEYNAME "Hist",pchBuf,pchTarg-pchBuf);
    free(pchBuf);
  }
  else PrfWriteProfileData(HINI_USERPROFILE,PRFAPPNAME,PRFKEYNAME "Hist",NULL,0);
}
void HISTORY::InitMenu(HWND hm)
{
  LONG      i;
  MENUITEM  mi;
  PLINELIST pll;
  WinSendMsg(hm,MM_DELETEITEM,MPFROM2SHORT(nSeparator,FALSE),0);
  for (i=nMinCmd; i<=nMaxCmd; i++)
    WinSendMsg(hm,MM_DELETEITEM,MPFROM2SHORT(i,FALSE),0);
  if (!pllHist) return;

  memset(&mi,0,sizeof(mi));
  mi.iPosition   = MIT_END;
  mi.afStyle     = MIS_SEPARATOR;
  mi.id          = nSeparator;
  WinSendMsg(hm,MM_INSERTITEM,MPARAM(&mi),MPARAM(""));


  for (pll=pllHist, i=nMinCmd;  pll && i<=nMaxCmd; pll=pll->next, i++)
  {
    mi.afStyle = MIS_TEXT;
    mi.id      = i;
    WinSendMsg(hm,MM_INSERTITEM,MPARAM(&mi),MPARAM(pll->text));
  }
}
PSZ  HISTORY::Cmd2File(int nCmd)
{
  PLINELIST pll;
  int       i;
  for (pll=pllHist, i=nMinCmd; pll && i<=nMaxCmd; pll=pll->next, i++)
    if (i==nCmd) return pll->text;
  return 0;
}
void HISTORY::AppItem(PSZ pszIt)
{
  PLINELIST *ppll;
  PLINELIST pll;
  int       i;
  if(!pszIt || !*(pszIt=sf_skipspaces(pszIt))) return;
  for (ppll=&pllHist; *ppll; ppll=&(*ppll)->next)
    if (!sf_stricmp((*ppll)->text,pszIt))
    {
      sf_cutlist(ppll);
      break;
    }
  for (pll=pllHist, i=nMinCmd; pll; pll=pll->next, i++)
    if (i>=nMaxCmd-1)
    {
      sf_freelist(pll->next);
      pll->next = 0;
      break;
    }
  pll       = sf_applist(0,pszIt);
  pll->next = pllHist;
  pllHist   = pll;
}


