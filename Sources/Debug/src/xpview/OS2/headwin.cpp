#define  INCL_WIN
#define  INCL_GPI
#include <OS2.H>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "headwin.h"

#define MM_SUBCLASS WM_USER+100

struct  HWINSTR;
typedef HWINSTR *PHWINSTR;

// Элементы списка кнопок - живут в списке HWINSTR::pBtList,
// в слове QWL_USER каждой кнопки - указатель на ее элемент
struct HBTN
{
  HWND      hWnd;
  USHORT    usCmd;
  PHWINSTR  pHW;
  HBTN     *next;
  HBTN() {memset(this,0,sizeof(*this));}
};
typedef HBTN *PHBTN;



// Структура для окна WC_HEADWIN. Указатель в слове QWL_USER окна.
struct HWINSTR
{
  HWND      hWnd;
  HMODES    hMode;
  PHBTN     pBtList;
   HWINSTR() {memset(this,0,sizeof(*this));}
  ~HWINSTR()
  {
    for(PHBTN pBt1, pBt = pBtList; pBt; pBt=pBt1)
    {
      pBt1=pBt->next;
      WinSetWindowULong(pBt->hWnd,QWL_USER,NULL);
      delete pBt;
    }
  }
};


MRESULT EXPENTRY wpHeadwin  (HWND,ULONG,MPARAM,MPARAM);
MRESULT EXPENTRY wpBtn      (HWND,ULONG,MPARAM,MPARAM);
HWND             AddButton  (PHWINSTR pHW, PHBTNCREATESTRUCT pBCS);
void             AlignAll   (PHWINSTR pHW);
PHBTN            IsMovepoint(PHWINSTR pHW, LONG xPos);
LONG             btpos2pos  (HWND hBtn, LONG xBtPos);


/*==========================================================================================*/

static HPOINTER hptrWE = 0;


BOOL InitHeadwin(HAB hAB)
{
  static BOOL f1st = TRUE;
  if (!f1st) return TRUE;
  f1st = !WinRegisterClass(hAB,WC_HEADWIN,(PFNWP)wpHeadwin,CS_SIZEREDRAW|CS_CLIPCHILDREN,4);
  hptrWE = WinQuerySysPointer(HWND_DESKTOP,SPTR_SIZEWE,FALSE);
  return !f1st;
}



MRESULT EXPENTRY wpHeadwin(HWND hWnd,ULONG ulMsg, MPARAM m1,MPARAM m2)
{
  static LONG  xTrack;
  static LONG  dxTrack;
  static PHBTN pbtTrack = 0;
  PHWINSTR p = (PHWINSTR)WinQueryWindowULong(hWnd,QWL_USER);
  switch(ulMsg)
  {
    case WM_CREATE:
    {
      WinDefWindowProc(hWnd,ulMsg,m1,m2);
      p = new HWINSTR;
      p->hMode = HMOD_DRAGABLE;
      p->hWnd  = hWnd;
      WinSetWindowULong(hWnd,QWL_USER,ULONG(p));
      WinSetPresParam(hWnd,PP_FONTNAMESIZE,sizeof(PP_FONT),PP_FONT);
      break;
    }
    case WM_DESTROY:
    {
      delete p;
      WinSetWindowULong(hWnd,QWL_USER,NULL);
      break;
    }
    case WM_PAINT:
    {
      RECTL rcl;
      HPS   hps = WinBeginPaint(hWnd,0,&rcl);
      WinFillRect(hps,&rcl,CLR_PALEGRAY);
      WinEndPaint(hps);
      break;
    }
    case HM_ADDBUTTON:
      return p ? (MRESULT)AddButton(p,PHBTNCREATESTRUCT(m1)) : 0;
    case HM_SIZEMODE:
      if (p)
      {
        p->hMode = HMODES(SHORT1FROMMP(m1));
        AlignAll(p);
      }
      break;
    case HM_QUERYBTNWIDTH:
    case HM_SETBTNWIDTH:
    {
      HWND hBt = 0;
      if (SHORT2FROMMP(m1)) hBt = WinWindowFromID(hWnd,SHORT1FROMMP(m1));
      else if(p)
      {
        int n = SHORT1FROMMP(m1);
        for (PHBTN pBt=p->pBtList; pBt; pBt=pBt->next)
          if (!(n--))
          {
            hBt = pBt->hWnd;
            break;
          }
      }
      SWP swp; swp.cx=-1;
      if (hBt)
      {
        WinQueryWindowPos(hBt,&swp);
        if (ulMsg==HM_SETBTNWIDTH)
        {
          WinSetWindowPos(hBt,0,0,0,LONG(m2),swp.cy,SWP_SIZE);
          AlignAll(p);
        }
      }
      return MRESULT(swp.cx);
    }
    case HM_QUERYOPTHEIGHT:
    {
      FONTMETRICS  fm;
      HPS          hps  = WinGetPS(hWnd);
      GpiQueryFontMetrics(hps,sizeof(fm),&fm);
      WinReleasePS(hps);
      return MRESULT(fm.lMaxBaselineExt+fm.lExternalLeading+5);
    }
    case WM_BUTTON1DOWN:
    case WM_BUTTON2DOWN:
    {
      if (WinQueryCapture(HWND_DESKTOP)==hWnd) goto defret;
      pbtTrack = IsMovepoint(p,SHORT1FROMMP(m1));
      if (!pbtTrack || !WinSetCapture(HWND_DESKTOP,hWnd))
        goto defret;
      WinSetPointer(HWND_DESKTOP,hptrWE);
      SWP swp;
      WinQueryWindowPos(pbtTrack->hWnd,&swp);
      xTrack  = swp.x+swp.cx;
      dxTrack = xTrack - SHORT1FROMMP(m1);
      return MRESULT(TRUE);
    }
    case WM_MOUSEMOVE:
    {
      if (WinQueryCapture(HWND_DESKTOP)!=hWnd || !pbtTrack) goto defret;
      SWP swpL,swp;
      WinQueryWindowPos(hWnd,                &swp);
      WinQueryWindowPos(pbtTrack->hWnd,      &swpL); // Kнопка слева
      LONG cxDesk = swp.cx;
      LONG xT     = SHORT(SHORT1FROMMP(m1))+dxTrack;
      xT          = max(xT,swpL.x+3);
      xT          = min(xT,swp.cx-2);
      LONG xMove  = xT - (swpL.x+swpL.cx);
      if (swpL.cx != xT-swpL.x)
      {
        WinSetWindowPos(pbtTrack->hWnd, 0, 0, 0, xT-swpL.x, swpL.cy, SWP_SIZE|SWP_NOREDRAW);
        for (PHBTN pbt = pbtTrack->next; pbt; pbt=pbt->next)
        {
          WinQueryWindowPos(pbt->hWnd,&swp);
          WinSetWindowPos(pbt->hWnd,0,swp.x+xMove,0, 0,0, SWP_MOVE|SWP_NOREDRAW);
          if (!pbt->next && swp.x+5<cxDesk)
            WinSetWindowPos(pbt->hWnd,0,0,0, cxDesk-swp.x, swp.cy,SWP_SIZE|SWP_NOREDRAW);
        }

        WinInvalidateRect(hWnd,0,TRUE);
        WinSendMsg(WinQueryWindow(hWnd,QW_OWNER),
                   WM_CONTROL,
                   MPFROM2SHORT(HBN_TRACKING, WinQueryWindowUShort(hWnd,QWS_ID)),
                   MPFROM2SHORT(pbtTrack->usCmd, xT-swpL.x));
      }
      xTrack = xT;
      break;
    }
    case WM_BUTTON1UP:
    case WM_BUTTON2UP:
      if (WinQueryCapture(HWND_DESKTOP)!=hWnd) goto defret;
      WinSetCapture(HWND_DESKTOP,0);
      if (pbtTrack)
      {
        SWP swp;
        WinQueryWindowPos(pbtTrack->hWnd,&swp);
        WinSendMsg(WinQueryWindow(hWnd,QW_OWNER),
                   WM_CONTROL,
                   MPFROM2SHORT(HBN_SIZE, WinQueryWindowUShort(hWnd,QWS_ID)),
                   MPFROM2SHORT(pbtTrack->usCmd, swp.cx));
        pbtTrack = 0;
      }
      goto defret;
    case WM_SIZE:
    {
      SWP  swp;
      LONG cy = SHORT2FROMMP(m2);
      for (PHBTN pbt = p->pBtList; pbt; pbt=pbt->next)
      {
        WinQueryWindowPos(pbt->hWnd,&swp);
        if (swp.cy!=cy)
          WinSetWindowPos(pbt->hWnd,0,0,0, swp.cx, cy, SWP_SIZE);
      }
      break;
    }
    default:
    defret:
      return WinDefWindowProc(hWnd,ulMsg,m1,m2);
  }
  return 0;
}


/*----------------------------------------------------------------------------------------------------------*/
/*                                                                                                          */
/*  Button window procedure                                                                                 */
/*                                                                                                          */
/*----------------------------------------------------------------------------------------------------------*/
//
MRESULT EXPENTRY wpBtn(HWND hWnd,ULONG ulMsg, MPARAM m1,MPARAM m2)
{
  static   PFNWP    pWindowProc;

  switch (ulMsg){
  case MM_SUBCLASS:
    pWindowProc         = PFNWP(ULONG(m1));
    return 0;
  case WM_MOUSEMOVE:
  {
    PHBTN pBt = (PHBTN)WinQueryWindowULong(hWnd,QWL_USER);
    if (pBt && IsMovepoint(pBt->pHW,btpos2pos(hWnd,SHORT1FROMMP(m1))))
    {
      WinSetPointer(HWND_DESKTOP,hptrWE);
      return 0;
    }
    break;
  }
  case WM_BUTTON1DOWN:
  case WM_BUTTON2DOWN:
  {
    PHBTN  pBt = (PHBTN)WinQueryWindowULong(hWnd,QWL_USER);
    MPARAM m11 = MPFROM2SHORT(btpos2pos(hWnd,SHORT1FROMMP(m1)), SHORT2FROMMP(m1));
    if (pBt &&
        IsMovepoint(pBt->pHW,SHORT1FROMMP(m11)) &&
        WinSendMsg(pBt->pHW->hWnd,ulMsg,m11,m2))
      return 0;
    break;
  }
  case BM_SETCHECK:
  case BM_SETDEFAULT:
    return 0;
  }
  return ((MRESULT)(*pWindowProc)(hWnd,ulMsg,m1,m2));
}


HWND AddButton(PHWINSTR pHW, PHBTNCREATESTRUCT pBCS)
{
  PHBTN pNew = new HBTN;
  pNew->usCmd = pBCS->usCmd;
  // Создаем Кнопку!
  HWND       hPapa    = pHW->hWnd;
  SWP        swp;       WinQueryWindowPos(pHW->hWnd,&swp);
  LONG       lHeight  = swp.cy;
  pNew->hWnd          = WinCreateWindow(hPapa, WC_BUTTON, pBCS->pszText,
                                        BS_PUSHBUTTON | BS_NOPOINTERFOCUS | WS_VISIBLE,
                                        -1000, 0, pBCS->lWidth, lHeight,
                                        WinQueryWindow(hPapa,QW_OWNER), HWND_TOP,
                                        pBCS->usCmd, 0, 0);
  pNew->pHW = pHW;
  if (!pNew->hWnd) return 0;
  WinSetPresParam(pNew->hWnd,PP_FONTNAMESIZE,sizeof(PP_FONT),PP_FONT);
  WinSetWindowULong(pNew->hWnd,QWL_USER,ULONG(pNew));
  wpBtn(pNew->hWnd,MM_SUBCLASS,MPARAM(ULONG(WinSubclassWindow(pNew->hWnd,(PFNWP)wpBtn))),0L);

  // Вставим на нужное место в списке
  for (PHBTN *pp = &pHW->pBtList; ; pp = &(*pp)->next)
  {
    if (!*pp)
    {
      *pp = pNew;
      break;
    }
    else if((*pp)->usCmd == pBCS->usCmdBefore)
    {
      pNew->next = *pp;
      *pp        = pNew;
      break;
    }
  }
  AlignAll(pHW);
  return pNew->hWnd;
}

void AlignAll(PHWINSTR pHW)
{ // Пока что hMode не учитываестя
  SWP  swp;
  WinQueryWindowPos(pHW->hWnd,&swp);
  LONG cxDesk = swp.cx;
  LONG xPos = 0;
  for (PHBTN pBt=pHW->pBtList; pBt; pBt=pBt->next)
  {
    WinQueryWindowPos(pBt->hWnd,&swp);
    if (swp.x!=xPos)
      WinSetWindowPos(pBt->hWnd,0,xPos,0,0,0,SWP_MOVE);
    if (!pBt->next && swp.x+5<cxDesk)
      WinSetWindowPos(pBt->hWnd,0,0,0,cxDesk-xPos,swp.cy,SWP_SIZE);
    xPos+=swp.cx;
  }
}

PHBTN IsMovepoint(PHWINSTR pHW, LONG xPos)
// Вернет кнопку _после_ которой стоит мыша или 0
{
  SWP   swp;
  LONG  cxNext  = -1;
  LONG  cx,xCnt =  0;
  PHBTN pbRet   =  0;
  for (PHBTN pBt=pHW->pBtList; pBt; pBt=(pbRet=pBt)->next)
  {
    WinQueryWindowPos(pBt->hWnd,&swp);
    if (xPos<swp.x) break;
    LONG dx  = min(swp.cx/5,5);
    LONG xHi = swp.x+swp.cx;
    if (xPos>=swp.x && xPos<=swp.x+dx && pbRet)  return pbRet; // После предыдущей
    if (xPos<=xHi && xPos>=xHi-dx && pBt->next)  return pBt;   // После этой
    // Нефиг последнюю границу дергать^^^^^^^^
    pbRet = pBt;
  }
  return 0;
}

LONG btpos2pos(HWND hBtn, LONG xBtPos)
{
  SWP swp;
  WinQueryWindowPos(hBtn,&swp);
  return xBtPos+swp.x;
}
