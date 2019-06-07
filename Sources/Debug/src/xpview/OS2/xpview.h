#ifndef _list_
#define _list_
#include <os2def.h>
#include "sf_lib.h"

#define PRFAPPNAME              "XDSPROF"
#define PRFKEYNAME              "PRF"
#define HIPERCENT               1000
#define RES_MAIN                100

#define HCMD_NUM                200
#define HCMD_TIME               201
#define HCMD_TIME1              202
#define HCMD_NAME               203

#define IDM_FILE                1000
#define IDM_FILEOPEN            1001
#define IDM_FILEHIST_MIN        1009
#define IDM_FILEHIST_MAX        1019

#define IDM_WINDOW              1100
#define IDM_WINCOMPONENTS       1101 // T    or
#define IDM_WINMODULES          1102 //  h    d
#define IDM_WINPROCEDURES       1103 //   i    e
#define IDM_WINLINES            1104 //    s    r
#define IDM_WIN_MIN             IDM_WINCOMPONENTS
#define IDM_WIN_MAX             IDM_WINLINES
#define IDM_WINPOPALL           1105
#define IDM_WINTILE             1106
#define IDM_WINCASCADE          1107




class LISTLINE
{
public:
  LISTLINE();
 ~LISTLINE();
  PSZ        QueryText   ();      // Text of the line
  int        QueryOrder  ()          {return nOrder;}
  int        GlobRelTime ();      // Global-related time (of HIPERCENT%)
  int        ModRelTime  ();      // Module-(Procedure-...)related time (of HIPERCENT%)
  void       ClickLine   ();      // Clicked...

private:
  enum LINE_KIND { BOTTOM, COMPONENT, PUBLIC, MODULE, PROCEDURE, LINE };
  LINE_KIND      lineKind;
  int            nOrder;         // This object's order
  PSZ            pszText;        // malloc()ed text or 0
  int            nGlobRelTime;   // or -1
  int            nModRelTime;    // or -1
  int            nSnapshots;     // or -1
  LISTLINE      *pParentLine;    // or  0 for BOTTOM
  int            q_snapshots();  // snapshots here
  friend         int _Optlink compare (const void *arg1, const void *arg2);
};
typedef LISTLINE  *PLISTLINE;
typedef PLISTLINE *PPLISTLINE;

enum VIEWPORT_KIND{VP_COMPONENTS,VP_MODULES,VP_PROCEDURES,VP_LINES};
enum SORT_MODE    {SORT_ORDER=0,SORT_TIME,SORT_TEXT};
class VIEWPORT
{
public:
                VIEWPORT       (VIEWPORT_KIND vpKind);
               ~VIEWPORT       ();
  void          Init           (int nPrfItem);
  void          Kill           ();
  void          StartUpdate    ();
  void          AddLine        (PLISTLINE pLL);
  void          EndUpdate      (PSZ pszCapt=0);
  void          Sort           (SORT_MODE sMode);
  void          SetCaption     (PSZ psz);
  MRESULT       WM_DrawItem    (HWND hWnd, MPARAM m1, MPARAM m2);
  MRESULT       WM_MeasureItem (HWND hWnd, MPARAM m1, MPARAM m2);
  MRESULT       WM_Control     (HWND hWnd, MPARAM m1, MPARAM m2);
  MRESULT       WM_Command     (HWND hWnd, MPARAM m1, MPARAM m2);
  MRESULT       WM_Size        (HWND hWnd, MPARAM m1, MPARAM m2); // client's size changed
  MRESULT       WM_InitMenu    (HWND hWnd, MPARAM m1, MPARAM m2);
  MRESULT       MM_ApplyWidths (HWND hWnd, MPARAM m1, MPARAM m2);

private:
  HWND          hFrame;
  HWND          hClient;
  HWND          hHead;
  HWND          hList;
  VIEWPORT_KIND vpKind;
  SORT_MODE     sortMode;
  LONG          lNumFract;     // Пропорциональные ширины
  LONG          lTimeFract;    //   полей,
  LONG          lTime1Fract;   //     не вырождаются при
  LONG          lNameFract;    //       уменьшении окна
  LONG          xNumFract;     // Реальные ширины
  LONG          xTimeFract;    //   полей
  LONG          xTime1Fract;   //     (в экранных
  LONG          xNameFract;    //       координатах)
  LONG          dyHead;
  int           nPrfItem;
  int           nUpdateLocked;
  BOOL          fLockLMSel;

  void          clear_list     ();
  void          lock_update(BOOL fLock);
};
typedef VIEWPORT *PVIEWPORT;

class HISTORY
{
public:
            HISTORY   ();
           ~HISTORY   ();
  void      Init      (int nMinCmd, int nMaxCmd, int nSeparator);
  void      Kill      ();
  void      InitMenu  (HWND hm);
  PSZ       Cmd2File  (int nCmd);
  void      AppItem   (PSZ pszIt);
private:
  int       nMinCmd;
  int       nMaxCmd;
  int       nSeparator;
  PLINELIST pllHist;
};



#endif /* ifndef _list_ */

