typedef enum	{
			FRAME, FREE, DOCKED
		}
			WINTYPE;

typedef	enum	{
			DOCK_DOWN,
			DOCK_LEFT,
			DOCK_UP,
			DOCK_RIGHT
		}
			DOCKDIR;

typedef struct	{
			WINDOWPLACEMENT p;
			WINTYPE type;
			DOCKDIR dir;
			int	docksize;
			int	docknum;
			int	winnum;
		}
			MDIPLACEMENT;

typedef struct	{
			HCURSOR	normal_cursor;			
			HCURSOR	hsize_cursor;
			HCURSOR	vsize_cursor;
			HCURSOR moving_cursor;
		}
			MDICURSORS;

typedef	BOOL	(* MDIENUMPROC) (int num, HWND hwnd, BOOL free, BOOL docked, BOOL active, BOOL mdiactive);

#define WS_MDI_NOCLIENTADJUST 0x00000001L

/* WM_MDICREATE:
	MDIPLACEMENT * wparam;
	MDICREATESTRUCT * lparam;
	wparam can be 0;
*/

/* -------------------------------------------- */
/*	Frame callback messages			*/
/* -------------------------------------------- */

#define WM_MYMDI_RECT WM_USER+4001
/* sent to frame to determine frame area
	PRECT lparam
*/  

/* ---------------------------------------- */
#define WM_MYMDI_ACTIVATEEMPTY WM_USER+4002
/* user selected empty window in menu
	int	wparam; = window number strting from 1
*/

/* ---------------------------------------- */
#define WM_MYMDI_CLIENTPAINT WM_USER+4003
/* paint the client area
	HDC	wparam;
	returns 1 if painted
*/

#define WM_MYMDI_CLIENTERASE WM_USER+4004
/* erases the client area
	HDC	wparam;
	returns 1 if erased
*/
/* -------------------------------------------- */
#define WM_MYMDI_ENTEREMPTY WM_USER+4005
/* Activate empty window;
	int wparam = window number starting from 1
*/

/* -------------------------------------------- */
/*	Frame messages				*/
/* -------------------------------------------- */
#define WM_MYMDI_WINDOWBYNUM WM_USER+5001
/* set up menu parameter:
	int wparam = number of window (from 1)
	HWND lresult = window handle or 0
*/

#define WM_MYMDI_MENU WM_USER+5002
/* set up menu parameter:
	HMENU wparam = handle of Windows menu
	UINT  lparam = first child id
*/
/* ---------------------------------------- */
#define WM_MYMDI_CURSOR WM_USER+5003
/* set up cursor information
	MDICURSORS * lparam

/* ---------------------------------------- */

#define WM_MYMDI_NUMBERING WM_USER+5004
/* set up MDI child windows numbering style:
	UINT wparam = mask			
*/

#define MDINUMBER_REVERSE	1L	/* make new window 1st */
#define MDINUMBER_NOFLIP	2L	/* do not flip 9th window with active */
#define MDINUMBER_ACTIVEFIRST	4L	/* make active window #1*/
#define MDINUMBER_FIXEDNUMBERS	8L	/* use fixed window numbers */
#define MDINUMBER_DRAWNUMBERS	16L	/* draw window numbers in right corner */

#define WM_MYMDI_GETNUMBERING WM_USER+5005
/* get numbering style as LRESULT
*/
/* ---------------------------------------- */

#define WM_MYMDI_ENUM	WM_USER+5006
/* enum all windows ni Window Menu order
	MDIENUMPROC lparam;
*/

/* ---------------------------------------- */
#define WM_MYMDI_GETCLIENT WM_USER+5007
/* returns client window handle
	HWND Lresult = ClientWindow
*/

/* ---------------------------------------- */
#define WM_MYMDI_DOCK	WM_USER+5008
/* dock / undock client
	HWND	wparam
*/

/* ---------------------------------------- */
#define WM_MYMDI_FREE	WM_USER+5009
/* free client / return it to MDI
	HWND	wparam
*/

/* ---------------------------------------- */
#define WM_MYMDI_ENUMZORDER WM_USER+5010

/* ---------------------------------------- */
#define WM_MYMDI_COMPACTNUMBERS WM_USER+5011

/* ---------------------------------------- */
#define WM_MYMDI_TAB WM_USER+5012
/* Set tab control attributes:
	DWORD lparam = attribute mask;
*/

#define	MDITAB_ICON	0x01
#define MDITAB_NUMBER	0x02
#define MDITAB_TEXT	0x04

#define MDITAB_TOP	0x10

/* ---------------------------------------- */
#define WM_MYMDI_DOCKSTYLE WM_USER+5013
/* Set docked window look:
	BOOL wparam = small caption flag
*/

/* ---------------------------------------- */
/*	MDI child messages */
/* ---------------------------------------- */

#define WM_MYMDI_GETPLACEMENT WM_USER+6000
/* query present window placement
	MDIPLACEMENT * lparam
*/

/* ---------------------------------------- */
#define WM_MYMDI_SETPLACEMENT WM_USER+6001
/* set new placement; change, if necessary, window type
	MDIPLACEMENT * lparam
*/

/* ---------------------------------------- */
#define WM_MYMDI_GETLONGTEXT  WM_USER+6002
#define WM_MYMDI_GETSHORTTEXT WM_USER+6003
/* query text for full or minimized caption;
	UINT wparam = buffer size
	char * lparam = buffer;
*/

#define WM_MYMDI_TEXTCHANGED WM_USER+6004
/* notify that window title has been changed; it must be queried again */

/* ---------------------------------------- */

extern	BOOL	TranslateMyMDISysAccel (HWND frame, MSG * msg);
extern	LRESULT	MyDefMDIProc	  (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam);
extern	LRESULT MyDefMDIChildProc (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam);

