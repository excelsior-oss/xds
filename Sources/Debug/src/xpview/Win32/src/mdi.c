#include <windows.h>
#include <commctrl.h>
#include "mdi.h"

/* ---------------------------------------------------------- */
/* Parameters for WM_COMMAND message */

#define CONTROL_ID   ((UINT) LOWORD (wparam))
#define NOTIFY_CODE  ((UINT) HIWORD (wparam))
#define CONTROL_HWND ((HWND) lparam)
/* ---------------------------------------------------------- */

#define NT35 (LOBYTE (LOWORD (GetVersion())) <= 3)

#define WM_MYMDI_SIZE WM_USER+3000
#define WM_MYMDI_MOVE WM_USER+3001
#define WM_MYMDI_TEXT WM_USER+3002

#define WS_MDI_NOCLIENTADJUST 0x00000001L

#define SC_DOCK		0xF200
#define SC_UNDOCK	0xF201
#define SC_FREE		0xF202
#define SC_MDI		0xF203
#define SC_MAIN		0xF204

#define MAX_NWINS 100
#define MAX_NICONS 10

ATOM	client_class = 0;
ATOM	mdi_prop_atom = 0;
HFONT	caption_font;
HFONT	caption_bold_font;
HFONT	caption_nonbold_font;
HFONT	caption_small_font;
HFONT	menu_font;

#define DOCK_HORZ(x) ((x) == DOCK_LEFT || (x) == DOCK_RIGHT)
#define DOCK_VERT(x) ((x) == DOCK_UP   || (x) == DOCK_DOWN)

#define DOCK_BORDER_SIZE 5

typedef struct dockwin {
			int	dock_dir;
			int	dock_size;
			int	max_size;
			RECT	wnd_rect;
			RECT	border_rect;
			HWND	hwnd;
			struct	dockwin * next;
		}
			DOCKWIN_INFO;

typedef	struct	{
		HWND	FrameWindow;
		HWND	ClientWindow;
		RECT	ClientArea;
		RECT	FrameArea;
		HWND	win_list [MAX_NWINS];
		int	win_cnt;
		HMENU	WindowsMenu;
		UINT	first_child;
		HWND	active_child;
		HWND	mdi_active_child;
		HWND	cur_button_child;
		HWND	mdi_maximized_child;
		int	move_lock_count;
		int	set_text_lock_count;
		int	ncpaint_lock_count;
		int	dflt_x;
		char	title_save [200];
		BOOL	title_set;
		HWND	hscroll;
		HWND	vscroll;
		MDICURSORS cursors;
		HCURSOR	cur_drag_cursor;
		DOCKWIN_INFO * dock_list;
		DOCKWIN_INFO * drag_over_border;
		int	dock_drag_delta;
		DOCKWIN_INFO * drag_insert;
		DOCKWIN_INFO * dragged_dock;
		int	drag_make_dir;
		int	drag_make_size;
		RECT	track_rect;
		int	create_dock_dir;
		UINT	number_flags;
		BOOL	dock_small;
		int	new_window_num;
		HWND	Tab;
		HIMAGELIST ilist;
		HICON	icons [MAX_NICONS];
		int	nicons;
		int	min_tab_size, max_tab_size;
		int	tab_xsize, tab_ysize;
		DWORD	tab_attr;
	}
		MDIDATA;


#define FREE_WINDOW(x)	 (x && GetParent (x) == 0)
#define DOCKED_WINDOW(x) (m && x && GetParent (x) == m->FrameWindow)
#define MDI_WINDOW(x)	 (m && x && GetParent (x) == m->ClientWindow)
#define OUR_WINDOW(x)	 (m && find_our_window (m, x) >= 0)

void	mdi_activate_child (MDIDATA * m, HWND hwnd);
void	activate_child (MDIDATA * m, HWND hwnd);
void	test_draw_scrollbars (MDIDATA * m, BOOL move_client);
void	draw_window_label (MDIDATA * m, HWND hwnd);
void	child_command (MDIDATA * m, int command, HWND child);
void	size_frame (MDIDATA * m, BOOL recalc_tabs);

/* --------------------------------------------------------------- */

void	MoveRect (HWND hwnd, RECT * rc)
{
	MoveWindow (hwnd, rc->left, rc->top, rc->right-rc->left, rc->bottom-rc->top, 1);
}

void	MoveRectZ (HWND hwnd, RECT * rc)
{
	MoveWindow (hwnd, rc->left, rc->top, rc->right-rc->left, rc->bottom-rc->top, 0);
}

void	client_to_screen_rect (HWND hwnd, RECT * rc)
{
	POINT pt;

	pt.x = rc->left; pt.y = rc->top;
	ClientToScreen (hwnd, &pt);
	rc->left = pt.x; rc->top = pt.y;
	pt.x = rc->right; pt.y = rc->bottom;
	ClientToScreen (hwnd, &pt);
	rc->right = pt.x; rc->bottom = pt.y;
}

void	screen_to_client_rect (HWND hwnd, RECT * rc)
{
	POINT pt;

	pt.x = rc->left; pt.y = rc->top;
	ScreenToClient (hwnd, &pt);
	rc->left = pt.x; rc->top = pt.y;
	pt.x = rc->right; pt.y = rc->bottom;
	ScreenToClient (hwnd, &pt);
	rc->right = pt.x; rc->bottom = pt.y;
}

int	find_our_window (MDIDATA * m, HWND hwnd)
{
	int i;

	for (i = 0; i < m->win_cnt; i++)
		if (m->win_list [i] == hwnd) return i;
	return -1;
}

HWND	find_next_mdi (MDIDATA * m, HWND hwnd)
{
	if (!hwnd)	return NULL;
	do
		hwnd = GetWindow (hwnd, GW_HWNDNEXT);
	while (hwnd && ! OUR_WINDOW (hwnd));
	return hwnd;
}

void	convert_coordinates (MDIDATA * m,
			     int x0, int y0, int w0, int h0,
			     int *x, int *y, int *w, int *h)
{
	int d;
	RECT rc;

	*x = x0;  *y = y0;
	*w = w0;   *h = h0;

	GetClientRect (m->ClientWindow, &rc);

	if (x0 == CW_USEDEFAULT) *x = m->dflt_x;
	if (y0 == CW_USEDEFAULT) *y = m->dflt_x;
	if (w0 == CW_USEDEFAULT)	{
		*w = rc.right * 2/3;
		if (*x + *w > rc.right) *w = rc.right - *x;
	}
	if (h0 == CW_USEDEFAULT)	{
		*h = rc.bottom * 2/3;
		if (*y + *h > rc.bottom) *h = rc.bottom - *y;
	}
	if (w0 == CW_USEDEFAULT && h0 == CW_USEDEFAULT)	{
		d = min (rc.bottom - *y - *h, rc.right - *x - *w);
		*w = rc.right  - *x - d;
		*h = rc.bottom - *y - d;
	}
	if (x0 == CW_USEDEFAULT || y0 == CW_USEDEFAULT)
		if (*x + *w >= rc.right || *y + *h >= rc.bottom)
			m->dflt_x = 0;
		else
			m->dflt_x += GetSystemMetrics (SM_CYCAPTION);
}

void	draw_raised_frame (HDC hdc, RECT * rc, HPEN light, HPEN dark)
{
	HPEN old;
	rc->right --;
	rc->bottom --;
	old = SelectObject (hdc, light);
	MoveToEx (hdc, rc->right, rc->top, NULL);
	LineTo (hdc, rc->left, rc->top);
	LineTo (hdc, rc->left, rc->bottom+1);
	SelectObject (hdc, dark);
	MoveToEx (hdc, rc->left+1, rc->bottom, NULL);
	LineTo (hdc, rc->right, rc->bottom);
	LineTo (hdc, rc->right, rc->top);
	rc->left ++; rc->top ++;
	SelectObject (hdc, old);
}

/* ------------------------- WinBar control ------------ */

int	tab_add_icon (MDIDATA * m, HICON icon)
{
	int i;

	if (!icon) return -1;
	for (i = 0; i < m->nicons; i++)
		if (m->icons [i] == icon) return i;
	if (m->nicons == MAX_NICONS) return -1;
	m->icons [m->nicons] = icon;
	ImageList_ReplaceIcon (m->ilist, m->nicons, icon);
	return m->nicons++;
}

int	tab_number (MDIDATA * m, HWND hwnd)
{
	int i, n;
	n = 0;
	for (i = 0; i < m->win_cnt; i++)
		if (m->win_list [i] == hwnd)
			return n;
		else if (m->win_list [i])
			++n;
	return -1;
}

HWND	tab_window (MDIDATA * m, int n)
{
	int i;
	if (n < 0) return NULL;
	for (i = 0; i < m->win_cnt; i++)
		if (m->win_list [i] && !n--)
				return m->win_list [i];
	return NULL;
}

void	set_min_position (MDIDATA * m, HWND hwnd)
{
	WINDOWPLACEMENT pl;
	RECT rc;
	int n;
	if (!m->Tab) return;
	if (!MDI_WINDOW (hwnd)) return;
	n = tab_number (m, hwnd);
	if (!m->win_list [n]) return;
	pl.length = sizeof (pl);
	GetWindowPlacement (hwnd, &pl);
	if (NT35)
		pl.ptMinPosition.x = pl.ptMinPosition.y = -30000;
	else	{
		TabCtrl_GetItemRect (m->Tab, n, &rc);
		client_to_screen_rect (m->Tab, &rc);
		screen_to_client_rect (m->ClientWindow, &rc);
		pl.ptMinPosition.x = rc.left;
		pl.ptMinPosition.y = rc.top;
	}
	pl.flags |= WPF_SETMINPOSITION;
	SetWindowPlacement (hwnd, &pl);
}

void	set_min_positions (MDIDATA * m)
{
	int i;

	if (!m->Tab) return;

	for (i = 0; i < m->win_cnt; i++)
		if (m->win_list [i])
			set_min_position (m, m->win_list [i]);
}

HICON	window_icon (HWND hwnd)
{
	return (HICON) GetClassLong (hwnd, NT35 ? GCL_HICON : GCL_HICONSM);
}

void	refresh_tab (MDIDATA * m)
{
	size_frame (m, TRUE);
}

void	add_to_tab_norefresh (MDIDATA * m, HWND hwnd)
{
	TC_ITEM item;
	int n;

	if (!m->Tab || !hwnd) return;
	memset (&item, 0, sizeof (item));
	item.mask = TCIF_PARAM;
	item.lParam = (LPARAM) hwnd;
	n = tab_number (m, hwnd);
	TabCtrl_InsertItem (m->Tab, n, &item);
}

void	add_to_tab (MDIDATA * m, HWND hwnd)
{
	if (!hwnd) return;
	add_to_tab_norefresh (m, hwnd);
	refresh_tab (m);
}

void	delete_from_tab (MDIDATA * m, int n)
{
	if (!m->Tab) return;
	TabCtrl_DeleteItem (m->Tab, n);
	refresh_tab (m);
}

void	calc_tab_size (MDIDATA * m)
{
	TEXTMETRIC tm;
	HDC dc;
	HFONT font;
	HBITMAP bmp;
	int cx, cy, cx0, cy0, i, nmin, nmax;

	if ((m->tab_attr & (MDITAB_NUMBER | MDITAB_TEXT | MDITAB_ICON)) == 0)	{
		m -> tab_xsize = GetSystemMetrics (SM_CXSMICON) + 4;
		m -> tab_ysize = GetSystemMetrics (SM_CYSMICON) + 4;
		m -> min_tab_size = m -> tab_xsize;
		m -> max_tab_size = m -> tab_xsize;
		return;
	}

	dc = GetDC (0);
	font = SelectObject (dc, caption_bold_font);
	GetTextMetrics (dc, &tm);

	nmin = 0;
	nmax = 0;
	if (m->tab_attr & MDITAB_NUMBER)
		nmin = nmax = 3;
	if (m->tab_attr & MDITAB_TEXT)	{
		nmin += 10;
		nmax += 20;
	}
	m -> min_tab_size = tm.tmAveCharWidth * nmin;
	m -> max_tab_size = tm.tmAveCharWidth * nmax;
	m -> tab_ysize = 0;
	if (m -> tab_attr & (MDITAB_NUMBER | MDITAB_TEXT))
		m -> tab_ysize = tm.tmHeight;
	
	if (m -> tab_attr & MDITAB_ICON)	{
		cx0 = GetSystemMetrics (SM_CXICON)/2;
		cy0 = GetSystemMetrics (SM_CYICON)/2;
		cx = GetSystemMetrics (SM_CXSMICON);
		cy = GetSystemMetrics (SM_CYSMICON);
		if (cx0 > cx) cx = cx0;
		if (cy0 > cy) cy = cy0;
		ImageList_SetIconSize (m->ilist, cx, cy);
		bmp = CreateCompatibleBitmap (dc, cx, cy);
		for (i = 0; i < MAX_NICONS; i++)
			ImageList_Add (m->ilist, bmp, bmp);
		m->nicons = 0;
		DeleteObject (bmp);
		m -> min_tab_size += cx + 4;
		m -> max_tab_size += cx + 4;
		if (m -> tab_ysize < cy) m -> tab_ysize = cy;
	}
	m -> tab_ysize += 5;
	m -> min_tab_size += 4;
	m -> max_tab_size += 4;

	ReleaseDC (0, dc);
}

void	set_tab_item_size (MDIDATA * m)
{
	int dx;
	
	SendMessage (m->Tab, WM_SETREDRAW, 0, 0);
	dx = m->min_tab_size;
	TabCtrl_SetItemSize (m->Tab, dx, m->tab_ysize);
	if (TabCtrl_GetRowCount (m->Tab) <= 1)
		for (dx = m -> min_tab_size + 1; dx < m->max_tab_size; dx ++)	{
			TabCtrl_SetItemSize (m->Tab, dx, m->tab_ysize);
			if (TabCtrl_GetRowCount (m->Tab) > 1)	{
				-- dx;
				TabCtrl_SetItemSize (m->Tab, dx, m->tab_ysize);
				break;
			}
		}
	if (m->tab_xsize != dx)
		InvalidateRect (m->Tab, 0, 0);
	m->tab_xsize = dx;
	SendMessage (m->Tab, WM_SETREDRAW, 1, 0);
}

void	Create_tab_control (MDIDATA * m)
{
	int i;

	calc_tab_size (m);
	m->Tab = CreateWindow (
			WC_TABCONTROL,
			"tab control",
			WS_VISIBLE|WS_CHILD|
			TCS_FOCUSNEVER|TCS_TOOLTIPS|
			TCS_BUTTONS|TCS_MULTILINE|TCS_FIXEDWIDTH|TCS_OWNERDRAWFIXED,
			m->FrameArea.left, 10000, m->FrameArea.right-m->FrameArea.left, 1000,
			m->FrameWindow,
			NULL,                   /* menu handle */
			(HINSTANCE) GetWindowLong (m->FrameWindow, GWL_HINSTANCE),
			NULL			/* create parms */
		);

	for (i = 0; i < m->win_cnt; i++)
		add_to_tab_norefresh (m, m->win_list [i]);
	i = tab_number (m, m->active_child);
	TabCtrl_SetCurSel (m->Tab, i);
	refresh_tab (m);
	set_min_positions (m);
}

void	set_tab_attr (MDIDATA * m, BOOL present, DWORD w)
{
	BOOL p = m->Tab != 0;
	
	if (w == m->tab_attr && p == present) return;
	if (m->Tab) DestroyWindow (m->Tab);
	m->Tab = 0;
	m -> tab_attr = w;
	if (present)
		Create_tab_control (m);
	else
		refresh_tab (m);
}

void	paint_tab_item (MDIDATA * m, HDC dc, RECT * rc, int n, BOOL selected)
{
	char s [200];
	HWND hwnd;
	HFONT font;
	HBRUSH br;
	int sx, sy, dy, image, num;
	HICON icon;
	COLORREF col;
	DWORD mode;

	col = GetSysColor (selected ? COLOR_3DHILIGHT : COLOR_3DFACE);
	br = CreateSolidBrush (col);
	FillRect (dc, rc, br);
	DeleteObject (br);
	hwnd = tab_window (m, n);
	s [0] = 0;
	if (m -> tab_attr & MDITAB_NUMBER)	{
		num = find_our_window (m, hwnd);
		if (num >= 0)	{
			wsprintf (s, "%d", num+1);
			if (m -> tab_attr & MDITAB_TEXT)
				strcat (s, ":");
			if (m -> tab_attr & MDITAB_TEXT)
				strcat (s, " ");
		}
	}
	if (m -> tab_attr & MDITAB_TEXT)	{
		SendMessage (hwnd, WM_MYMDI_GETSHORTTEXT, sizeof (s)-4, (LPARAM) s+strlen(s));
		s [sizeof (s)-1] = 0;
	}
	++rc->top; --rc->bottom;
	dy = rc->bottom - rc->top;

	if (selected)	{
		++rc->left; ++rc->top; rc->bottom;
		font = SelectObject (dc, caption_bold_font);
	} else
		font = SelectObject (dc, caption_nonbold_font);
	icon = (m->tab_attr & MDITAB_ICON) ? window_icon (hwnd) : 0;
	if (icon)	{
		image = tab_add_icon (m, icon);
		if (image >= 0)	{
			ImageList_GetIconSize (m->ilist, &sx, &sy);
			ImageList_Draw (m->ilist, image, dc,
					rc->left+2, rc->top + (dy-sy)/2,
					ILD_TRANSPARENT);
			rc->left += sx + 4;
		}
	}
	SetBkMode (dc, TRANSPARENT);
	mode = DT_LEFT;
	if ((m -> tab_attr & (MDITAB_TEXT | MDITAB_NUMBER)) == MDITAB_NUMBER)
		mode = DT_CENTER;
	mode |= DT_SINGLELINE | DT_VCENTER | DT_NOPREFIX;
	if (NT35)
		DrawText (dc, s, -1, rc, mode);
	else
		DrawTextEx (dc, s, -1, rc, mode | DT_PATH_ELLIPSIS, 0);

	SelectObject (dc, font);
}

void	draw_tab_item (MDIDATA * m, DRAWITEMSTRUCT * ds)
{
	HDC bdc;
	HBITMAP bmp, obmp;
	RECT rc;
	int w, h;
	rc.left = rc.top = 0;
	rc.right  = w = ds->rcItem.right  - ds->rcItem.left;
	rc.bottom = h = ds->rcItem.bottom - ds->rcItem.top;
	bmp = CreateCompatibleBitmap (ds->hDC, w, h);
	bdc = CreateCompatibleDC (ds->hDC);
	obmp = SelectObject (bdc, bmp);
	paint_tab_item (m, bdc, &rc, ds->itemID, (ds->itemState & ODS_SELECTED));
	BitBlt (ds->hDC, ds->rcItem.left, ds->rcItem.top, w, h, bdc, 0, 0, SRCCOPY);
	SelectObject (bdc, obmp);
	DeleteDC (bdc);
	DeleteObject (bmp);
	//paint_tab_item (m, ds->hDC, &ds->rcItem, ds->itemID, (ds->itemState & ODS_SELECTED));
}

void	invalidate_tab (MDIDATA * m, HWND hwnd)
{
	int i;
	RECT rc;

	if (!m->Tab) return;
	if (!hwnd)	{
		InvalidateRect (m->Tab, 0, 0);
		return;
	}
	i = tab_number (m, hwnd);
	if (i < 0) return;
	if (TabCtrl_GetItemRect (m->Tab, i, &rc))
		InvalidateRect (m->Tab, &rc, 0);
	set_min_position (m, hwnd);
}

void	tab_need_text (MDIDATA * m, NMHDR * nm)
{
	static char tip_buffer [512];
	HWND hwnd;
	TOOLTIPTEXT * t = (TOOLTIPTEXT*) nm;
	hwnd = tab_window (m, nm->idFrom);
	t->szText [0] = 0;
	if (!hwnd) return;
	wsprintf (tip_buffer, "%d: ", find_our_window (m, hwnd)+1);
	SendMessage (hwnd, WM_MYMDI_GETLONGTEXT, sizeof (tip_buffer)-5,
		     (LPARAM) (tip_buffer+strlen(tip_buffer)));
	tip_buffer [sizeof (tip_buffer)-1] = 0;
	t->lpszText = tip_buffer;
}

void	tab_system_menu (MDIDATA * m)
{
	POINT pt;
	TC_HITTESTINFO ht;
	HMENU menu;
	HWND child;
	int i;
	BOOL icon, zoom;
	GetCursorPos (&pt);
	ht.pt = pt;
	ScreenToClient (m->Tab, &ht.pt);
	i = TabCtrl_HitTest (m->Tab, &ht); 
	if (i < 0) return;
	child = tab_window (m, i);
	if (! child) return;
	activate_child (m, child);
	menu = GetSystemMenu (child, 0);
	icon = IsIconic (child);
	zoom = IsZoomed (child);
	EnableMenuItem (menu, SC_RESTORE, (icon || zoom) ? (MF_ENABLED | MF_BYCOMMAND) : (MF_GRAYED | MF_BYCOMMAND));
	EnableMenuItem (menu, SC_MAXIMIZE, zoom ? (MF_GRAYED | MF_BYCOMMAND) : (MF_ENABLED | MF_BYCOMMAND));
	EnableMenuItem (menu, SC_MINIMIZE, icon ? (MF_GRAYED | MF_BYCOMMAND) : (MF_ENABLED | MF_BYCOMMAND));
	EnableMenuItem (menu, SC_MOVE, (icon || zoom) ? (MF_GRAYED | MF_BYCOMMAND) : (MF_ENABLED | MF_BYCOMMAND));
	EnableMenuItem (menu, SC_SIZE, (icon || zoom) ? (MF_GRAYED | MF_BYCOMMAND) : (MF_ENABLED | MF_BYCOMMAND));
	SetMenuDefaultItem (menu, (zoom || icon) ? SC_MAXIMIZE : SC_RESTORE, 0);
	TrackPopupMenu (menu, TPM_LEFTALIGN | TPM_LEFTBUTTON | TPM_RIGHTBUTTON, pt.x, pt.y, 0, child, 0);
}

/* ------------------------- Docked children control ------------- */

DOCKWIN_INFO * find_dock_info (MDIDATA * m, HWND hwnd)
{
	DOCKWIN_INFO * d;

	for (d = m->dock_list; d; d = d -> next)
		if (d->hwnd == hwnd) break;
	return d;
}

void	add_to_dock_list (MDIDATA * m, DOCKWIN_INFO * d)
{
	DOCKWIN_INFO ** l;
	for (l = &m->dock_list; *l; l = &(*l)->next);
	*l = d;
	d -> next = NULL;
}

void	put_into_dock_list (MDIDATA * m, DOCKWIN_INFO * d, int pos)
{
	DOCKWIN_INFO * dd;
	if (pos <= 0 || ! m->dock_list)	{
		d->next = m->dock_list;
		m->dock_list = d;
	} else	{
		for (dd = m->dock_list; dd->next && --pos; dd = dd -> next);
		d->next = dd->next;
		dd->next = d;
	}
}

void	remove_dock_info (MDIDATA * m, DOCKWIN_INFO * d)
{
	DOCKWIN_INFO * l;
	if (!d) return;
	if (m->dock_list == d)
		m->dock_list = d -> next;
	else	{
		for (l = m->dock_list; l && l->next != d; l = l -> next);
		if (l) l -> next = d -> next;
	}
}

void	delete_dock_info (MDIDATA * m, DOCKWIN_INFO * d)
{
	if (!d) return;
	remove_dock_info (m, d);
	free (d);
}

void	insert_dock_info (MDIDATA * m, DOCKWIN_INFO * d, DOCKWIN_INFO * before)
{
	DOCKWIN_INFO * dd;
	if (before == m->dock_list)	{
		d -> next = m->dock_list;
		m->dock_list = d;
		return;
	}
	for (dd = m->dock_list; dd; dd = dd->next)
		if (!dd->next || dd->next == before)	{
			d->next = dd->next;
			dd->next = d;
			return;
		}
}

void	move_dock_info (MDIDATA * m, DOCKWIN_INFO * d, DOCKWIN_INFO * before)
{
	if (d == before) return;
	remove_dock_info (m, d);
	insert_dock_info (m, d, before);
}

void	Rearrange_dock (MDIDATA * m)
{
	RECT rc, rcdock, rcborder;
	DOCKWIN_INFO * d;
	HDWP dwp;
	int cnt;

	rc = m->FrameArea;
	cnt = 0;
	for (d = m->dock_list; d; d = d -> next) ++ cnt;
	if (!cnt)	{
		m->ClientArea = rc;
		InvalidateRect (m->FrameWindow, &m->ClientArea, 0);
		test_draw_scrollbars (m, TRUE);
		return;
	}

	dwp = 0;//BeginDeferWindowPos (cnt);
	for (d = m->dock_list; d; d = d -> next)	{
		rcdock = rcborder = rc;
		if (DOCK_HORZ (d -> dock_dir))	{
			d -> max_size = (rc.right - rc.left) / 2  - DOCK_BORDER_SIZE;
			if (d -> dock_size == 0)
				d -> dock_size = (rc.right - rc.left) / 3 - DOCK_BORDER_SIZE;
		} else	{
			d -> max_size = (rc.bottom - rc.top) / 2 - DOCK_BORDER_SIZE;
			if (d -> dock_size == 0)
				d -> dock_size = (rc.bottom - rc.top) / 3 - DOCK_BORDER_SIZE;
		}
		if (d -> max_size  < DOCK_BORDER_SIZE) d -> max_size  = DOCK_BORDER_SIZE;
		if (d -> dock_size < DOCK_BORDER_SIZE) d -> dock_size = DOCK_BORDER_SIZE;

		switch (d -> dock_dir)	{
		case DOCK_UP:
				rc.top += min (d -> dock_size, d -> max_size);
				rcdock.bottom = rcborder.top = rc.top;
				rc.top += DOCK_BORDER_SIZE;
				rcborder.bottom = rc.top;
				break;
		case DOCK_DOWN:
				rc.bottom -= min (d -> dock_size, d -> max_size);
				rcdock.top = rcborder.bottom = rc.bottom;
				rc.bottom -= DOCK_BORDER_SIZE;
				rcborder.top = rc.bottom;
				break;
		case DOCK_LEFT:
				rc.left += min (d -> dock_size, d -> max_size);
				rcdock.right = rcborder.left = rc.left;
				rc.left += DOCK_BORDER_SIZE;
				rcborder.right = rc.left;
				break;
		case DOCK_RIGHT:
				rc.right -= min (d -> dock_size, d -> max_size);
				rcdock.left = rcborder.right = rc.right;
				rc.right -= DOCK_BORDER_SIZE;
				rcborder.left = rc.right;
				break;
		}
		d->wnd_rect = rcdock;
		if (!EqualRect (&d->border_rect, &rcborder))	{
			d->border_rect = rcborder;
			InvalidateRect (m->FrameWindow, &rcborder, 0);
		}

		if (dwp)
			dwp = DeferWindowPos (dwp, d->hwnd, 0, rcdock.left, rcdock.top,
					      rcdock.right - rcdock.left,
					      rcdock.bottom - rcdock.top, SWP_NOZORDER);
		else
			MoveRect (d->hwnd, &rcdock);
	}
	if (dwp)
		EndDeferWindowPos (dwp);
	m->ClientArea = rc;
	InvalidateRect (m->FrameWindow, &m->ClientArea, 0);
	test_draw_scrollbars (m, TRUE);
}

void	test_drag (MDIDATA * m, int x, int y)
{
	POINT pt;
	DOCKWIN_INFO * d;
	int size;
	if (GetCapture () == m->FrameWindow)	{
		d = m->drag_over_border;
		if (!d) return;
		switch (d->dock_dir)	{
		case DOCK_LEFT:
			size = x - d->wnd_rect.left; break;
		case DOCK_RIGHT:
			size = d->wnd_rect.right - x; break;
		case DOCK_UP:
			size = y - d->wnd_rect.top; break;
		case DOCK_DOWN:
			size = d->wnd_rect.bottom - y; break;
		}
		size -= m->dock_drag_delta;
		if (size < DOCK_BORDER_SIZE) size = DOCK_BORDER_SIZE;
		if (size > d -> max_size) size = d -> max_size;
		d -> dock_size = size;
		Rearrange_dock (m);
	} else	{
		pt.x = x; pt.y = y;
		for (d = m->dock_list; d; d = d -> next)
			if (PtInRect (&d->border_rect, pt))
				break;
		m->drag_over_border = d;
		if (!d)
			m->cur_drag_cursor = m->cursors.normal_cursor;
		else
			m->cur_drag_cursor = DOCK_HORZ (d->dock_dir) ? 
				m->cursors.hsize_cursor : m->cursors.vsize_cursor;
		SetCursor (m->cur_drag_cursor);
	}
}

void	begin_drag (MDIDATA * m, int x, int y)
{
	DOCKWIN_INFO * d;
	POINT pt;

	pt.x = x; pt.y = y;
	for (d = m->dock_list; d; d = d -> next)
		if (PtInRect (&d->border_rect, pt))
			break;
	m->drag_over_border = d;
	if (!d)	{
		SetCursor (m->cur_drag_cursor = m->cursors.normal_cursor);
		return;
	}

	switch (d->dock_dir)	{
	case DOCK_LEFT:
		m->dock_drag_delta = x - d->border_rect.left; break;
	case DOCK_RIGHT:
		m->dock_drag_delta = d->border_rect.right - x; break;
	case DOCK_UP:
		m->dock_drag_delta = y - d->border_rect.top; break;
	case DOCK_DOWN:
		m->dock_drag_delta = d->border_rect.bottom - y; break;
	}

	m->cur_drag_cursor = (d->dock_dir == DOCK_LEFT || d->dock_dir == DOCK_RIGHT) ?
		m->cursors.hsize_cursor : m->cursors.vsize_cursor;
	SetCursor (m->cur_drag_cursor);
	SetCapture (m->FrameWindow);
}

void	calc_new_drag_rect (MDIDATA * m, HWND hwnd, int x, int y, RECT * rc)
{
	DOCKWIN_INFO * d, *d0;
	POINT pt;

	int dx, dy, up, dn, rt, lt, dir;

	pt.x = x; pt.y = y;

	m->drag_make_size = 0;
	m->drag_insert = NULL;
	
	if (!PtInRect (&m->FrameArea, pt))	{
		if (x < m->FrameArea.left)
			dir = DOCK_LEFT;
		else if (x > m->FrameArea.right)
			dir = DOCK_RIGHT;
		else if (y < m->FrameArea.top)
			dir = DOCK_UP;
		else
			dir = DOCK_DOWN;
		if (m->dragged_dock == m->dock_list &&
		    dir == m->dragged_dock->dock_dir)
			return;
		m->drag_make_dir = dir;
		m->drag_insert = m->dock_list;
		if (DOCK_HORZ (dir) == DOCK_HORZ (m->dragged_dock->dock_dir))
			m->drag_make_size = m->dragged_dock->dock_size;
		else if (DOCK_HORZ (dir))
			m ->drag_make_size = (m->FrameArea.right  - m->FrameArea.left) / 3;
		else
			m ->drag_make_size = (m->FrameArea.bottom - m->FrameArea.top) / 3;
		*rc = m->FrameArea;
		switch (dir)	{
		case DOCK_UP:	rc->bottom = rc->top    + m->drag_make_size; break;
		case DOCK_DOWN:	rc->top    = rc->bottom - m->drag_make_size; break;
		case DOCK_LEFT:	rc->right  = rc->left   + m->drag_make_size; break;
		case DOCK_RIGHT:rc->left   = rc->right  - m->drag_make_size; break;
		}
		return;
	}
	if (PtInRect (&m->ClientArea, pt))	{
		dx = m->ClientArea.right  - m->ClientArea.left;
		dy = m->ClientArea.bottom - m->ClientArea.top;
		up = (y - m->ClientArea.top)    * 300 / dy;
		dn = (m->ClientArea.bottom - y) * 300 / dy;
		lt = (x - m->ClientArea.left)   * 300 / dx;
		rt = (m->ClientArea.right - x)  * 300 / dx;
		if (!m->dragged_dock -> next)
			switch (m->dragged_dock -> dock_dir)	{
			case DOCK_LEFT:	 lt = 100; break;
			case DOCK_RIGHT: rt = 100; break;
			case DOCK_UP:	 up = 100; break;
			case DOCK_DOWN:	 dn = 100; break;
			}

		if (up >= 100 && dn >= 100 && lt >= 100 && rt >= 100)
			return;
		* rc = m->ClientArea;
		if (up <= dn && up <= lt && up <= rt)	{
			m->drag_make_dir = DOCK_UP;
			m->drag_make_size = dy / 3;
			rc->bottom = m->ClientArea.top + dy / 3;
		} else if (dn <= up && dn <= lt && dn <= rt)	{
			m->drag_make_dir = DOCK_DOWN;
			rc->top = m->ClientArea.bottom - dy / 3;
			m->drag_make_size = dy / 3;
		} else if (lt <= up && lt <= dn && lt <= rt)	{
			m->drag_make_dir = DOCK_LEFT;
			rc->right = m->ClientArea.left + dx / 3;
			m->drag_make_size = dx / 3;
		} else	{
			m->drag_make_dir = DOCK_RIGHT;
			rc->left = m->ClientArea.right - dx / 3;
			m->drag_make_size = dx / 3;
		}
		return;
	}
	
	for (d0 = m->dock_list; d0; d0 = d0 -> next)
		if (d0->hwnd == hwnd) break;
	if (!d0) return;
	for (d = m->dock_list; d; d = d -> next)
		if (PtInRect (&d->wnd_rect, pt))
			break;
	if (!d) d = d0;
	*rc = d -> wnd_rect;
	m->drag_insert = d;
}

void	invert_rect (RECT * rc)
{
	RECT r;
	HDC dc = GetDC (0);
	r = *rc;
	DrawFocusRect (dc, &r);
	r.left ++; r.right --; r.top ++; r.bottom --;
	DrawFocusRect (dc, &r);
	ReleaseDC (0, dc);
}

void	docked_child_begin_drag (MDIDATA * m, HWND hwnd)
{
	DOCKWIN_INFO * d;

	for (d = m->dock_list; d; d = d -> next)
		if (d->hwnd == hwnd) break;
	if (!d) return;
	m->dragged_dock = d;
	GetWindowRect (hwnd, &m->track_rect);
	invert_rect (&m->track_rect);
	SetCursor (m->cursors.moving_cursor);
	SetCapture (hwnd);
}

void	docked_child_in_drag (MDIDATA * m, HWND hwnd, POINTS pt)
{
	POINT p;
	RECT rc;
	if (IsRectEmpty (&m->track_rect)) return;
	p.x = pt.x;
	p.y = pt.y;
	ClientToScreen (hwnd, &p);
	ScreenToClient (m->FrameWindow, &p);
	rc = m->track_rect;
	screen_to_client_rect (m->FrameWindow, &m->track_rect);
	calc_new_drag_rect (m, hwnd, p.x, p.y, &m->track_rect);
	client_to_screen_rect (m->FrameWindow, &m->track_rect);
	invert_rect (&rc);
	invert_rect (&m->track_rect);
	SetCursor (m->cursors.moving_cursor);
}

void	docked_child_stop_drag (MDIDATA * m, HWND hwnd)
{
	if (IsRectEmpty (&m->track_rect)) return;
	invert_rect (&m->track_rect);
	screen_to_client_rect (m->FrameWindow, &m->track_rect);
	SetRectEmpty (&m->track_rect);
	ReleaseCapture ();

	if (m->drag_make_size)	{
		m->dragged_dock -> dock_size = m->drag_make_size;
		m->dragged_dock -> dock_dir = m->drag_make_dir;
	}

	if (m->drag_insert)	{
		move_dock_info (m, m->dragged_dock, m->drag_insert);
		if (! m->drag_make_size)	{
			if (DOCK_HORZ (m->dragged_dock->dock_dir) != DOCK_HORZ (m->drag_insert->dock_dir))
				m->dragged_dock -> dock_size = m->drag_insert -> dock_size;
			m->dragged_dock -> dock_dir = m->drag_insert -> dock_dir;
		}
	}
	else if (m->drag_make_size)
		move_dock_info (m, m->dragged_dock, 0);
	else
		return;
	SetCursor (m->cursors.normal_cursor);
	Rearrange_dock (m);
}

void	test_kill_drag_rect (MDIDATA * m)
{
	if (IsRectEmpty (&m->track_rect)) return;
	invert_rect (&m->track_rect);
	SetRectEmpty (&m->track_rect);
	m->dragged_dock = 0;
}

int	closest_dir (HWND child, HWND client)
{
	RECT rc, crc;
	int dl, dr, du, dd;

	GetWindowRect (child,  &rc);
	GetWindowRect (client, &crc);

	dl = rc.left - crc.left;
	dr = crc.right - rc.right;
	du = rc.top - crc.top;
	dd = crc.bottom - rc.bottom;

	if (dl < dr)
		if (dl < du)
			return dl < dd ? DOCK_LEFT : DOCK_DOWN;
		else
			return du < dd ? DOCK_UP : DOCK_DOWN;
	else
		if (dr < du)
			return dr < dd ? DOCK_RIGHT : DOCK_DOWN;
		else
			return du < dd ? DOCK_UP : DOCK_DOWN;
}

void	dock_child (MDIDATA * m, HWND child)
{
	DWORD style;
	DOCKWIN_INFO * d;
	HMENU menu;

	d = malloc (sizeof (DOCKWIN_INFO));
	if (!d) return;
	memset (d, 0, sizeof (DOCKWIN_INFO));
	mdi_activate_child (m, NULL);
	if (m->mdi_maximized_child == child)
		ShowWindow (child, SW_RESTORE);
	d -> dock_dir = closest_dir (child, m->ClientWindow);
	d -> dock_size = DOCK_HORZ (d->dock_dir) ?
		(m->ClientArea.right - m->ClientArea.left)/3 :
		(m->ClientArea.bottom - m->ClientArea.top)/3;
	d -> hwnd = child;
	add_to_dock_list (m, d);
	style = GetWindowLong (child, GWL_STYLE);
	style &= ~(WS_MAXIMIZEBOX | WS_MINIMIZEBOX | WS_THICKFRAME);
	SetWindowLong (child, GWL_STYLE, style);
	if (m -> dock_small && !NT35)	{
		DWORD exstyle = GetWindowLong (child, GWL_EXSTYLE);
		exstyle |= WS_EX_TOOLWINDOW;
		SetWindowLong (child, GWL_EXSTYLE, exstyle);
	}
	ShowWindow (child, SW_HIDE);
	SetParent (child, m->FrameWindow);
	MoveWindow (child, -100, -100, 0, 0, 0);
	Rearrange_dock (m);
	ShowWindow (child, SW_SHOWNORMAL);

	menu = GetSystemMenu (child, FALSE);
	InsertMenu (menu, SC_DOCK, MF_STRING | MF_BYCOMMAND, SC_UNDOCK, "&Undock");
	RemoveMenu (menu, SC_FREE, MF_BYCOMMAND);
	RemoveMenu (menu, SC_DOCK, MF_BYCOMMAND);

	mdi_activate_child (m, child);
}

void	undock_child (MDIDATA * m, HWND child)
{
	HMENU menu;
	DWORD style, exstyle;
	RECT rc, crc;
	DOCKWIN_INFO * d;
	int dir;
	
	SetActiveWindow (m->FrameWindow);
	GetWindowRect (child, &rc);
	ShowWindow (child, SW_HIDE);

	style = GetWindowLong (child, GWL_STYLE);
	style |= WS_MAXIMIZEBOX | WS_MINIMIZEBOX | WS_THICKFRAME;
	SetWindowLong (child, GWL_STYLE, style);
	exstyle = GetWindowLong (child, GWL_EXSTYLE);
	exstyle &= ~WS_EX_TOOLWINDOW;
	SetWindowLong (child, GWL_EXSTYLE, exstyle);

	d = find_dock_info (m, child);
	dir = d ? d->dock_dir : DOCK_DOWN;
	if (d)
		delete_dock_info (m, d);
	SetParent (child, m->ClientWindow);

	menu = GetSystemMenu (child, FALSE);
	InsertMenu (menu, SC_UNDOCK, MF_STRING | MF_BYCOMMAND, SC_DOCK, "&Dock");
	InsertMenu (menu, SC_UNDOCK, MF_STRING | MF_BYCOMMAND, SC_FREE, "&Free");
	RemoveMenu (menu, SC_UNDOCK, MF_BYCOMMAND);
	Rearrange_dock (m);

	GetWindowRect (child, &rc);
	
	GetClientRect (m->ClientWindow, &crc);
	switch (dir)	{
	case DOCK_LEFT:	 crc.right  = rc.right-rc.left; break;
	case DOCK_RIGHT: crc.left   = crc.right - (rc.right-rc.left); break;
	case DOCK_UP:	 crc.bottom = rc.bottom-rc.top; break;
	case DOCK_DOWN:	 crc.top    = crc.bottom - (rc.bottom-rc.top); break;
	}
	MoveWindow (child, -100, -100, 0, 0, 0);	
	MoveRectZ (child, &crc);

	if (m->mdi_maximized_child)
		activate_child (m, m->mdi_maximized_child);
	++ m -> move_lock_count;
	ShowWindow (child, SW_SHOWNORMAL);
	-- m->move_lock_count;
	
	activate_child (m, child);
}

void	set_small_dock (MDIDATA * m, BOOL sm)
{
	DOCKWIN_INFO * d;
	DWORD exstyle;
	RECT rc;

	if (m->dock_small == sm)  return;
	m->dock_small = sm;
	for (d = m->dock_list; d; d = d->next)	{
		GetWindowRect (d->hwnd, &rc);
		screen_to_client_rect (m->FrameWindow, &rc);
		exstyle = GetWindowLong (d->hwnd, GWL_EXSTYLE);
		if (sm)
			exstyle |= WS_EX_TOOLWINDOW;
		else
			exstyle &= ~WS_EX_TOOLWINDOW;
		MoveWindow (d->hwnd, -100, -100, 0, 0, 0);
		SetWindowLong (d->hwnd, GWL_EXSTYLE, exstyle);
		MoveRect (d->hwnd, &rc);
	}
	Rearrange_dock (m);
}

/* ------------------------- Free children control --------------- */

void	free_child (MDIDATA * m, HWND child)
{
	HMENU menu;
	WINDOWPLACEMENT wpl;
	DWORD style;

	wpl.length = sizeof (wpl);
	GetWindowPlacement (child, &wpl);
 	client_to_screen_rect (GetParent (child), &wpl.rcNormalPosition);

	mdi_activate_child (m, NULL);

	style = GetWindowLong (child, GWL_STYLE);
	SetWindowLong (child, GWL_STYLE, style & ~WS_CHILD);

	ShowWindow (child, SW_HIDE);
	wpl.showCmd = SW_HIDE;
	SetWindowPlacement (child, &wpl);
	SetParent (child, 0);
	SetWindowPlacement (child, &wpl);
	menu = GetSystemMenu (child, FALSE);
	InsertMenu (menu, SC_FREE, MF_STRING | MF_BYCOMMAND, SC_MDI, "&Return to Frame");
	RemoveMenu (menu, SC_FREE, MF_BYCOMMAND);
	RemoveMenu (menu, SC_DOCK, MF_BYCOMMAND);
	AppendMenu (menu, MF_STRING, SC_MAIN, "&Frame window\tAlt+F6");
	if (!m->mdi_maximized_child)
		SendMessage (m->ClientWindow, WM_MYMDI_SIZE, SIZE_RESTORED, 0);
	ShowWindow (child, SW_SHOWNORMAL);
	activate_child (m, child);
}

void	return_child_to_mdi (MDIDATA * m, HWND child)
{
	HMENU menu;
	WINDOWPLACEMENT wpl;
	DWORD style;
	RECT rc;
	int w, h, showcmd;
	
	SetActiveWindow (m->FrameWindow);
	wpl.length = sizeof (wpl);
	GetWindowPlacement (child, &wpl);
 	screen_to_client_rect (m->ClientWindow, &wpl.rcNormalPosition);
	GetClientRect (m->ClientWindow, &rc);
	if (wpl.rcNormalPosition.top < 0 || wpl.rcNormalPosition.right < 0 ||
	    wpl.rcNormalPosition.top >= rc.bottom - GetSystemMetrics (SM_CYCAPTION) ||
	    wpl.rcNormalPosition.left >= rc.right)
	{
		convert_coordinates (m, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
			     &wpl.rcNormalPosition.left,
			     &wpl.rcNormalPosition.top,
			     &w, &h);
		wpl.rcNormalPosition.right  = wpl.rcNormalPosition.left + w;
		wpl.rcNormalPosition.bottom = wpl.rcNormalPosition.top + h;
	}

	style = GetWindowLong (child, GWL_STYLE);
	SetWindowLong (child, GWL_STYLE, style | WS_CHILD);

	showcmd = wpl.showCmd;
	ShowWindow (child, SW_HIDE);
	if (m->active_child == child)
		m->active_child = 0;
	wpl.showCmd = SW_HIDE;
	SetWindowPlacement (child, &wpl);
	SetParent (child, m->ClientWindow);
	SetWindowPlacement (child, &wpl);
	SetMenu (child, 0);
	menu = GetSystemMenu (child, FALSE);
	InsertMenu (menu, SC_MDI, MF_STRING | MF_BYCOMMAND, SC_DOCK, "&Dock");
	InsertMenu (menu, SC_MDI, MF_STRING | MF_BYCOMMAND, SC_FREE, "&Free");
	RemoveMenu (menu, SC_MDI, MF_BYCOMMAND);
	RemoveMenu (menu, SC_MAIN, MF_BYCOMMAND);	
	if (m->mdi_maximized_child)
		ShowWindow (child, SW_MAXIMIZE);
	else
		ShowWindow (child, SW_SHOWNORMAL);
	
	activate_child (m, child);
	test_draw_scrollbars (m, FALSE);
}

/* ------------------------- Plain MDI control ------------------- */

BOOL	get_child_placement (MDIDATA * m, HWND hwnd, MDIPLACEMENT * p)
{
	int n;
	DOCKWIN_INFO * d, * dd;
	memset (p, 0, sizeof (MDIPLACEMENT));
	if (!m) return FALSE;
	n = find_our_window (m, hwnd);
	if (n < 0) return FALSE;
	
	p->p.length = sizeof (WINDOWPLACEMENT);
	GetWindowPlacement (hwnd, &p->p);
	p->winnum = n;
	p->type = FREE_WINDOW   (hwnd) ? FREE :
		  DOCKED_WINDOW (hwnd) ? DOCKED :
		  FRAME;
	if (p -> type == DOCKED)	{
		d = find_dock_info (m, hwnd);
		if (!d)	{
			p -> dir = DOCK_DOWN;
			p -> docknum = 1000;
		} else	{
			p -> docksize = d -> dock_size;
			p -> dir = d -> dock_dir;
			p -> docknum = 0;
			for (dd = m->dock_list; dd && dd != d; dd = dd -> next)
				++ p -> docknum;
		}
	}
	return TRUE;
}

BOOL	set_child_placement (MDIDATA * m, HWND hwnd, MDIPLACEMENT * p)
{
	if (!m) return FALSE;
	return TRUE;
}


LRESULT MyDefMDIChildProc (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
	HWND child;
	RECT rc;
	LRESULT lresult;
	MDIDATA * m = (MDIDATA*) GetProp (hwnd, MAKEINTATOM (mdi_prop_atom));
	char s [1000];

	switch (msg) {
	case WM_DESTROY:
		if (FREE_WINDOW (hwnd))
			SendMessage (m->ClientWindow, WM_PARENTNOTIFY, WM_DESTROY, (LPARAM) hwnd);
		break;
	case WM_MOVE:
		if (MDI_WINDOW (hwnd)) SendMessage (m->ClientWindow, WM_MYMDI_MOVE, wparam, lparam);
		break;
	case WM_SIZE:
		if ((GetWindowLong (hwnd, GWL_STYLE) & WS_MDI_NOCLIENTADJUST) == 0)	{
			child = GetWindow (hwnd, GW_CHILD);
			if (child)	{
				GetClientRect (hwnd, &rc);
				MoveRect (child, &rc);
				InvalidateRect (child, 0, 0);
			}
		}
		if (MDI_WINDOW (hwnd)) SendMessage (m->ClientWindow, WM_MYMDI_SIZE, wparam, (LPARAM) hwnd);
		break;
	case WM_SETFOCUS:
		if ((GetWindowLong (hwnd, GWL_STYLE) & WS_MDI_NOCLIENTADJUST) == 0)	{
			child = GetWindow (hwnd, GW_CHILD);
			if (child) SetFocus (child);
		}
		break;
	case WM_MYMDI_GETPLACEMENT:
		return get_child_placement (m, hwnd, (MDIPLACEMENT *) lparam);
	case WM_MYMDI_SETPLACEMENT:
		return set_child_placement (m, hwnd, (MDIPLACEMENT *) lparam);
	case WM_COMMAND:
		if (CONTROL_ID >= 0xF000)
			SendMessage (hwnd, WM_SYSCOMMAND, wparam, lparam);
		return 0;
	case WM_SYSCOMMAND:
		if (!m) break;
		switch (CONTROL_ID)	{
		case SC_MOVE:
			if (DOCKED_WINDOW (hwnd))
				return 0;
			break;
		case SC_CLOSE:
		case SC_NEXTWINDOW:
		case SC_PREVWINDOW:
		case SC_DOCK:
		case SC_UNDOCK:
		case SC_FREE:
		case SC_MDI:
		case SC_MAIN:
			child_command (m, CONTROL_ID, hwnd); 
			return 0;
		}
		break;
	case WM_ACTIVATE:
		if (FREE_WINDOW (hwnd))	{
			if (LOWORD (wparam) == WA_INACTIVE)
				draw_window_label (m, hwnd);
			else
				activate_child (m, hwnd);
		}
		break;
	case WM_SETTEXT:
		if (!m) break;
		if (MDI_WINDOW (hwnd)) SendMessage (m->ClientWindow, WM_MYMDI_TEXT, wparam, lparam);
		lresult = DefWindowProc (hwnd, msg, wparam, lparam);
		draw_window_label (m, hwnd);
		return lresult;
		break;
	case WM_NCLBUTTONDOWN:
		if (! DOCKED_WINDOW (hwnd)) break;
		if (wparam != HTCAPTION) break;
		docked_child_begin_drag (m, hwnd);
		return 0;
	case WM_MOUSEMOVE:
		if (! DOCKED_WINDOW (hwnd)) break;
		if (GetCapture () != hwnd) break;
		docked_child_in_drag (m, hwnd, MAKEPOINTS (lparam));
		return 0;
	case WM_LBUTTONUP:
		if (! DOCKED_WINDOW (hwnd)) break;
		if (GetCapture () != hwnd) break;
		docked_child_stop_drag (m, hwnd);
		return 0;
	case WM_NCPAINT:
		if (m->ncpaint_lock_count) return 0;
		lresult = DefWindowProc (hwnd, msg, wparam, lparam);
		draw_window_label (m, hwnd);
		return lresult;
	case WM_MYMDI_GETLONGTEXT:
	case WM_MYMDI_GETSHORTTEXT:
		msg = WM_GETTEXT;
		break;
	case WM_MYMDI_TEXTCHANGED:
		SendMessage (hwnd, IsIconic (hwnd) ? WM_MYMDI_GETSHORTTEXT : WM_MYMDI_GETLONGTEXT,
				sizeof (s), (LPARAM)s);
		s [sizeof (s)-1] = 0;
		SetWindowText (hwnd, s);
		invalidate_tab (m, hwnd);
		break;
	}
	return DefWindowProc (hwnd, msg, wparam, lparam);
}

/*----------------------------------------------------------------*/

void	Invalidate_window_label (HWND hwnd)
{
	char s [1000];
	if (!hwnd) return;
	GetWindowText (hwnd, s, sizeof (s));
	SetWindowText (hwnd, s);
}

void	draw_label (HWND hwnd, HFONT font, int n, int nbuttons, int y, int dx, int dy)
{
	HDC hdc;
	int x;
	RECT rc;
	HPEN textpen, open;
	HFONT ofont;
	SIZE sz;
	char s [10];

	GetWindowRect (hwnd, &rc);
	x = rc.right - rc.left - GetSystemMetrics (SM_CXFRAME) - (nbuttons+1)*dx;

	rc.left = x; rc.right = x+dx;
	rc.top = y; rc.bottom = y+dy;

	wsprintf (s, "%d", n+1);

	textpen = CreatePen (PS_SOLID, 1, GetSysColor (COLOR_BTNTEXT));

	hdc = GetWindowDC (hwnd);

	if (font) ofont = SelectObject (hdc, font);
	GetTextExtentPoint32 (hdc, s, strlen(s), &sz);
	if (rc.right-rc.left-4<sz.cx) rc.left = rc.right-sz.cx-4;

	open = SelectObject (hdc, textpen);
	SelectObject (hdc, GetStockObject (NULL_BRUSH));
	Rectangle (hdc, rc.left, rc.top, rc.right, rc.bottom);

	rc.left ++; rc.right--; rc.top ++; rc.bottom --;
	SetBkColor (hdc, GetSysColor (COLOR_3DFACE));
	SetTextColor (hdc, GetSysColor (COLOR_BTNTEXT));

	ExtTextOut (hdc,
		    rc.left + (rc.right-rc.left-sz.cx)/2,
		    rc.top  + (rc.bottom-rc.top-sz.cy)/2,
		    ETO_OPAQUE | ETO_CLIPPED, &rc, s, strlen(s), 0);

	SelectObject (hdc, open);
	if (font) SelectObject (hdc, ofont);
	ReleaseDC (hwnd, hdc);
	DeleteObject (textpen);
}

void	draw_window_label (MDIDATA * m, HWND hwnd)
{
	int nbuttons;
	int n, y, dx, dy;
	DWORD style, exstyle;
	HFONT font;

	if (!m || !(m->number_flags & MDINUMBER_DRAWNUMBERS)) return;
	if (NT35 && IsIconic (hwnd)) return;

	n = find_our_window  (m, hwnd);
	if (n < 0) return;

	style   = GetWindowLong (hwnd, GWL_STYLE);
	exstyle = GetWindowLong (hwnd, GWL_EXSTYLE);
	if ((style & WS_SYSMENU) == 0)
		nbuttons = 0;
	else	{
		nbuttons = ((style & WS_MAXIMIZEBOX) != 0) +
			   ((style & WS_MINIMIZEBOX) != 0);
		if (!NT35)
			nbuttons = nbuttons ? 3 : 1;
	}

	if (NT35)
		if (style & WS_THICKFRAME)
			y = GetSystemMetrics (SM_CYFRAME);
		else if (style & WS_BORDER)
			y = GetSystemMetrics (SM_CYBORDER);
		else y = 0;
	else
		if (style & WS_THICKFRAME)
			y = GetSystemMetrics (SM_CYSIZEFRAME);
		else if (style & WS_BORDER)
			y = GetSystemMetrics (SM_CYFIXEDFRAME);
		else y = 0;

	if (exstyle & WS_EX_TOOLWINDOW)	{
		font = caption_small_font;
		dx = GetSystemMetrics (SM_CXSMSIZE);
		dy = GetSystemMetrics (SM_CYSMSIZE);
	} else	{
		font = caption_font;
		dx = GetSystemMetrics (SM_CXSIZE);
		dy = GetSystemMetrics (SM_CYSIZE);
	}
	draw_label (hwnd, caption_font, n, nbuttons,
		    y, dx, dy);
}

void	draw_frame_window_label (MDIDATA * m, HWND hwnd)
{
	int n, nbuttons;
	int y, dx, dy;
	RECT rc, rcw;

	if (!m || !(m->number_flags & MDINUMBER_DRAWNUMBERS)) return;
	if (! m -> mdi_maximized_child) return;

	n = find_our_window  (m, m -> mdi_maximized_child);
	if (n < 0) return;

	nbuttons = NT35 ? 2 : 3;
	GetClientRect (hwnd, &rc);
	GetWindowRect (hwnd, &rcw);
	client_to_screen_rect (hwnd, &rc);
	screen_to_client_rect (GetParent (hwnd), &rc);
	//	y = GetSystemMetrics (SM_CYFRAME) + GetSystemMetrics (SM_CYCAPTION);
	dx = GetSystemMetrics (NT35 ? SM_CXSIZE : SM_CXMENUSIZE);
	dy = GetSystemMetrics (NT35 ? SM_CYMENU : SM_CYMENUSIZE);
	y = rc.top - rcw.top - dy;
	draw_label (hwnd, menu_font, n, nbuttons, y, dx, dy);
}		

void	redraw_all_numbers (MDIDATA * m)
{
	int i;
	for (i = 0; i < m->win_cnt; i++)
		Invalidate_window_label (m->win_list [i]);
	DrawMenuBar (m->FrameWindow);
}

void	record_created (MDIDATA * m, HWND hwnd, int winnum)
{
	if (winnum)	{
		-- winnum;
		if (winnum > m->win_cnt)	{
			if (winnum >= MAX_NWINS) return;
			while (m->win_cnt < winnum)
				m->win_list [m->win_cnt++] = 0;
			m->win_list [winnum] = hwnd;
			m->win_cnt++;
		} else	{
			while (winnum < m->win_cnt && m->win_list [winnum]) ++ winnum;
			if (winnum >= MAX_NWINS) return;
			m->win_list [winnum] = hwnd;
			if (winnum == m->win_cnt) ++ m->win_cnt;
		}
	} else if (m->number_flags & MDINUMBER_REVERSE)	{
		if (m->win_cnt >= MAX_NWINS) return;
		memmove (m->win_list+1, m->win_list, m->win_cnt * sizeof (m->win_list[0]));
		m->win_list[0] = hwnd;
		++ m->win_cnt;
		redraw_all_numbers (m);
		invalidate_tab (m, 0);
	} else	{
		winnum = m->new_window_num;
		while (winnum < m->win_cnt && m->win_list [winnum]) ++ winnum;
		if (winnum >= MAX_NWINS) return;
		m->win_list [winnum] = hwnd;
		if (winnum >= m->win_cnt) ++ m->win_cnt;
		m->new_window_num = 0;
	}
	add_to_tab (m, hwnd);
}

void	record_destroyed (MDIDATA * m, HWND hwnd)
{
	int i, n;
	i = find_our_window (m, hwnd);
	if (i < 0) return;
	n = tab_number (m, hwnd);
	m->win_list [i] = NULL;
	delete_from_tab (m, n);
	if (m->number_flags & MDINUMBER_FIXEDNUMBERS)	{
		m->win_list [i] = NULL;
		if (i == m->win_cnt-1 && i >= 9)
			-- m->win_cnt;
		return;
	}
	-- m->win_cnt;
	for (; i < m->win_cnt; i++)	{
		m->win_list [i] = m->win_list [i+1];
		Invalidate_window_label (m->win_list [i]);
	}
}

void	record_activated (MDIDATA * m, HWND hwnd, HWND oldwnd)
{
	int i, j;

	if (!hwnd) return;
	i = find_our_window (m, hwnd);
	if (m -> number_flags &	MDINUMBER_ACTIVEFIRST)	{
		memmove (m->win_list+1, m->win_list, i*sizeof (m->win_list[0]));
		m->win_list[0] = hwnd;
		for (j = 1; j <= i; j++)
			Invalidate_window_label (m->win_list [j]);
		invalidate_tab (m, 0);
	} else if (! (m -> number_flags & MDINUMBER_NOFLIP) && i >= 9)	{
		m->win_list [i] = m->win_list [8];
		m->win_list [8] = hwnd;
		if (m->win_list [i] != oldwnd)
			Invalidate_window_label (m->win_list [i]);
		invalidate_tab (m, m->win_list [i]);
		invalidate_tab (m, hwnd);
	}
	if (oldwnd) Invalidate_window_label (oldwnd);
	Invalidate_window_label (hwnd);
	if (m->mdi_maximized_child)
		DrawMenuBar (m->FrameWindow);
}

void	compact_numbers (MDIDATA * m)
{
	int i, j;
	if (m->number_flags & MDINUMBER_FIXEDNUMBERS)
		return;
	i = 0;
	for (j = 0; j < m->win_cnt; j++)
		if (m->win_list [j])
			m->win_list [i++] = m->win_list [j];
	m->win_cnt = i;
	if (i != j)
		redraw_all_numbers (m);
	invalidate_tab (m, 0);
}

HWND	next_window (MDIDATA * m, HWND hwnd)
{
	int i, i0;

	i0 = find_our_window (m, hwnd);

	for (i = i0+1; i < m->win_cnt; i++)
		if (m->win_list [i]) return m->win_list [i];
	for (i = 0; i < i0; i++)
		if (m->win_list [i]) return m->win_list [i];
	return NULL;
}

HWND	prev_window (MDIDATA * m, HWND hwnd)
{
	int i, i0;

	i0 = find_our_window (m, hwnd);

	for (i = i0-1; i >= 0; i--)
		if (m->win_list [i]) return m->win_list [i];
	for (i = m->win_cnt-1; i > i0; i--)
		if (m->win_list [i]) return m->win_list [i];
	return NULL;
}

void	set_maximized_title (MDIDATA * m, HWND child, char * text)
{
	char s [1100];
	char t [800];
	if (! m->title_set)	{
		GetWindowText (m->FrameWindow, m->title_save, sizeof (m->title_save));
		m->title_save [sizeof (m->title_save)-1] - 0;
		m->title_set = TRUE;
	}
	strcpy (s, m->title_save);
	if (child)	{
		strcat (s, " - [");
		if (text)
			strncpy (t, text, sizeof (t));
		else
			GetWindowText (child, t, sizeof (t));
		t [sizeof (t)-1] = 0;
		strcat  (s, t);
		strcat (s, "]");
	}
	++ m->set_text_lock_count;
	SetWindowText (m->FrameWindow, s);
	-- m->set_text_lock_count;
}

void	make_menu_buttons (MDIDATA * m, HWND child)
{
	HMENU menu, sysmenu;

	menu = GetMenu (m->FrameWindow);
	if (m->cur_button_child)	{
		RemoveMenu (menu, 0, MF_BYPOSITION);
		RemoveMenu (menu, SC_MINIMIZE,  MF_BYCOMMAND);
		RemoveMenu (menu, SC_RESTORE,   MF_BYCOMMAND);
		RemoveMenu (menu, SC_CLOSE,    MF_BYCOMMAND);
	}
	m->cur_button_child = child;
	if (m->cur_button_child)	{
		sysmenu = GetSystemMenu (m->cur_button_child, FALSE);
		AppendMenu (menu, MF_BITMAP | MF_RIGHTJUSTIFY, SC_MINIMIZE,  (LPCSTR)(DWORD)3);
		AppendMenu (menu, MF_BITMAP | MF_RIGHTJUSTIFY, SC_RESTORE, (LPCSTR)(DWORD)2);
		if (! NT35) AppendMenu (menu, MF_BITMAP | MF_RIGHTJUSTIFY, SC_CLOSE,   (LPCSTR)(DWORD)5);
		InsertMenu (menu, 0, MF_BITMAP | MF_POPUP | MF_BYPOSITION, (UINT) sysmenu, (LPCSTR)(DWORD)1);
		EnableMenuItem (sysmenu, SC_MAXIMIZE, MF_GRAYED | MF_BYCOMMAND);
		EnableMenuItem (sysmenu, SC_SIZE, MF_GRAYED | MF_BYCOMMAND);
		EnableMenuItem (sysmenu, SC_MOVE, MF_GRAYED | MF_BYCOMMAND);
		EnableMenuItem (sysmenu, SC_RESTORE, MF_ENABLED | MF_BYCOMMAND);
		SetMenuDefaultItem (sysmenu, SC_CLOSE, FALSE);

	}
	DrawMenuBar (m->FrameWindow);
}

void	MyFlashWindow (MDIDATA * m, HWND hwnd)
{
	FlashWindow (hwnd, TRUE);
	if (NT35 && IsIconic (hwnd))	{
		ShowWindow (hwnd, SW_HIDE);
		ShowWindow (hwnd, SW_SHOWMINIMIZED);
	} else
		draw_window_label (m, hwnd);
}

void	mdi_activate_child (MDIDATA * m, HWND hwnd)
{
	RECT rc;
	if (m->mdi_active_child == hwnd) return;
	if (FREE_WINDOW (hwnd)) hwnd = NULL;
	if (!hwnd)
		hwnd = find_next_mdi (m, m->mdi_active_child);

	if (m->mdi_active_child)
		MyFlashWindow (m, m->mdi_active_child);

	if (m->mdi_maximized_child && MDI_WINDOW (hwnd) && hwnd != m->mdi_maximized_child)	{
		++ m->move_lock_count;
		++ m->ncpaint_lock_count;
		SendMessage (m->mdi_maximized_child, WM_SETREDRAW, 0, 0);
		SendMessage (hwnd,		     WM_SETREDRAW, 0, 0);
		ShowWindow  (m->mdi_maximized_child, SW_SHOWNORMAL);
		ShowWindow  (hwnd,		     SW_MAXIMIZE);
		GetClientRect (m->ClientWindow, &rc);
		AdjustWindowRect (&rc, GetWindowLong (hwnd, GWL_STYLE), 0);
		MoveRect (hwnd, &rc);
		SendMessage (m->mdi_maximized_child, WM_SETREDRAW, 1, 0);
		SendMessage (hwnd,		     WM_SETREDRAW, 1, 0);
		-- m->ncpaint_lock_count;
		-- m->move_lock_count;
		make_menu_buttons (m, hwnd);
		set_maximized_title (m, hwnd, 0);
		m->mdi_maximized_child = hwnd;
	}
	if (hwnd)	{
		SetWindowPos (hwnd, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE);
		MyFlashWindow (m, hwnd);
		if (DOCKED_WINDOW (hwnd))
			SetWindowPos (m->ClientWindow, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE);
	}
	m->mdi_active_child = hwnd;
	if (m->mdi_active_child) SetFocus (m->active_child);
}

void	activate_child (MDIDATA * m, HWND hwnd)
{
	int i;
	if (hwnd && FREE_WINDOW (hwnd))
		SetActiveWindow (hwnd);
	else
		SetActiveWindow (m->FrameWindow);
	if (m->active_child == hwnd)	{
		if (hwnd) SetFocus (hwnd);
		return;
	}
	if (!hwnd || ! FREE_WINDOW (hwnd))
		mdi_activate_child (m, hwnd);
	record_activated (m, hwnd, m->active_child);
	if (m->active_child) SendMessage (m->active_child, WM_MDIACTIVATE, (WPARAM) m->active_child, (LPARAM) hwnd);
	if (hwnd) SendMessage (hwnd, WM_MDIACTIVATE, (WPARAM) m->active_child, (LPARAM) hwnd);
	m->active_child = hwnd;
	if (m->active_child) SetFocus (m->active_child);
	if (m->Tab)	{
		i = tab_number (m, m->active_child);
		TabCtrl_SetCurSel (m->Tab, i);
	}
}

void	mdi_adjust_windows_menu (MDIDATA * m)
{
	int i;
	char s [2000];
	int n;
	
	if (! m->WindowsMenu) return;
	n = GetMenuItemCount (m->WindowsMenu);

	for (i = n-1; i >= 0 && GetMenuItemID (m->WindowsMenu, i) >= m->first_child; i--)
		DeleteMenu (m->WindowsMenu, i, MF_BYPOSITION);
	for (i = 0; i < m->win_cnt; i++)	{
		if (i < 9)
			wsprintf (s, "&%d ", i+1);
		else
			strcpy (s, " ");
		if (! m->win_list [i])
			strcat (s, "Empty");
		else	{
			GetWindowText (m->win_list [i], s+strlen (s), sizeof (s)-strlen (s));
			s [sizeof (s)-1] = 0;
		}
		AppendMenu (m->WindowsMenu, MF_STRING, m->first_child+i, s);
		if (m->win_list [i] && m->win_list [i] == m->active_child)	
			CheckMenuItem (m->WindowsMenu, m->first_child+i, MF_CHECKED | MF_BYCOMMAND);
	}
}

void	tile_windows (MDIDATA * m, int N, int c)
{
	RECT rc;
	int x, y, dx, dy, n, c1, i, j;
	HWND child;

	GetClientRect (m->ClientWindow, &rc);
	dx = rc.right / c;
	c1 = c - N % c;
	x = 0;
	child = GetWindow (m->ClientWindow, GW_CHILD);
	while (child && (IsIconic (child) || !OUR_WINDOW (child)))
		child = GetWindow (child, GW_HWNDNEXT);
	for (i = 0; i < c; i++)	{
		if (i == c-1) dx += rc.right % c;
		n = N/c;
		if (i >= c1) ++ n;
		y = 0;
		dy = rc.bottom / n;
		for (j = 0; j < n; j++)	{
			if (j == n-1) dy += rc.bottom % n;
			MoveWindow (child, x, y, dx, dy, 1);
			y += dy;
			do	{
				child = GetWindow (child, GW_HWNDNEXT);
			} while (child && (IsIconic (child) || !OUR_WINDOW (child)));
		}
		x += dx;
	}
}

void	test_draw_scrollbars (MDIDATA * m, BOOL move_client)
{
	HWND child;
	RECT rc, rcchild;
	POINT pt;
	int xmin, xmax, ymin, ymax, dx, dy;
	BOOL need_h, need_v;
	SCROLLINFO si;

	dx = GetSystemMetrics (SM_CXVSCROLL);
	dy = GetSystemMetrics (SM_CYHSCROLL);

	rc = m->ClientArea;
	++ m->move_lock_count;

	if (m->mdi_maximized_child)	{
		need_h = FALSE;
		need_v = FALSE;
	} else	{

		child = GetWindow (m->ClientWindow, GW_CHILD);

		xmin = ymin = 0;
		xmax = 0;
		ymax = 0;
		for (child = GetWindow (m->ClientWindow, GW_CHILD); child;
		     child = GetWindow (child, GW_HWNDNEXT))
		{
			if (!IsWindowVisible (child)) continue;
			if (m->Tab && IsIconic (child)) continue;
			if (!OUR_WINDOW (child)) continue;
			GetWindowRect (child, &rcchild);
			pt.x = rcchild.left; pt.y = rcchild.top;
			ScreenToClient (m->ClientWindow, &pt);
			if (pt.x < xmin) xmin = pt.x;
			if (pt.y < ymin) ymin = pt.y;
			pt.x = rcchild.right; pt.y = rcchild.bottom;
			ScreenToClient (m->ClientWindow, &pt);
			if (pt.x > xmax) xmax = pt.x;
			if (pt.y > ymax) ymax = pt.y;
		}

		need_h = need_v = 0;
		if (xmin < 0 || xmax > rc.right-rc.left)	{
			need_h = TRUE;
			rc.bottom -= dy;
		}
		if (ymin < 0 || ymax > rc.bottom-rc.top)	{
			need_v = TRUE;
			rc.right -= dx;
		}
		if (!need_h && xmax > rc.right-rc.left)	{
			need_h = TRUE;
			rc.bottom -= dy;
		}

		si.cbSize = sizeof (si);
		si.fMask = SIF_ALL;

		if (need_h)	{
			si.nPos  = -xmin;
			si.nMin  = 0;
			si.nMax  = max (xmax, rc.right-rc.left)-xmin-1;
			si.nPage = rc.right-rc.left;
			SetScrollInfo (m->hscroll, SB_CTL, &si, 1);
		}
		if (need_v)	{
			si.nPos  = -ymin;
			si.nMin  = 0;
			si.nMax  = max (ymax, rc.bottom-rc.top)-ymin-1;
			si.nPage = rc.bottom-rc.top;
			SetScrollInfo (m->vscroll, SB_CTL, &si, 1);
			memset (&si, 0, sizeof (si));
			si.cbSize = sizeof (si);
			si.fMask = SIF_ALL;
		}
	}
	if (move_client ||
	    IsWindowVisible (m->hscroll) != need_h ||
	    IsWindowVisible (m->vscroll) != need_v)
	{
		MoveRect (m->ClientWindow, &rc);
		if (need_h) MoveWindow (m->hscroll, rc.left, rc.bottom, rc.right-rc.left, dy, 1);
		if (need_v) MoveWindow (m->vscroll, rc.right, rc.top, dx, rc.bottom-rc.top, 1);
		if (need_h) InvalidateRect (m->hscroll, 0, 0);
		if (need_v) InvalidateRect (m->vscroll, 0, 0);
		ShowWindow (m->hscroll, need_h ? SW_NORMAL : SW_HIDE);
		ShowWindow (m->vscroll, need_v ? SW_NORMAL : SW_HIDE);
		set_min_positions (m);
	}
	-- m->move_lock_count;
}

void	scroll_process (MDIDATA * m, HWND scr, int code)
{
	HWND child;
	RECT rc;
	POINT pt;
	SCROLLINFO si;
	int pos, opos;

	si.cbSize = sizeof (si);
	si.fMask = SIF_ALL;
	GetScrollInfo (scr, SB_CTL, &si);
	opos = pos = si.nPos;

	if (! IsWindowVisible (scr)) return;
	switch (code)	{
	case SB_LINEDOWN:
		++pos; break;
	case SB_LINEUP:
		--pos; break;
	case SB_PAGEUP:
		pos -= si.nPage;
		break;
	case SB_PAGEDOWN:
		pos += si.nPage;
		break;
	case SB_THUMBTRACK:
	case SB_THUMBPOSITION:
		pos = si.nTrackPos;
		break;
	case SB_TOP:
		pos = si.nMin;
		break;
	case SB_BOTTOM:
		pos = si.nMax;
		break;
	}

	if (pos < si.nMin) pos = si.nMin;
	if (pos > (int) (si.nMax - si.nPage + 1)) pos = (int) (si.nMax - si.nPage + 1);
	if (pos == opos) return;
	SetScrollPos (scr, SB_CTL, pos, 1);

	child = GetWindow (m->ClientWindow, GW_CHILD);
	++ m->move_lock_count;
	for (child = GetWindow (m->ClientWindow, GW_CHILD); child;
	     child = GetWindow (child, GW_HWNDNEXT))
	{
		if (m->Tab && IsIconic (child)) continue;
		GetWindowRect (child, &rc);
		pt.x = rc.left; pt.y = rc.top;
		ScreenToClient (m->ClientWindow, &pt);
		if (scr == m->hscroll)
			SetWindowPos (child, 0, pt.x+opos-pos, pt.y, 0, 0, SWP_NOZORDER | SWP_NOSIZE);
		else
			SetWindowPos (child, 0, pt.x, pt.y+opos-pos, 0, 0, SWP_NOZORDER | SWP_NOSIZE);
	}
	-- m->move_lock_count;
	test_draw_scrollbars (m, FALSE);
}

void	new_child_created (MDIDATA * m, HWND hwnd, MDIPLACEMENT * placement)
{
	HMENU menu;
	int i;
	UINT id;
	DWORD save_number_style;
	char s [1000];

	SetProp (hwnd, MAKEINTATOM (mdi_prop_atom), (HANDLE) m);
	menu = GetSystemMenu (hwnd, FALSE);
	for (i = GetMenuItemCount (menu)-1; i >= 0; i--)	{
		id = GetMenuItemID (menu, i);
		RemoveMenu (menu, i, MF_BYPOSITION);
		if (id == SC_CLOSE) break;
	}
	if (MDI_WINDOW (hwnd))	{
		AppendMenu (menu, MF_STRING, SC_DOCK,  "&Dock");
		AppendMenu (menu, MF_STRING, SC_FREE,  "&Free");
	} else if (DOCKED_WINDOW (hwnd))
		AppendMenu (menu, MF_STRING, SC_UNDOCK, "&Undock");
	else if (FREE_WINDOW (hwnd))
		AppendMenu (menu, MF_STRING, SC_MDI, "&Return to Frame");
	AppendMenu (menu, MF_SEPARATOR, 0, 0);
	AppendMenu (menu, MF_STRING, SC_CLOSE, "&Close\tCtrl+F4");
	AppendMenu (menu, MF_SEPARATOR, 0, 0);
	AppendMenu (menu, MF_STRING, SC_NEXTWINDOW, "Nex&t\tCtrl+F6");
	AppendMenu (menu, MF_STRING, SC_PREVWINDOW, "Previou&s\tCtrl+Shift+F6");

	save_number_style = m -> number_flags;
	if (placement && placement -> winnum)	{
		m -> number_flags &=~MDINUMBER_ACTIVEFIRST;
		m -> number_flags |= MDINUMBER_NOFLIP;
	}
	record_created (m, hwnd, placement ? placement -> winnum : 0);
	activate_child (m, hwnd);
	m -> number_flags = save_number_style;
	SendMessage (hwnd, WM_MYMDI_GETLONGTEXT, sizeof (s), (LPARAM)s);
	s [sizeof (s)-1] = 0;
	SetWindowText (hwnd, s);
	if (placement && placement -> type != DOCKED)	{
		if (placement -> type == FRAME)	{
			if (m -> mdi_maximized_child)
				placement -> p.showCmd = SW_SHOWMAXIMIZED;
			else if (placement -> p.showCmd == SW_SHOWMAXIMIZED)
				placement -> p.showCmd = SW_SHOWNORMAL;
		}
		placement -> p.length = sizeof (WINDOWPLACEMENT);
		placement -> p.flags |= WPF_SETMINPOSITION;
		SetWindowPlacement (hwnd, &placement->p);
		test_draw_scrollbars (m, FALSE);
	} else if (m -> mdi_maximized_child && (!placement || placement->type == FRAME))
		ShowWindow (hwnd, SW_SHOWMAXIMIZED);
	else
		ShowWindow (hwnd, SW_SHOWNORMAL);
}

void	mdi_destroy_notify (MDIDATA * m, HWND hwnd)
{
	HWND wnd;

	if (!OUR_WINDOW (hwnd)) return;
	wnd = NULL;
	if (DOCKED_WINDOW (hwnd))	{
		delete_dock_info (m, find_dock_info (m, hwnd));
		Rearrange_dock (m);
	}

	if (hwnd == m->active_child)	{
		if (FREE_WINDOW (hwnd))	{
			wnd = m->mdi_active_child;
			if (! wnd)
				wnd = next_window (m, hwnd);
		} else	{
			wnd = find_next_mdi (m, hwnd);
			if (!wnd)	{
				mdi_activate_child (m, NULL);
				wnd = next_window (m, hwnd);
			}
		}
		activate_child (m, wnd);
	}

	record_destroyed (m, hwnd);
	if (m->mdi_maximized_child == hwnd)	{
		m->mdi_maximized_child = NULL;
		make_menu_buttons (m, NULL);
		set_maximized_title (m, NULL, NULL);
	}
	RemoveProp (hwnd, MAKEINTATOM (mdi_prop_atom));
	ShowWindow (hwnd, SW_HIDE);
	test_draw_scrollbars (m, FALSE);
}

LRESULT	CALLBACK MyMDIClientProc (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
	MDIDATA * m;
	HDC dc;
	PAINTSTRUCT ps;
	RECT rc;
	HWND child;
	WORD event;
	DWORD style;
	MDICREATESTRUCT * mdi;
	MDIPLACEMENT * p;
	int x, y, w, h, N, c;
	DOCKWIN_INFO * d;
	char s [200];
	int type;

	m = (MDIDATA*) GetWindowLong (hwnd, 0);

	switch (msg)	{
	case WM_CREATE:
		m = (MDIDATA*) GetProp (GetParent (hwnd), MAKEINTATOM (mdi_prop_atom));
		SetWindowLong (hwnd, 0, (LONG) m);
		return 0;

	case WM_PARENTNOTIFY:
		event = LOWORD (wparam);
		if (event == WM_DESTROY)	{
			child = (HWND) lparam;
			mdi_destroy_notify (m, child);
		}
		return 0;

	case WM_PAINT:
		GetClientRect (hwnd, &rc);
		dc = BeginPaint (hwnd, &ps);
		SendMessage (m->FrameWindow, WM_MYMDI_CLIENTPAINT, (WPARAM) dc, (LPARAM) &rc);
		EndPaint (hwnd, &ps);
		return 0;
	case WM_ERASEBKGND:
		GetClientRect (hwnd, &rc);
		return SendMessage (m->FrameWindow, WM_MYMDI_CLIENTERASE, wparam, (LPARAM) &rc);
	case WM_SIZE:
	case WM_WININICHANGE:
		++ m->move_lock_count;
		if (m->mdi_maximized_child)	{
			GetClientRect (hwnd, &rc);
			AdjustWindowRect (&rc, GetWindowLong (m->mdi_maximized_child, GWL_STYLE), 0);
			MoveRect (m->mdi_maximized_child, &rc);
		}
		-- m->move_lock_count;
		break;
	case WM_MDICREATE:
		p   = (MDIPLACEMENT*) wparam;
		mdi = (MDICREATESTRUCT*) lparam;
		type = p ? p -> type : FRAME;
		if (type == FRAME)
			convert_coordinates (m, mdi->x, mdi->y, mdi->cx, mdi->cy,
					     &x, &y, &w, &h);
		style = mdi->style | WS_CLIPCHILDREN | WS_CLIPSIBLINGS;
		if (type != FREE)
			style |= WS_CHILD;
		style &= ~WS_VISIBLE;
		if (type == DOCKED)	{
			style &= ~(WS_MAXIMIZEBOX | WS_MINIMIZEBOX | WS_THICKFRAME);
			d = malloc (sizeof (DOCKWIN_INFO));
			if (!d) return 0;
			child = CreateWindowEx (m -> dock_small ? WS_EX_TOOLWINDOW : 0,
						mdi->szClass, mdi->szTitle,
						style | WS_SYSMENU | WS_CAPTION,
						0, 0, 0, 0, m->FrameWindow, 0,
						(HINSTANCE) GetWindowLong (m->FrameWindow, GWL_HINSTANCE),
						NULL);
			if (!child)	{
				free (d);
				return 0;
			}
			d -> hwnd = child;
			d -> dock_dir = p -> dir;
			d -> dock_size = p -> docksize;
			put_into_dock_list (m, d, p->docknum);
			Rearrange_dock (m);
		} else	{
			child = CreateWindow (mdi->szClass, mdi->szTitle,
					     style,
					     x, y, w, h,
					     type == FREE ? 0 : m->ClientWindow,
					     0,
					     (HANDLE) GetWindowLong (m->ClientWindow, GWL_HINSTANCE),
					     mdi);

			if (!child) return 0;
		}
		new_child_created (m, child, p);
		UpdateWindow (child);
		return (LRESULT) child;
	case WM_MDICASCADE:
		m->dflt_x = 0;
		child = GetWindow (hwnd, GW_CHILD);
		if (! child) return 1;
		child = GetWindow (child, GW_HWNDLAST);
		while (child)	{
			if (! IsIconic (child) && OUR_WINDOW (child))	{
				convert_coordinates (m, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
						     &x, &y, &w, &h);
				if (IsZoomed (child)) ShowWindow (child, SW_NORMAL);
				MoveWindow (child, x, y, w, h, 1);
			}
			child = GetWindow (child, GW_HWNDPREV);
		}
		return 1;
	case WM_MDITILE:
		child = GetWindow (hwnd, GW_CHILD);
		if (! child) return 1;
		N = 0;
		while (child)	{
			if (! IsIconic (child) && OUR_WINDOW (child))
				++ N;
			child = GetWindow (child, GW_HWNDNEXT);
		}
		for (c = 1; c * c < N; c++);
		if (c * c > N && (wparam == MDITILE_HORIZONTAL)) --c;
		tile_windows (m, N, c);
		return 1;
		break;
	case WM_MDIICONARRANGE:
		ArrangeIconicWindows (hwnd);
		return 1;
	case WM_MDIACTIVATE:
		activate_child (m, (HWND) wparam);
		if (lparam && IsIconic ((HWND) wparam))
			ShowWindow ((HWND) wparam, SW_RESTORE);
		break;
	case WM_MDIGETACTIVE:
		return (LRESULT) m->active_child;
	case WM_MYMDI_SIZE:
		if (m->move_lock_count) return 0;
		++ m->move_lock_count;
		child = (HWND) lparam;
		if (wparam == SIZE_MINIMIZED)	{
			SendMessage (child, WM_MYMDI_GETSHORTTEXT, sizeof (s), (LPARAM) s);
			s [sizeof (s)-1] = 0;
			SetWindowText (child, s);
		} else if (wparam == SIZE_MAXIMIZED || wparam == SIZE_RESTORED)	{
			SendMessage (child, WM_MYMDI_GETLONGTEXT, sizeof (s), (LPARAM) s);
			s [sizeof (s)-1] = 0;
			SetWindowText (child, s);
		}
		if (wparam == SIZE_MAXIMIZED)	{
			m->mdi_maximized_child = child;
			test_draw_scrollbars (m, FALSE);
			GetClientRect (hwnd, &rc);
			AdjustWindowRect (&rc, GetWindowLong (child, GWL_STYLE), 0);
			MoveRect (child, &rc);
			make_menu_buttons (m, child);
			set_maximized_title (m, child, NULL);
		} else	{
			if (m->mdi_maximized_child)	{
				m->mdi_maximized_child = FALSE;
				make_menu_buttons (m, NULL);
				set_maximized_title (m, NULL, NULL);
			}
			test_draw_scrollbars (m, FALSE);
		}
		-- m->move_lock_count;
		return 0;
	case WM_MYMDI_MOVE:
		if (m->move_lock_count) break;
		test_draw_scrollbars (m, FALSE);
		break;
	case WM_MYMDI_TEXT:
		if (m->mdi_maximized_child)
			set_maximized_title (m, m->mdi_maximized_child, (char*) lparam);
		return 0;
	}

	return DefWindowProc (hwnd, msg, wparam, lparam);
}


void	Frame_paint (MDIDATA * m)
{
	HDC dc;
	PAINTSTRUCT ps;
	DOCKWIN_INFO * d;
	RECT rc;
	HBRUSH br;
	HPEN light, dark;

	dc = BeginPaint (m->FrameWindow, &ps);
	br = CreateSolidBrush (GetSysColor (COLOR_3DFACE));

	if (m->dock_list)	{
		light =	CreatePen (PS_SOLID, 1, GetSysColor (COLOR_3DHIGHLIGHT));
		dark  = CreatePen (PS_SOLID, 1, GetSysColor (COLOR_3DSHADOW));

		for (d = m->dock_list; d; d = d -> next)	{
			rc = d -> border_rect;
			draw_raised_frame (dc, &rc, light, dark);
			FillRect (dc, &rc, br);
			ExcludeClipRect (dc, d -> border_rect.left, d -> border_rect.top,
					 d -> border_rect.right, d -> border_rect.bottom);
		}
		DeleteObject (light);
		DeleteObject (dark);
	}
	DeleteObject (br);
	br = CreateSolidBrush (GetSysColor (COLOR_APPWORKSPACE));
	GetClientRect (m->FrameWindow, &rc);
	FillRect (dc, &rc, br);
	DeleteObject (br);
	EndPaint (m->FrameWindow, &ps);
}

void	mdi_parent_lbuttondown (MDIDATA * m, int x, int y)
{
	POINT pt;
	HWND hwnd;

	pt.x = x; pt.y = y;
	hwnd = ChildWindowFromPoint (m->FrameWindow, pt);
	if (hwnd == m->FrameWindow) return;
	if (hwnd == m->ClientWindow)	{
		ClientToScreen (m->FrameWindow, &pt);
		ScreenToClient (m->ClientWindow, &pt);
		hwnd = ChildWindowFromPoint (m->ClientWindow, pt);
		if (hwnd == m->ClientWindow)	return;
	}
	if (OUR_WINDOW (hwnd)) activate_child (m, hwnd);
}

void	child_command (MDIDATA * m, int command, HWND child)
{
	switch (command)	{
	case SC_MINIMIZE:	ShowWindow (child, SW_MINIMIZE);	return;
	case SC_RESTORE:	ShowWindow (child, SW_SHOWNORMAL);	return;
	case SC_CLOSE:		SendMessage (child, WM_CLOSE, 0, 0);	return;
	case SC_NEXTWINDOW:	child = next_window (m, child);
				if (child)
					activate_child (m, child);
				return;
	case SC_PREVWINDOW:	child = prev_window (m, child);
				if (child)
					activate_child (m, child);
				return;
	case SC_FREE:		if (MDI_WINDOW (child))
					free_child (m, child);
				return;
	case SC_DOCK:		if (MDI_WINDOW (child))
					dock_child (m, child);
				return;
	case SC_UNDOCK:		if (DOCKED_WINDOW (child))
					undock_child (m, child);
				return;
	case SC_MDI:		if (FREE_WINDOW (child))
					return_child_to_mdi (m, child);
				return;
	case SC_MAIN:		SetActiveWindow (m->FrameWindow);
				return;
	}
}

int	enum_mdi_windows (MDIDATA * m, MDIENUMPROC pr)
{
	int i;
	int win_cnt_copy;
	HWND	win_list_copy [MAX_NWINS];

	if (!pr)
		return m->win_cnt;
	win_cnt_copy = m->win_cnt;
	memmove (win_list_copy, m->win_list, m->win_cnt * sizeof (m->win_list [0]));
	for (i = 0; i < win_cnt_copy; i++)
		if (! pr (i+1, win_list_copy[i],
			  FREE_WINDOW (win_list_copy[i]),
			  DOCKED_WINDOW (win_list_copy[i]),
			  m->active_child && win_list_copy[i] == m->active_child,
			  m->mdi_active_child && win_list_copy[i] == m->mdi_active_child))
				return i+1;
	return win_cnt_copy;
}

int	enum_zorder (MDIDATA * m, MDIENUMPROC pr)
{
	HWND hwnd;
	int n, cnt;
	DOCKWIN_INFO * d;

	cnt = 0;
	hwnd = GetWindow (m->ClientWindow, GW_CHILD);
	while (hwnd)	{
		n = find_our_window (m, hwnd);
		if (n >= 0)	{
			++ cnt;
			if (! pr (n+1, hwnd, FALSE, FALSE,
				  hwnd == m->active_child,
				  hwnd == m->mdi_active_child))
				return cnt;
		}
		hwnd = GetWindow (hwnd, GW_HWNDNEXT);
	}
	hwnd = GetWindow (m->FrameWindow, GW_HWNDFIRST);
	while (hwnd)	{
		n = find_our_window (m, hwnd);
		if (n >= 0)	{
			++ cnt;
			if (! pr (n+1, hwnd, TRUE, FALSE,
				  hwnd == m->active_child,
				  FALSE))
				return cnt;
		}
		hwnd = GetWindow (hwnd, GW_HWNDNEXT);
	}
	for (d = m->dock_list; d; d = d->next)	{
		n = find_our_window (m, d->hwnd);
		if (n >= 0)	{
			++ cnt;
			if (! pr (n+1, d->hwnd, FALSE, TRUE,
				  d->hwnd == m->active_child,
				  FALSE))
				return cnt;
		}
	}
	return cnt;
}

void	create_client (MDIDATA * m)
{
	WNDCLASSEX wc;

	if (m->ClientWindow) return;
	if (!client_class)	{
		wc.cbSize = sizeof (WNDCLASSEX);
		wc.style = 0;
		wc.lpfnWndProc = (LPVOID) MyMDIClientProc;
		wc.cbClsExtra = 0;
		wc.cbWndExtra = sizeof (MDIDATA*);
		wc.hInstance = (HINSTANCE) GetWindowLong (m->FrameWindow, GWL_HINSTANCE);
		wc.hIcon = 0;
		wc.hCursor = LoadCursor (NULL, IDC_ARROW);
		wc.hbrBackground = 0;
		wc.lpszMenuName = 0;
		wc.lpszClassName = "MyMDIClass";
		wc.hIconSm = 0;
		client_class = RegisterClassEx (&wc);
		if (!client_class) abort ();
	}
	m->hscroll = CreateWindowEx (WS_EX_NOPARENTNOTIFY,
			"SCROLLBAR",
			"",
			WS_CHILD | SBS_HORZ,
			0, 0, 0, GetSystemMetrics (SM_CYHSCROLL),
			m->FrameWindow,
			NULL,
			(HINSTANCE) GetWindowLong (m->FrameWindow, GWL_HINSTANCE),
			0
		);

	m->vscroll = CreateWindowEx (WS_EX_NOPARENTNOTIFY,
			"SCROLLBAR",
			"",
			WS_CHILD | SBS_VERT,
			0, 0, GetSystemMetrics (SM_CXVSCROLL), 0,
			m->FrameWindow,
			NULL,
			(HINSTANCE) GetWindowLong (m->FrameWindow, GWL_HINSTANCE),
			0
		);

	m->ClientWindow = CreateWindowEx (WS_EX_NOPARENTNOTIFY,
			"MyMDIClass",
			"",
			WS_VISIBLE|WS_CHILD|WS_CLIPCHILDREN|WS_CLIPSIBLINGS,
			0, 0, 0, 0,
			m->FrameWindow,
			NULL,
			(HINSTANCE) GetWindowLong (m->FrameWindow, GWL_HINSTANCE),
			0
		);
	if (! m->ClientWindow) abort ();
}

void	size_frame (MDIDATA * m, BOOL recalc_tabs)
{
	RECT rc, rctab;
	GetClientRect (m->FrameWindow, &m->FrameArea);
	SendMessage (m->FrameWindow, WM_MYMDI_RECT, 0, (LPARAM) &m->FrameArea);
	if (m -> Tab)	{
		GetWindowRect (m->Tab, &rctab);
		if (recalc_tabs || rctab.right-rctab.left != m->FrameArea.right - m->FrameArea.left)	{
			MoveWindow (m->Tab, m->FrameArea.left, m->FrameArea.bottom,
					m->FrameArea.right - m->FrameArea.left, 1, 0);
			set_tab_item_size (m);
		}
		rc = m->FrameArea;
		TabCtrl_AdjustRect (m->Tab, FALSE, &rc);
		if (m->tab_attr & MDITAB_TOP)	{
			MoveWindow (m->Tab, m->FrameArea.left, m->FrameArea.top,
				    m->FrameArea.right-m->FrameArea.left,
				    rc.top - m->FrameArea.top, 1);
			m->FrameArea.top += rc.top - m->FrameArea.top;
		} else	{
			m->FrameArea.bottom -= rc.top - m->FrameArea.top;
			MoveWindow (m->Tab, m->FrameArea.left, m->FrameArea.bottom,
				    m->FrameArea.right-m->FrameArea.left,
				    rc.top - m->FrameArea.top, 1);
		}
	}
	Rearrange_dock (m);
	set_min_positions (m);
	test_draw_scrollbars (m, TRUE);
}

LRESULT	MyDefMDIProc (HWND hwnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
	MDIDATA * m;
	MDICURSORS * c;
	HWND child;
	UINT num;
	int x, y, i;
	NONCLIENTMETRICS metrics;
	HBRUSH br;
	NMHDR * nm;

	if (! mdi_prop_atom)
		mdi_prop_atom = GlobalAddAtom ("MDI_property");

	m = (MDIDATA*) GetProp (hwnd, MAKEINTATOM (mdi_prop_atom));
	if (!m)	{
		m = malloc (sizeof (MDIDATA));
		memset (m, 0, sizeof (MDIDATA));
		m->FrameWindow = hwnd;
		SetProp (hwnd, MAKEINTATOM (mdi_prop_atom), (HANDLE) m);
		create_client (m);
		m->cur_drag_cursor = 
		m->cursors.normal_cursor =
		m->cursors.moving_cursor = LoadCursor (0, IDC_ARROW);
		m->cursors.hsize_cursor = LoadCursor (0, IDC_SIZEWE);
		m->cursors.vsize_cursor = LoadCursor (0, IDC_SIZENS);
		InitCommonControls ();
		m->ilist = ImageList_Create (10, 10, ILC_COLOR|ILC_MASK, 0, MAX_NICONS);
		SendMessage (hwnd, WM_WININICHANGE, 0, 0);
		m->dock_small = TRUE;
	}
	switch (msg)	{
	case WM_PARENTNOTIFY:
		switch (LOWORD (wparam))	{
		case WM_LBUTTONDOWN:
			x = LOWORD (lparam);
			y = HIWORD (lparam);
			mdi_parent_lbuttondown (m, x, y);
			break;
		case WM_DESTROY:
			child = (HWND) lparam;
			mdi_destroy_notify (m, child);
		}
		return 0;
	case WM_DESTROY:	RemoveProp (hwnd, MAKEINTATOM (mdi_prop_atom)); break;
	case WM_MDICREATE:
	case WM_MDICASCADE:
	case WM_MDITILE:
	case WM_MDIICONARRANGE:
	case WM_MDIACTIVATE:
	case WM_MDIGETACTIVE:
		return SendMessage (m->ClientWindow, msg, wparam, lparam);
	case WM_SETTEXT:
		if (! m->set_text_lock_count)	{
			strncpy (m->title_save, (char*) lparam, sizeof (m->title_save));
			m->title_save [sizeof (m->title_save)-1] = 0;
			if (m->mdi_maximized_child)	{
				set_maximized_title (m, m->mdi_maximized_child, NULL);
				return 0;
			}
		}
		break;
	case WM_SETFOCUS:
		if (m->mdi_active_child)
			SetFocus (m->mdi_active_child);
		break;
	case WM_INITMENU:
		mdi_adjust_windows_menu (m);
		break;
	case WM_SIZE:
		size_frame (m, FALSE);
		break;
	case WM_PAINT:
		Frame_paint (m);
		break;
	case WM_NCPAINT:
		DefWindowProc (hwnd, msg, wparam, lparam);
		draw_frame_window_label (m, hwnd);
		return 0;
	case WM_WININICHANGE:
		memset (&metrics, 0, sizeof (metrics));
		metrics.cbSize = sizeof (metrics);;
		SystemParametersInfo (SPI_GETNONCLIENTMETRICS, 0, &metrics, 0);
		if (caption_font) DeleteObject (caption_font);
		if (caption_bold_font) DeleteObject (caption_bold_font);
		if (caption_nonbold_font) DeleteObject (caption_nonbold_font);
		if (caption_small_font) DeleteObject (caption_small_font);
		if (menu_font) DeleteObject (menu_font);
		caption_font = CreateFontIndirect (&metrics.lfCaptionFont);
		metrics.lfCaptionFont.lfWeight = FW_BOLD;
		caption_bold_font    = CreateFontIndirect (&metrics.lfCaptionFont);
		metrics.lfCaptionFont.lfWeight = 0;
		caption_nonbold_font = CreateFontIndirect (&metrics.lfCaptionFont);		
		caption_small_font = CreateFontIndirect (&metrics.lfSmCaptionFont);		
		menu_font    = CreateFontIndirect (&metrics.lfMenuFont);
		calc_tab_size (m);
		SendMessage (m->ClientWindow, WM_WININICHANGE, 0, 0);
		break;
	case WM_MOUSEMOVE:
		test_drag (m, LOWORD (lparam), HIWORD (lparam));
		break;
	case WM_LBUTTONDOWN:
		if (m->drag_over_border)
			begin_drag (m, LOWORD (lparam), HIWORD (lparam));
		break;
	case WM_LBUTTONUP:
		ReleaseCapture ();
		break;
	case WM_SETCURSOR:
		if ((HWND) wparam == hwnd && LOWORD (lparam) == HTCLIENT)	{
			SetCursor (m->cur_drag_cursor);
			return 0;
		}
		break;
	case WM_NOTIFY:
		nm = (NMHDR*) lparam;
		if (nm->code == TCN_SELCHANGE && nm->hwndFrom == m->Tab)	{
			i = TabCtrl_GetCurSel (m->Tab);
			child = tab_window (m, i);
			if (!child) return 0;
			SendMessage (hwnd, WM_MDIACTIVATE, (WPARAM) child, 1);
			return 0;
		} else if (nm->code == NM_CLICK && nm->hwndFrom == m->Tab)	{
			i = TabCtrl_GetCurFocus(m->Tab);
			child = tab_window (m, i);
			if (!child) return 0;
			SendMessage (hwnd, WM_MDIACTIVATE, (WPARAM) child, 1);
			return 0;
		} else if (nm->code == NM_RCLICK && nm->hwndFrom == m->Tab)	{
			tab_system_menu (m);
			return 0;
		} else if (nm->code == TTN_NEEDTEXT && m->Tab && nm->hwndFrom == TabCtrl_GetToolTips (m->Tab))	{
			tab_need_text (m, nm);
			return 0;
		}
		break;
	case WM_MYMDI_CURSOR:
		c = (MDICURSORS *) lparam;
		if (c->normal_cursor) m->cursors.normal_cursor = c->normal_cursor;
		if (c->hsize_cursor) m->cursors.hsize_cursor = c->hsize_cursor;
		if (c->vsize_cursor) m->cursors.vsize_cursor = c->vsize_cursor;
		if (c->moving_cursor) m->cursors.moving_cursor = c->moving_cursor;
		return 0;
	case WM_MYMDI_TAB:
		set_tab_attr (m, (BOOL) wparam, lparam);
		return 0;
	case WM_MYMDI_DOCKSTYLE:
		set_small_dock (m, (BOOL) wparam);
		return 0;
	case WM_DRAWITEM:
		draw_tab_item (m, (DRAWITEMSTRUCT*) lparam);
		return 0;
	case WM_HSCROLL:
		scroll_process (m, m->hscroll, LOWORD (wparam));
		break;
	case WM_VSCROLL:
		scroll_process (m, m->vscroll, LOWORD (wparam));
		break;
	case WM_ACTIVATE:
		if (LOWORD (wparam) == WA_INACTIVE)
			test_kill_drag_rect (m);
		if (! m->mdi_active_child) break;
		MyFlashWindow (m, m->mdi_active_child);
		if (LOWORD (wparam) != WA_INACTIVE)	{
			activate_child (m, m->mdi_active_child);
			SetFocus (m->mdi_active_child);
		}
		break;
	case WM_MYMDI_CLIENTPAINT:
	case WM_MYMDI_CLIENTERASE:
		br = CreateSolidBrush (GetSysColor (COLOR_APPWORKSPACE));
		FillRect ((HDC) wparam, (RECT*) lparam, br);
		DeleteObject (br);
		return 1;
	case WM_MYMDI_MENU:
		m->WindowsMenu = (HMENU) wparam;
		m->first_child = (UINT) lparam;
		break;
	case WM_MYMDI_GETCLIENT:
		return (LRESULT) m->ClientWindow;
	case WM_MYMDI_NUMBERING:
		num = m->number_flags;
		m->number_flags = (UINT) wparam;
		if (m -> number_flags & MDINUMBER_FIXEDNUMBERS)
			while (m->win_cnt<9) m->win_list [m->win_cnt++] = NULL;
		else
			compact_numbers (m);
		if ((num^m->number_flags) & MDINUMBER_DRAWNUMBERS)
			redraw_all_numbers (m);
		break;
	case WM_MYMDI_GETNUMBERING:
		return (LRESULT) m -> number_flags;
	case WM_MYMDI_WINDOWBYNUM:
		i = (int) wparam;
		if (i <= 0 || i > m->win_cnt) return 0;
		return (LRESULT) m->win_list [i-1];
	case WM_MYMDI_ENUM:
		return (LRESULT) enum_mdi_windows (m, (MDIENUMPROC) lparam);
	case WM_MYMDI_ENUMZORDER:
		return (LRESULT) enum_zorder (m, (MDIENUMPROC) lparam);
	case WM_MYMDI_COMPACTNUMBERS:
		compact_numbers (m);
		return 0;
	case WM_MYMDI_DOCK:
		child = (HWND) wparam;
		if (MDI_WINDOW (child)) child_command (m, SC_DOCK, child);
		else if (DOCKED_WINDOW (child)) child_command (m, SC_UNDOCK, child);
		return 0;
	case WM_MYMDI_FREE:
		child = (HWND) wparam;
		if (MDI_WINDOW (child)) child_command (m, SC_FREE, child);
		else if (FREE_WINDOW (child)) child_command (m, SC_MDI, child);
		return 0;
	case WM_COMMAND:
		switch (CONTROL_ID)	{
		case SC_MINIMIZE:
		case SC_RESTORE:
		case SC_CLOSE:
		case SC_NEXTWINDOW:
		case SC_PREVWINDOW:
		case SC_FREE:
		case SC_DOCK:
		case SC_MDI:
		case SC_MAIN:		child_command (m, CONTROL_ID, m->mdi_maximized_child);
					return 0;
		}
		if (CONTROL_ID >= m->first_child && CONTROL_ID < m->first_child + m->win_cnt)	{
			child = m->win_list [CONTROL_ID - m->first_child];
			if (child)	{
				activate_child (m, child);
				if (IsIconic (child)) ShowWindow (child, SW_RESTORE);
			} else	{
				m->new_window_num = CONTROL_ID - m->first_child;
				SendMessage (hwnd, WM_MYMDI_ENTEREMPTY, (WPARAM) (m->new_window_num+1), 0);
				m->new_window_num = 0;
			}
		} else if (m->active_child)
			SendMessage (m->active_child, msg, wparam, lparam);
		break;
	}
	return DefWindowProc (hwnd, msg, wparam, lparam);
}

HACCEL	MyMDIAccel = 0;
int	altnumber = 0;

BOOL	TranslateMyMDISysAccel (HWND frame, MSG * msg)
{
	ACCEL accel [6];
	MDIDATA * m;
	int n;

	if (! MyMDIAccel)	{
		accel [0].fVirt = FVIRTKEY | FCONTROL;
		accel [0].key   = VK_F6;
		accel [0].cmd   = SC_NEXTWINDOW;

		accel [1].fVirt = FVIRTKEY | FCONTROL;
		accel [1].key   = VK_TAB;
		accel [1].cmd   = SC_NEXTWINDOW;

		accel [2].fVirt = FVIRTKEY | FCONTROL | FSHIFT;
		accel [2].key   = VK_F6;
		accel [2].cmd   = SC_PREVWINDOW;

		accel [3].fVirt = FVIRTKEY | FALT;
		accel [3].key   = VK_F6;
		accel [3].cmd   = SC_MAIN;

		accel [4].fVirt = FVIRTKEY | FCONTROL | FSHIFT;
		accel [4].key   = VK_TAB;
		accel [4].cmd   = SC_PREVWINDOW;

		accel [5].fVirt = FVIRTKEY | FCONTROL;
		accel [5].key   = VK_F4;
		accel [5].cmd   = SC_CLOSE;

		MyMDIAccel = CreateAcceleratorTable (accel, 6);
	}
	m = (MDIDATA*) GetProp (frame, MAKEINTATOM (mdi_prop_atom));

	if (msg->message == WM_KEYUP && msg->wParam == VK_MENU)
		if (altnumber && m->first_child)	{
			if (altnumber > m->win_cnt)
				MessageBeep (MB_ICONASTERISK);
			else
				PostMessage (m->FrameWindow, WM_COMMAND, m->first_child+altnumber-1, 0);
			altnumber = 0;
		}

	if (msg->message == WM_SYSKEYDOWN)	{
		msg->hwnd = m->FrameWindow;
		if (m->first_child && msg->wParam >= '0' && msg->wParam <= '9')	{
			n = altnumber * 10 + msg->wParam - '0';
			if (!n) return FALSE;
			if (n > m->win_cnt)
				MessageBeep (MB_ICONASTERISK);
			else if (n * 10 > m->win_cnt)	{
				PostMessage (m->FrameWindow, WM_COMMAND, m->first_child+n-1, 0);
				altnumber = 0;
				return TRUE;
			}
			altnumber = n;
			return TRUE;
		} else
			altnumber = 0;
	}

	if (m && MyMDIAccel && m->active_child)
		return TranslateAccelerator (m->active_child, MyMDIAccel, msg);
	return FALSE;
}
