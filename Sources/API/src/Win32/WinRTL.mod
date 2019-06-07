(*
  WinRTL.MOD
  Run-time support for Win32 native code compilers.

  Copyright (C) 1996-1998 XDS Ltd.

*)

<*-IOVERFLOW*>
<*-COVERFLOW*>
<*-CHECKINDEX*>
<*-CHECKRANGE*>
<*+M2EXTENSIONS*>

IMPLEMENTATION MODULE WinRTL;

<* IF BACKEND # "C" THEN *>

IMPORT SYSTEM;
IMPORT Windows;
FROM Windows IMPORT RESOURCESTRA, RESOURCESTRW, ATOMSTR, ATOMWSTR, ATOM,
                    BYTE, WORD, DWORD, HGLOBAL, POINT, BOOL, ULONG,
                    WPARAM, LPARAM, HWND, HMENU, HINSTANCE,
                    PBYTE, WS_SET, DLGPROC, DLGTEMPLATE, WCHAR, HICON,
                    GW_ENUM, HCURSOR, UINT, HLOCAL, LONG, SHORT, PSTR, PWSTR,
                    ROP, ROP4, COLORREF, POINTS, LANGID, LCID, PVOID, LPCVOID, HANDLE,
                    RECT, COORD, SMALL_RECT, PCSMALL_RECT, CHAR_INFO,
                    PCSTR, PCWSTR, CHAR_ATTRIBUTES_SET, CWP_SET,
                    SendMessageA, SendMessageW,
                    CDM_GETSPEC,
                    CDM_GETFILEPATH,
                    CDM_GETFOLDERPATH,
                    CDM_GETFOLDERIDLIST,
                    CDM_SETCONTROLTEXT,
                    CDM_HIDECONTROL,
                    CDM_SETDEFEXT,
                    OVERLAPPED, STATUS_PENDING,
                    SOCKET, fd_set, FD_SETSIZE, timeval, u_long, in_addr;

(* ------------------ General section ---------------------- *)

PROCEDURE ["StdCall"] MyInstance () : HINSTANCE;
BEGIN
    RETURN Windows.GetModuleHandle (NIL);
END MyInstance;

PROCEDURE ["StdCall"] MAKEINTRESOURCEA (x : INTEGER) : RESOURCESTRA;
BEGIN
      RETURN SYSTEM.CAST (RESOURCESTRA, SYSTEM.CAST (CARDINAL, x) MOD 10000H);
END MAKEINTRESOURCEA;

PROCEDURE ["StdCall"] MAKEINTRESOURCEW (x : INTEGER) : RESOURCESTRW;
BEGIN
      RETURN SYSTEM.CAST (RESOURCESTRW, SYSTEM.CAST (CARDINAL, x) MOD 10000H);
END MAKEINTRESOURCEW;

PROCEDURE ["StdCall"] MAKEINTATOMA (x : ATOM) : ATOMSTR;
BEGIN
    RETURN SYSTEM.CAST (ATOMSTR, VAL (UINT, x));
END MAKEINTATOMA;

PROCEDURE ["StdCall"] MAKEINTATOMW (x : ATOM) : ATOMWSTR;
BEGIN
    RETURN SYSTEM.CAST (ATOMWSTR, VAL (UINT, x));
END MAKEINTATOMW;

PROCEDURE ["StdCall"] GetPSTR (x : ARRAY OF CHAR) : PSTR;
BEGIN
    RETURN SYSTEM.CAST (PSTR, SYSTEM.ADR (x));
END GetPSTR;

PROCEDURE ["StdCall"] GetPWSTR (x : ARRAY OF WCHAR) : PWSTR;
BEGIN
    RETURN SYSTEM.CAST (PWSTR, SYSTEM.ADR (x));
END GetPWSTR;

PROCEDURE ["StdCall"] max (a, b : INTEGER) : INTEGER;
BEGIN
    IF a > b THEN RETURN a; ELSE RETURN b END;
END max;

PROCEDURE ["StdCall"] min (a, b : INTEGER) : INTEGER;
BEGIN
      IF a < b THEN RETURN a; ELSE RETURN b END;
END min;

PROCEDURE ["StdCall"] MAKEWORD (lo, hi : BYTE) : WORD;
BEGIN
    RETURN VAL (WORD, VAL (CARDINAL, hi) * 8 + VAL (CARDINAL, lo));
END MAKEWORD;

PROCEDURE ["StdCall"] MAKELONG (lo, hi : WORD) : DWORD;
BEGIN
    RETURN VAL (DWORD, VAL (CARDINAL, hi) * 65536 + VAL (CARDINAL, lo));
END MAKELONG;

PROCEDURE ["StdCall"] LOWORD (x : DWORD) : WORD;
BEGIN
    RETURN VAL (WORD, x MOD 65536);
END LOWORD;

PROCEDURE ["StdCall"] HIWORD (x : DWORD) : WORD;
BEGIN
    RETURN VAL (WORD, x DIV 65536);
END HIWORD;

PROCEDURE ["StdCall"] LOBYTE (x : WORD) : BYTE;
BEGIN
    RETURN VAL (BYTE, x MOD 256);
END LOBYTE;

PROCEDURE ["StdCall"] HIBYTE (x : WORD) : BYTE;
BEGIN
    RETURN VAL (BYTE, x DIV 256);
END HIBYTE;

(* ----------------- WinBase section ----------------------- *)

PROCEDURE ["StdCall"] GlobalDiscard (h : HGLOBAL) : HGLOBAL;
BEGIN
    RETURN Windows.GlobalReAlloc (h, 0, Windows.GMEM_MOVEABLE);
END GlobalDiscard;

PROCEDURE ["StdCall"] LocalDiscard (h : HLOCAL) : HLOCAL;
BEGIN
    RETURN Windows.LocalReAlloc (h, 0, Windows.LMEM_MOVEABLE);
END LocalDiscard;

PROCEDURE ["StdCall"] HasOverlappedIoCompleted (VAR Overlapped : OVERLAPPED) : BOOL;
BEGIN
    RETURN Overlapped.Internal # STATUS_PENDING;
END HasOverlappedIoCompleted;

(* ----------------- WinUser section ----------------------- *)

PROCEDURE ["StdCall"] ExitWindows (dwReserved : DWORD; Code : INTEGER) : BOOL;
BEGIN
    RETURN Windows.ExitWindowsEx (Windows.EWX_LOGOFF, 0FFFFFFFFh);
END ExitWindows;

PROCEDURE ["StdCall"] POINTSTOPOINT (VAR pt : POINT; pts : LPARAM);
BEGIN
    pt.x := VAL (LONG, SYSTEM.CAST (SHORT, LOWORD (pts)));
    pt.y := VAL (LONG, SYSTEM.CAST (SHORT, HIWORD (pts)));
END POINTSTOPOINT;

PROCEDURE ["StdCall"] POINTTOPOINTS (pt : POINT) : LPARAM;
BEGIN
    RETURN MAKELONG (SYSTEM.CAST (WORD, VAL (SHORT, pt.x)),
                     SYSTEM.CAST (WORD, VAL (SHORT, pt.y)));
END POINTTOPOINTS;

PROCEDURE ["StdCall"] CreateWindowA (ClassName  : ATOMSTR;
                         WindowName : ARRAY OF CHAR;
                         Style : WS_SET;
                         X, Y, Width, Height : INTEGER;
                         hWndParent : HWND;
                         hMenu : HMENU;
                         hInstance : HINSTANCE;
                         Param : PBYTE
                        )
                           : HWND;
BEGIN
    RETURN Windows.CreateWindowExA (Windows.WS_EX_SET {},
                                    ClassName, WindowName, Style,
                                    X, Y, Width, Height,
                                    hWndParent, hMenu, hInstance, Param);
END CreateWindowA;

PROCEDURE ["StdCall"] CreateWindowW (ClassName  : ATOMWSTR;
                         WindowName : ARRAY OF WCHAR;
                         Style : WS_SET;
                         X, Y, Width, Height : INTEGER;
                         hWndParent : HWND;
                         hMenu : HMENU;
                         hInstance : HINSTANCE;
                         Param : PBYTE
                        )
                           : HWND;
BEGIN
    RETURN Windows.CreateWindowExW (Windows.WS_EX_SET {},
                                    ClassName, WindowName, Style,
                                    X, Y, Width, Height,
                                    hWndParent, hMenu, hInstance, Param);
END CreateWindowW;

PROCEDURE ["StdCall"] CreateDialogA (hInstance : HINSTANCE;
                         TemplateName : RESOURCESTRA;
                         Parent : HWND;
                         lpDialogFunc : DLGPROC) : HWND;
BEGIN
    RETURN Windows.CreateDialogParamA (hInstance, TemplateName, Parent, lpDialogFunc, 0);
END CreateDialogA;

PROCEDURE ["StdCall"] CreateDialogW (hInstance : HINSTANCE;
                         TemplateName : RESOURCESTRW;
                         Parent : HWND;
                         lpDialogFunc : DLGPROC) : HWND;
BEGIN
    RETURN Windows.CreateDialogParamW (hInstance, TemplateName, Parent, lpDialogFunc, 0);
END CreateDialogW;

PROCEDURE ["StdCall"] CreateDialogIndirectA (hInstance : HINSTANCE;
                                 Template : DLGTEMPLATE;
                                 Parent : HWND;
                                 DialogFunc : DLGPROC) : HWND;
BEGIN
    RETURN Windows.CreateDialogIndirectParamA (hInstance, Template, Parent,
                                       DialogFunc, 0);
END CreateDialogIndirectA;

PROCEDURE ["StdCall"] CreateDialogIndirectW (hInstance : HINSTANCE;
                                 Template : DLGTEMPLATE;
                                 Parent : HWND;
                                 DialogFunc : DLGPROC) : HWND;
BEGIN
    RETURN Windows.CreateDialogIndirectParamW (hInstance, Template, Parent,
                                       DialogFunc, 0);
END CreateDialogIndirectW;

PROCEDURE ["StdCall"] DialogBoxA (hInstance : HINSTANCE;
                      TemplateName : RESOURCESTRA;
                      Parent : HWND;
                      lpDialogFunc : DLGPROC) : INTEGER;
BEGIN
    RETURN Windows.DialogBoxParamA (hInstance, TemplateName,
                            Parent, lpDialogFunc, 0);
END DialogBoxA;

PROCEDURE ["StdCall"] DialogBoxW (hInstance : HINSTANCE;
                      TemplateName : RESOURCESTRW;
                      Parent : HWND;
                      lpDialogFunc : DLGPROC) : INTEGER;
BEGIN
    RETURN Windows.DialogBoxParamW (hInstance, TemplateName,
                            Parent, lpDialogFunc, 0);
END DialogBoxW;

PROCEDURE ["StdCall"] DialogBoxIndirectA (hInstance : HINSTANCE;
                              Template-: DLGTEMPLATE;
                              Parent : HWND;
                              DialogFunc : DLGPROC) : INTEGER;
BEGIN
    RETURN Windows.DialogBoxIndirectParamA (hInstance, Template,
                                    Parent, DialogFunc, 0);
END DialogBoxIndirectA;

PROCEDURE ["StdCall"] DialogBoxIndirectW (hInstance : HINSTANCE;
                              Template-: DLGTEMPLATE;
                              Parent : HWND;
                              DialogFunc : DLGPROC) : INTEGER;
BEGIN
    RETURN Windows.DialogBoxIndirectParamW (hInstance, Template,
                                    Parent, DialogFunc, 0);
END DialogBoxIndirectW;

PROCEDURE ["StdCall"] / CharUpperA (s : DWORD) : DWORD;
PROCEDURE ["StdCall"] / CharUpperW (s : DWORD) : DWORD;
PROCEDURE ["StdCall"] / CharLowerA (s : DWORD) : DWORD;
PROCEDURE ["StdCall"] / CharLowerW (s : DWORD) : DWORD;

PROCEDURE ["StdCall"] CharUpper1A (s : CHAR) : CHAR;
BEGIN
    RETURN VAL (CHAR, CharUpperA (VAL (DWORD, s)));
END CharUpper1A;

PROCEDURE ["StdCall"] CharUpper1W (s : WCHAR) : WCHAR;
BEGIN
    RETURN VAL (WCHAR, CharUpperW (VAL (DWORD, s)));
END CharUpper1W;

PROCEDURE ["StdCall"] CharLower1A (s : CHAR)  : CHAR;
BEGIN
    RETURN VAL (CHAR, CharLowerA (VAL (DWORD, s)));
END CharLower1A;

PROCEDURE ["StdCall"] CharLower1W (s : WCHAR)  : WCHAR;
BEGIN
    RETURN VAL (WCHAR, CharLowerW (VAL (DWORD, s)));
END CharLower1W;

PROCEDURE ["StdCall"] / MenuItemFromPoint (hWnd : HWND; hMenu : HMENU; X, Y : LONG) : INTEGER;

PROCEDURE ["StdCall"] rtlMenuItemFromPoint (hWnd : HWND; hMenu : HMENU; pt : POINT) : INTEGER;
BEGIN
    RETURN MenuItemFromPoint (hWnd, hMenu, pt.x, pt.y);
END rtlMenuItemFromPoint;

PROCEDURE ["StdCall"] / DragDetect (hwnd : HWND; X, Y : LONG) : BOOL;

PROCEDURE ["StdCall"] rtlDragDetect (hwnd : HWND; pt : POINT) : BOOL;
BEGIN
    RETURN DragDetect (hwnd, pt.x, pt.y);
END rtlDragDetect;

PROCEDURE ["StdCall"] / WindowFromPoint (X, Y : LONG) : HWND;

PROCEDURE ["StdCall"] rtlWindowFromPoint (pt : POINT) : HWND;
BEGIN
    RETURN WindowFromPoint (pt.x, pt.y);
END rtlWindowFromPoint;

PROCEDURE ["StdCall"] / ChildWindowFromPoint (Parent: HWND; X, Y : LONG) : HWND;

PROCEDURE ["StdCall"] rtlChildWindowFromPoint (Parent: HWND; pt : POINT) : HWND;
BEGIN
    RETURN ChildWindowFromPoint (Parent, pt.x, pt.y);
END rtlChildWindowFromPoint;

PROCEDURE ["StdCall"] / ChildWindowFromPointEx (Parent  : HWND;
                                                X, Y: LONG;
                                                flags   : CWP_SET) : HWND;

PROCEDURE ["StdCall"] rtlChildWindowFromPointEx (Parent : HWND; pt : POINT; flags : CWP_SET) : HWND;
BEGIN
    RETURN ChildWindowFromPointEx (Parent, pt.x, pt.y, flags);
END rtlChildWindowFromPointEx;

PROCEDURE ["StdCall"] / PtInRect (Rect : RECT; X, Y : LONG) : BOOL;

PROCEDURE ["StdCall"] rtlPtInRect (Rect : RECT; pt : POINT) : BOOL;
BEGIN
    RETURN PtInRect (Rect, pt.x, pt.y);
END rtlPtInRect;

PROCEDURE ["StdCall"] GetWindowPtrA (hWnd : HWND; Index : INTEGER) : SYSTEM.ADDRESS;
BEGIN
    RETURN SYSTEM.CAST (SYSTEM.ADDRESS, Windows.GetWindowLongA (hWnd, Index));
END GetWindowPtrA;

PROCEDURE ["StdCall"] GetWindowPtrW (hWnd : HWND; Index : INTEGER) : SYSTEM.ADDRESS;
BEGIN
    RETURN SYSTEM.CAST (SYSTEM.ADDRESS, Windows.GetWindowLongW (hWnd, Index));
END GetWindowPtrW;

PROCEDURE ["StdCall"] SetWindowPtrA (hWnd : HWND; Index : INTEGER; NewLong : SYSTEM.ADDRESS) : SYSTEM.ADDRESS;
BEGIN
    RETURN SYSTEM.CAST (SYSTEM.ADDRESS, Windows.SetWindowLongA (hWnd, Index, SYSTEM.CAST (DWORD, NewLong)));
END SetWindowPtrA;

PROCEDURE ["StdCall"] SetWindowPtrW (hWnd : HWND; Index : INTEGER; NewLong : SYSTEM.ADDRESS) : SYSTEM.ADDRESS;
BEGIN
    RETURN SYSTEM.CAST (SYSTEM.ADDRESS, Windows.SetWindowLongW (hWnd, Index, SYSTEM.CAST (DWORD, NewLong)));
END SetWindowPtrW;

PROCEDURE ["StdCall"] CopyCursor (hCursor : HCURSOR) : HCURSOR;
BEGIN
    RETURN SYSTEM.CAST (HCURSOR, Windows.CopyIcon (SYSTEM.CAST (HICON, hCursor)));
END CopyCursor;

(* ----------------- WinGDI section ----------------------- *)

PROCEDURE ["StdCall"] MAKEROP4 (fore, back : ROP) : ROP4;
BEGIN
    RETURN SYSTEM.CAST (ROP4, SYSTEM.CAST (BITSET, (back * 256)) * {24..31} +
                              SYSTEM.CAST (BITSET, fore));
END MAKEROP4;

PROCEDURE ["StdCall"] GetCValue (cmyk : COLORREF) : BYTE;
BEGIN
    RETURN VAL (BYTE, cmyk MOD 256);
END GetCValue;

PROCEDURE ["StdCall"] GetMValue (cmyk : COLORREF) : BYTE;
BEGIN
    RETURN VAL (BYTE, cmyk DIV 100H MOD 100H);
END GetMValue;

PROCEDURE ["StdCall"] GetYValue (cmyk : COLORREF) : BYTE;
BEGIN
    RETURN VAL (BYTE, cmyk DIV 10000H MOD 100H);
END GetYValue;

PROCEDURE ["StdCall"] GetKValue (cmyk : COLORREF) : BYTE;
BEGIN
    RETURN VAL (BYTE, cmyk DIV 1000000H MOD 100H);
END GetKValue;

PROCEDURE ["StdCall"] CMYK (c,m,y,k : BYTE) : COLORREF;
BEGIN
    RETURN ORD (c) + ORD (m) * 100H + ORD (y) * 10000H + ORD (k) * 1000000H;
END CMYK;

PROCEDURE ["StdCall"] MAKEPOINTS (l : DWORD) : POINTS;
BEGIN
    RETURN SYSTEM.CAST (POINTS, l);
END MAKEPOINTS;

PROCEDURE ["StdCall"] RGB (r,g,b : BYTE) : COLORREF;
BEGIN
    RETURN ORD (r) + ORD (g) * 100H + ORD (b) * 10000H;
END RGB;

PROCEDURE ["StdCall"] PALETTERGB   (r,g,b : BYTE) : COLORREF;
BEGIN
    RETURN RGB (r, g, b) + 02000000H;
END PALETTERGB;

PROCEDURE ["StdCall"] PALETTEINDEX (i : BYTE) : COLORREF;
BEGIN
    RETURN ORD (i) + 01000000H;
END PALETTEINDEX;

PROCEDURE ["StdCall"] GetRValue (rgb : COLORREF) : BYTE;
BEGIN
    RETURN VAL (BYTE, rgb MOD 100H);
END GetRValue;

PROCEDURE ["StdCall"] GetGValue (rgb : COLORREF) : BYTE;
BEGIN
    RETURN VAL (BYTE, rgb DIV 100H MOD 100H);
END GetGValue;

PROCEDURE ["StdCall"] GetBValue (rgb : COLORREF) : BYTE;
BEGIN
    RETURN VAL (BYTE, rgb DIV 10000H MOD 100H);
END GetBValue;

(* ----------------- WinError section --------------------- *)


PROCEDURE ["StdCall"] SUCCEEDED (Status : ULONG) : BOOL;
BEGIN
    RETURN VAL (BOOL, ORD (SYSTEM.CAST (LONG, Status) >= 0));
END SUCCEEDED;

PROCEDURE ["StdCall"] FAILED (Status : ULONG) : BOOL;
BEGIN
    RETURN VAL (BOOL, ORD (SYSTEM.CAST (LONG, Status) < 0));

END FAILED;

PROCEDURE ["StdCall"] IS_ERROR (Status : ULONG) : BOOL;
BEGIN
    RETURN VAL (BOOL, ORD ((Status DIV 80000000H) = Windows.SEVERITY_ERROR));
END IS_ERROR;

PROCEDURE ["StdCall"] HRESULT_CODE (hr : ULONG) : ULONG;
BEGIN
    RETURN hr MOD 10000H;
END HRESULT_CODE;

PROCEDURE ["StdCall"] SCODE_CODE (sc : ULONG) : ULONG;
BEGIN
    RETURN sc MOD 10000H;
END SCODE_CODE;

PROCEDURE ["StdCall"] HRESULT_FACILITY (hr : ULONG) : ULONG;
BEGIN
    RETURN hr DIV 10000H MOD 2000H;
END HRESULT_FACILITY;

PROCEDURE ["StdCall"] SCODE_FACILITY (sc : ULONG) : ULONG;
BEGIN
    RETURN sc DIV 10000H MOD 2000H;
END SCODE_FACILITY;

PROCEDURE ["StdCall"] HRESULT_SEVERITY (hr : ULONG) : BOOL;
BEGIN
    RETURN VAL (BOOL, hr DIV 80000000H MOD 2);
END HRESULT_SEVERITY;

PROCEDURE ["StdCall"] SCODE_SEVERITY (sc : ULONG) : BOOL;
BEGIN
    RETURN VAL (BOOL, sc DIV 80000000H MOD 2);
END SCODE_SEVERITY;

PROCEDURE ["StdCall"] MAKE_HRESULT (sev : BOOL; fac : ULONG; code : ULONG) : ULONG;
BEGIN
    RETURN VAL (ULONG, sev) * 80000000H + fac * 10000H + code;
END MAKE_HRESULT;

PROCEDURE ["StdCall"] MAKE_SCODE (sev : BOOL; fac : ULONG; code : ULONG) : ULONG;
BEGIN
    RETURN VAL (ULONG, sev) * 80000000H + fac * 10000H + code;
END MAKE_SCODE;

PROCEDURE ["StdCall"] HRESULT_FROM_WIN32 (x : ULONG) : ULONG;
BEGIN
    IF x <> 0 THEN
       RETURN x MOD 10000H + Windows.FACILITY_WIN32 * 10000H + 80000000H;
    ELSE
       RETURN 0;
    END;
END HRESULT_FROM_WIN32;

CONST FACILITY_NT_BIT = 10000000H;

PROCEDURE ["StdCall"] HRESULT_FROM_NT (x : ULONG) : ULONG;
BEGIN
    RETURN x + FACILITY_NT_BIT;
END HRESULT_FROM_NT;

PROCEDURE ["StdCall"] GetScode (hr : ULONG) : ULONG;
BEGIN
    RETURN hr;
END GetScode;

PROCEDURE ["StdCall"] ResultFromScode (sc : ULONG) : ULONG;
BEGIN
    RETURN sc;
END ResultFromScode;

PROCEDURE ["StdCall"] PropagateResult (hrPrevious : ULONG; scBase : ULONG) : ULONG;
BEGIN
    RETURN scBase;
END PropagateResult;

(* ----------------- WinNT section ----------------------- *)

PROCEDURE ["StdCall"] MAKELANGID    (primary, sublang : WORD) : LANGID;
BEGIN
    RETURN ORD (sublang) * 400H + ORD (primary);
END MAKELANGID;

PROCEDURE ["StdCall"] PRIMARYLANGID (id : LANGID) : WORD;
BEGIN
    RETURN VAL (WORD, id MOD 400H);
END PRIMARYLANGID;

PROCEDURE ["StdCall"] SUBLANGID (id : LANGID) : WORD;
BEGIN
    RETURN id DIV 400H;
END SUBLANGID;

PROCEDURE ["StdCall"] MAKELCID (lgid : LANGID; srtid : WORD) : LCID;
BEGIN
    RETURN ORD (srtid) * 10000H + ORD (lgid);
END MAKELCID;

PROCEDURE ["StdCall"] LANGIDFROMLCID (lcid : LCID) : LANGID;
BEGIN
    RETURN VAL (LANGID, lcid MOD 10000H);
END LANGIDFROMLCID;

PROCEDURE ["StdCall"] SORTIDFROMLCID (lcid : LCID) : LANGID;
BEGIN
    RETURN SYSTEM.CAST (ULONG, SYSTEM.CAST (BITSET, lcid) * SYSTEM.CAST (BITSET,
           VAL (ULONG, Windows.NLS_VALID_LOCALE_MASK))) DIV 10000H;
END SORTIDFROMLCID;

PROCEDURE ["StdCall"] RtlCopyMemory (Destination : PVOID;
                                     Source : LPCVOID;
                                     Length : DWORD);
BEGIN
    SYSTEM.MOVE (Source, Destination, Length);
END RtlCopyMemory;

(* ----------------- WinCon section ---------------------- *)

PROCEDURE ["StdCall"] / ReadConsoleOutputA (hConsoleOutput : HANDLE;
                                            VAR Buffer     : ARRAY OF CHAR_INFO;
                                            dwBufferSize   : DWORD;
                                            dwBufferCoord  : DWORD;
                                            VAR ReadRegion : SMALL_RECT) : BOOL;

PROCEDURE ["StdCall"] rtlReadConsoleOutputA (hConsoleOutput : HANDLE;
                                             VAR Buffer     : ARRAY OF CHAR_INFO;
                                             dwBufferSize   : COORD;
                                             dwBufferCoord  : COORD;
                                             VAR ReadRegion : SMALL_RECT) : BOOL;
BEGIN
    RETURN ReadConsoleOutputA (hConsoleOutput, Buffer,
                               SYSTEM.CAST (DWORD, dwBufferSize),
                               SYSTEM.CAST (DWORD, dwBufferCoord),
                               ReadRegion);
END rtlReadConsoleOutputA;


PROCEDURE ["StdCall"] / ReadConsoleOutputW (hConsoleOutput : HANDLE;
                                            VAR Buffer     : ARRAY OF CHAR_INFO;
                                            dwBufferSize   : DWORD;
                                            dwBufferCoord  : DWORD;
                                            VAR ReadRegion : SMALL_RECT) : BOOL;

PROCEDURE ["StdCall"] rtlReadConsoleOutputW (hConsoleOutput : HANDLE;
                                             VAR Buffer     : ARRAY OF CHAR_INFO;
                                             dwBufferSize   : COORD;
                                             dwBufferCoord  : COORD;
                                             VAR ReadRegion : SMALL_RECT) : BOOL;
BEGIN
    RETURN ReadConsoleOutputW (hConsoleOutput, Buffer,
                               SYSTEM.CAST (DWORD, dwBufferSize),
                               SYSTEM.CAST (DWORD, dwBufferCoord),
                               ReadRegion);
END rtlReadConsoleOutputW;

(* ------------------------------------------------------- *)

PROCEDURE ["StdCall"] / WriteConsoleOutputA (hConsoleOutput : HANDLE;
                                             Buffer     : ARRAY OF CHAR_INFO;
                                             dwBufferSize   : DWORD;
                                             dwBufferCoord  : DWORD;
                                             VAR WriteRegion : SMALL_RECT) : BOOL;

PROCEDURE ["StdCall"] rtlWriteConsoleOutputA (hConsoleOutput  : HANDLE;
                                              Buffer          : ARRAY OF CHAR_INFO;
                                              dwBufferSize    : COORD;
                                              dwBufferCoord   : COORD;
                                              VAR WriteRegion : SMALL_RECT) : BOOL;
BEGIN
    RETURN WriteConsoleOutputA (hConsoleOutput, Buffer,
                                SYSTEM.CAST (DWORD, dwBufferSize),
                                SYSTEM.CAST (DWORD, dwBufferCoord),
                                WriteRegion);
END rtlWriteConsoleOutputA;


PROCEDURE ["StdCall"] / WriteConsoleOutputW (hConsoleOutput : HANDLE;
                                             Buffer     : ARRAY OF CHAR_INFO;
                                             dwBufferSize   : DWORD;
                                             dwBufferCoord  : DWORD;
                                             VAR WriteRegion : SMALL_RECT) : BOOL;

PROCEDURE ["StdCall"] rtlWriteConsoleOutputW (hConsoleOutput  : HANDLE;
                                              Buffer          : ARRAY OF CHAR_INFO;
                                              dwBufferSize    : COORD;
                                              dwBufferCoord   : COORD;
                                              VAR WriteRegion : SMALL_RECT) : BOOL;
BEGIN
    RETURN WriteConsoleOutputW (hConsoleOutput, Buffer,
                                SYSTEM.CAST (DWORD, dwBufferSize),
                                SYSTEM.CAST (DWORD, dwBufferCoord),
                                WriteRegion);
END rtlWriteConsoleOutputW;

(* ------------------------------------------------------- *)

PROCEDURE ["StdCall"] / ReadConsoleOutputCharacterA (hConsoleOutput : HANDLE;
                                                     VAR Character  : ARRAY OF CHAR;
                                                     nLength        : DWORD;
                                                     dwReadCoord    : DWORD;
                                                     VAR NumberOfCharsRead : DWORD) : BOOL;

PROCEDURE ["StdCall"] rtlReadConsoleOutputCharacterA (hConsoleOutput : HANDLE;
                                                      VAR Character  : ARRAY OF CHAR;
                                                      nLength        : DWORD;
                                                      dwReadCoord    : COORD;
                                                      VAR NumberOfCharsRead : DWORD) : BOOL;
BEGIN
    RETURN ReadConsoleOutputCharacterA (hConsoleOutput, Character, nLength,
                                        SYSTEM.CAST (DWORD, dwReadCoord),
                                        NumberOfCharsRead);
END rtlReadConsoleOutputCharacterA;


PROCEDURE ["StdCall"] / ReadConsoleOutputCharacterW (hConsoleOutput : HANDLE;
                                                     VAR Character  : ARRAY OF WCHAR;
                                                     nLength        : DWORD;
                                                     dwReadCoord    : DWORD;
                                                     VAR NumberOfCharsRead : DWORD) : BOOL;

PROCEDURE ["StdCall"] rtlReadConsoleOutputCharacterW (hConsoleOutput : HANDLE;
                                                      VAR Character  : ARRAY OF WCHAR;
                                                      nLength        : DWORD;
                                                      dwReadCoord    : COORD;
                                                      VAR NumberOfCharsRead : DWORD) : BOOL;
BEGIN
    RETURN ReadConsoleOutputCharacterW (hConsoleOutput, Character,
                                        nLength, SYSTEM.CAST (DWORD, dwReadCoord),
                                        NumberOfCharsRead);
END rtlReadConsoleOutputCharacterW;

(* ------------------------------------------------------- *)

PROCEDURE ["StdCall"] / ReadConsoleOutputAttribute (hConsoleOutput        : HANDLE;
                                                    VAR Attribute         : ARRAY OF CHAR_ATTRIBUTES_SET;
                                                    nLength               : DWORD;
                                                    dwReadCoord           : DWORD;
                                                    VAR NumberOfAttrsRead : DWORD) : BOOL;

PROCEDURE ["StdCall"] rtlReadConsoleOutputAttribute (hConsoleOutput        : HANDLE;
                                                     VAR Attribute         : ARRAY OF CHAR_ATTRIBUTES_SET;
                                                     nLength               : DWORD;
                                                     dwReadCoord           : COORD;
                                                     VAR NumberOfAttrsRead : DWORD) : BOOL;
BEGIN
    RETURN ReadConsoleOutputAttribute (hConsoleOutput,
                                       Attribute, nLength,
                                       SYSTEM.CAST (DWORD, dwReadCoord),
                                       NumberOfAttrsRead);
END rtlReadConsoleOutputAttribute;

(* ------------------------------------------------------- *)

PROCEDURE ["StdCall"] / WriteConsoleOutputCharacterA (hConsoleOutput : HANDLE;
                                                      lpCharacter    : PCSTR;
                                                      nLength        : DWORD;
                                                      dwWriteCoord   : DWORD;
                                                      VAR NumberOfCharsWritten : DWORD) : BOOL;

PROCEDURE ["StdCall"] rtlWriteConsoleOutputCharacterA (hConsoleOutput : HANDLE;
                                                       lpCharacter    : PCSTR;
                                                       nLength        : DWORD;
                                                       dwWriteCoord   : COORD;
                                                       VAR NumberOfCharsWritten : DWORD) : BOOL;
BEGIN
    RETURN WriteConsoleOutputCharacterA (hConsoleOutput, lpCharacter, nLength,
                                         SYSTEM.CAST (DWORD, dwWriteCoord),
                                         NumberOfCharsWritten);
END rtlWriteConsoleOutputCharacterA;


PROCEDURE ["StdCall"] / WriteConsoleOutputCharacterW (hConsoleOutput : HANDLE;
                                                      lpCharacter    : PCWSTR;
                                                      nLength        : DWORD;
                                                      dwWriteCoord   : DWORD;
                                                      VAR NumberOfCharsWritten : DWORD) : BOOL;

PROCEDURE ["StdCall"] rtlWriteConsoleOutputCharacterW (hConsoleOutput : HANDLE;
                                           lpCharacter    : PCWSTR;
                                           nLength        : DWORD;
                                           dwWriteCoord   : COORD;
                                           VAR NumberOfCharsWritten : DWORD) : BOOL;
BEGIN
    RETURN WriteConsoleOutputCharacterW (hConsoleOutput, lpCharacter, nLength,
                                         SYSTEM.CAST (DWORD, dwWriteCoord),
                                         NumberOfCharsWritten);
END rtlWriteConsoleOutputCharacterW;

(* ------------------------------------------------------- *)

PROCEDURE ["StdCall"] / WriteConsoleOutputAttribute (hConsoleOutput : HANDLE;
                                                     Attribute      : ARRAY OF CHAR_ATTRIBUTES_SET;
                                                     nLength        : DWORD;
                                                     dwWriteCoord   : DWORD;
                                                     VAR NumberOfAttrsWritten : DWORD) : BOOL;

PROCEDURE ["StdCall"] rtlWriteConsoleOutputAttribute (hConsoleOutput : HANDLE;
                                                      Attribute      : ARRAY OF CHAR_ATTRIBUTES_SET;
                                                      nLength        : DWORD;
                                                      dwWriteCoord   : COORD;
                                                      VAR NumberOfAttrsWritten : DWORD) : BOOL;
BEGIN
    RETURN WriteConsoleOutputAttribute (hConsoleOutput, Attribute, nLength,
                                        SYSTEM.CAST (DWORD, dwWriteCoord),
                                        NumberOfAttrsWritten);
END rtlWriteConsoleOutputAttribute;

(* ------------------------------------------------------- *)

PROCEDURE ["StdCall"] / FillConsoleOutputCharacterA (hConsoleOutput : HANDLE;
                                                     cCharacter     : CHAR;
                                                     nLength        : DWORD;
                                                     dwWriteCoord   : DWORD;
                                                     VAR NumberOfCharsWritten : DWORD) : BOOL;

PROCEDURE ["StdCall"] rtlFillConsoleOutputCharacterA (hConsoleOutput : HANDLE;
                                                      cCharacter     : CHAR;
                                                      nLength        : DWORD;
                                                      dwWriteCoord   : COORD;
                                                      VAR NumberOfCharsWritten : DWORD) : BOOL;
BEGIN
    RETURN FillConsoleOutputCharacterA (hConsoleOutput, cCharacter, nLength,
                                        SYSTEM.CAST (DWORD, dwWriteCoord),
                                        NumberOfCharsWritten);
END rtlFillConsoleOutputCharacterA;


PROCEDURE ["StdCall"] / FillConsoleOutputCharacterW (hConsoleOutput : HANDLE;
                                                     cCharacter     : WCHAR;
                                                     nLength        : DWORD;
                                                     dwWriteCoord   : DWORD;
                                                     VAR NumberOfCharsWritten : DWORD) : BOOL;

PROCEDURE ["StdCall"] rtlFillConsoleOutputCharacterW (hConsoleOutput : HANDLE;
                                                      cCharacter     : WCHAR;
                                                      nLength        : DWORD;
                                                      dwWriteCoord   : COORD;
                                                      VAR NumberOfCharsWritten : DWORD) : BOOL;
BEGIN
    RETURN FillConsoleOutputCharacterW (hConsoleOutput, cCharacter, nLength,
                                        SYSTEM.CAST (DWORD, dwWriteCoord),
                                        NumberOfCharsWritten);
END rtlFillConsoleOutputCharacterW;

(* ------------------------------------------------------- *)

PROCEDURE ["StdCall"] / FillConsoleOutputAttribute (hConsoleOutput : HANDLE;
                                                    wAttribute     : CHAR_ATTRIBUTES_SET;
                                                    nLength        : DWORD;
                                                    dwWriteCoord   : DWORD;
                                                    VAR NumberOfAttrsWritten : DWORD) : BOOL;

PROCEDURE ["StdCall"] rtlFillConsoleOutputAttribute (hConsoleOutput : HANDLE;
                                                     wAttribute     : CHAR_ATTRIBUTES_SET;
                                                     nLength        : DWORD;
                                                     dwWriteCoord   : COORD;
                                                     VAR NumberOfAttrsWritten : DWORD) : BOOL;
BEGIN
    RETURN FillConsoleOutputAttribute (hConsoleOutput, wAttribute, nLength,
                                       SYSTEM.CAST (DWORD, dwWriteCoord),
                                       NumberOfAttrsWritten);
END rtlFillConsoleOutputAttribute;

(* ------------------------------------------------------- *)

PROCEDURE ["StdCall"] / GetLargestConsoleWindowSize (hConsoleOutput : HANDLE) : DWORD;

PROCEDURE ["StdCall"] rtlGetLargestConsoleWindowSize (hConsoleOutput : HANDLE) : COORD;
BEGIN
    RETURN SYSTEM.CAST (COORD, GetLargestConsoleWindowSize (hConsoleOutput));
END rtlGetLargestConsoleWindowSize;

(* ------------------------------------------------------- *)

PROCEDURE ["StdCall"] / SetConsoleScreenBufferSize (hConsoleOutput : HANDLE;
                                                    dwSize         : DWORD) : BOOL;

PROCEDURE ["StdCall"] rtlSetConsoleScreenBufferSize (hConsoleOutput : HANDLE;
                                                     dwSize         : COORD) : BOOL;
BEGIN
    RETURN SetConsoleScreenBufferSize (hConsoleOutput,
                                       SYSTEM.CAST (DWORD, dwSize));
END rtlSetConsoleScreenBufferSize;

(* ------------------------------------------------------- *)

PROCEDURE ["StdCall"] / SetConsoleCursorPosition (hConsoleOutput   : HANDLE;
                                         dwCursorPosition : DWORD) : BOOL;

PROCEDURE ["StdCall"] rtlSetConsoleCursorPosition (hConsoleOutput   : HANDLE;
                                         dwCursorPosition : COORD) : BOOL;
BEGIN
    RETURN SetConsoleCursorPosition (hConsoleOutput,
                                     SYSTEM.CAST (DWORD, dwCursorPosition));
END rtlSetConsoleCursorPosition;

(* ------------------------------------------------------- *)

PROCEDURE ["StdCall"] / ScrollConsoleScreenBufferA (hConsoleOutput      : HANDLE;
                                                    ScrollRectangle     : SMALL_RECT;
                                                    ClipRectangle       : PCSMALL_RECT; (* can be NILL *)
                                                    dwDestinationOrigin : DWORD;
                                                    Fill                : CHAR_INFO) : BOOL;

PROCEDURE ["StdCall"] rtlScrollConsoleScreenBufferA (hConsoleOutput      : HANDLE;
                                                     ScrollRectangle     : SMALL_RECT;
                                                     ClipRectangle       : PCSMALL_RECT;
                                                     dwDestinationOrigin : COORD;
                                                     Fill                : CHAR_INFO) : BOOL;
BEGIN
    RETURN ScrollConsoleScreenBufferA (hConsoleOutput, ScrollRectangle,
                                       ClipRectangle,
                                       SYSTEM.CAST (DWORD, dwDestinationOrigin),
                                       Fill);
END rtlScrollConsoleScreenBufferA;


PROCEDURE ["StdCall"] / ScrollConsoleScreenBufferW (hConsoleOutput      : HANDLE;
                                                    ScrollRectangle     : SMALL_RECT;
                                                    ClipRectangle       : PCSMALL_RECT; (* can be NILL *)
                                                    dwDestinationOrigin : DWORD;
                                                    Fill                : CHAR_INFO) : BOOL;

PROCEDURE ["StdCall"] rtlScrollConsoleScreenBufferW (hConsoleOutput      : HANDLE;
                                         ScrollRectangle     : SMALL_RECT;
                                         ClipRectangle       : PCSMALL_RECT;
                                         dwDestinationOrigin : COORD;
                                         Fill                : CHAR_INFO) : BOOL;
BEGIN
    RETURN ScrollConsoleScreenBufferW (hConsoleOutput, ScrollRectangle,
                                       ClipRectangle,
                                       SYSTEM.CAST (DWORD, dwDestinationOrigin),
                                       Fill);
END rtlScrollConsoleScreenBufferW;

(* ----------------- CommDlg section ----------------------- *)

PROCEDURE ["StdCall"] CommDlg_OpenSave_GetSpecA (hdlg : HWND; psz : PSTR; cbmax : INTEGER) : INTEGER;
BEGIN
    RETURN VAL (INTEGER, SendMessageA (hdlg, CDM_GETSPEC, VAL (WPARAM, cbmax),
                                       SYSTEM.CAST (LPARAM, psz)));
END CommDlg_OpenSave_GetSpecA;

PROCEDURE ["StdCall"] CommDlg_OpenSave_GetSpecW (hdlg : HWND; psz : PWSTR; cbmax : INTEGER) : INTEGER;
BEGIN
    RETURN VAL (INTEGER, SendMessageW (hdlg, CDM_GETSPEC, VAL (WPARAM, cbmax),
                                       SYSTEM.CAST (LPARAM, psz)));
END CommDlg_OpenSave_GetSpecW;

PROCEDURE ["StdCall"] CommDlg_OpenSave_GetFilePathA (hdlg : HWND; psz : PSTR; cbmax : INTEGER) : INTEGER;
BEGIN
    RETURN VAL (INTEGER, SendMessageA (hdlg, CDM_GETFILEPATH, VAL (WPARAM, cbmax),
                                       SYSTEM.CAST (LPARAM, psz)));
END CommDlg_OpenSave_GetFilePathA;

PROCEDURE ["StdCall"] CommDlg_OpenSave_GetFilePathW (hdlg : HWND; psz : PWSTR; cbmax : INTEGER) : INTEGER;
BEGIN
    RETURN VAL (INTEGER, SendMessageW (hdlg, CDM_GETFILEPATH, VAL (WPARAM, cbmax),
                                       SYSTEM.CAST (LPARAM, psz)));
END CommDlg_OpenSave_GetFilePathW;

PROCEDURE ["StdCall"] CommDlg_OpenSave_GetFolderPathA (hdlg : HWND; psz : PSTR; cbmax : INTEGER) : INTEGER;
BEGIN
    RETURN VAL (INTEGER, SendMessageA (hdlg, CDM_GETFOLDERPATH, VAL (WPARAM, cbmax),
                                       SYSTEM.CAST (LPARAM, psz)));
END CommDlg_OpenSave_GetFolderPathA;

PROCEDURE ["StdCall"] CommDlg_OpenSave_GetFolderPathW (hdlg : HWND; psz : PWSTR; cbmax : INTEGER) : INTEGER;
BEGIN
    RETURN VAL (INTEGER, SendMessageW (hdlg, CDM_GETFOLDERPATH, VAL (WPARAM, cbmax),
                                       SYSTEM.CAST (LPARAM, psz)));
END CommDlg_OpenSave_GetFolderPathW;

PROCEDURE ["StdCall"] CommDlg_OpenSave_GetFolderIDList (hdlg : HWND; pidl : PVOID; cbmax : INTEGER) : INTEGER;
BEGIN
    RETURN VAL (INTEGER, SendMessageW (hdlg, CDM_GETFOLDERIDLIST,
                                       VAL (WPARAM, cbmax),
                                       SYSTEM.CAST (LPARAM, pidl)));
END CommDlg_OpenSave_GetFolderIDList;

PROCEDURE ["StdCall"] CommDlg_OpenSave_SetControlText (hdlg : HWND; id : INTEGER; text : PSTR) : INTEGER;
BEGIN
    RETURN VAL (INTEGER, SendMessageA (hdlg, CDM_SETCONTROLTEXT,
                                       VAL (WPARAM, id),
                                       SYSTEM.CAST (LPARAM, text)));
END CommDlg_OpenSave_SetControlText;

PROCEDURE ["StdCall"] CommDlg_OpenSave_HideControl (hdlg : HWND; id : INTEGER);
BEGIN
    SendMessageA (hdlg, CDM_HIDECONTROL, VAL (WPARAM, id), 0);
END CommDlg_OpenSave_HideControl;

PROCEDURE ["StdCall"] CommDlg_OpenSave_SetDefExt (hdlg : HWND; ext : PSTR);
BEGIN
    SendMessageA (hdlg, CDM_SETDEFEXT, 0, SYSTEM.CAST (LPARAM, ext));
END CommDlg_OpenSave_SetDefExt;


(* ----------------- WinSock section --------------------- *)

PROCEDURE ["StdCall"] FD_CLR (fd : SOCKET; VAR set : fd_set);
VAR i, j, c : INTEGER;
BEGIN
    c := VAL (INTEGER, set.fd_count);
    FOR i := 0 TO c - 1 DO
        IF set.fd_array [i] = fd THEN
           FOR j := i TO c - 2 DO
               set.fd_array [j] := set.fd_array [j+1];
           END;
           RETURN;
        END;
    END;
END FD_CLR;

PROCEDURE ["StdCall"] FD_SET (fd : SOCKET; VAR set : fd_set);
BEGIN
    IF set.fd_count < FD_SETSIZE THEN
       set.fd_array [set.fd_count] := fd;
       INC (set.fd_count);
    END;
END FD_SET;

PROCEDURE ["StdCall"] FD_ZERO (VAR set : fd_set);
BEGIN
    set.fd_count := 0;
END FD_ZERO;


(*
 * Operations on timevals.
 *
 * NB: timercmp does not work for >= or <=.
 *)

PROCEDURE ["StdCall"] timerisset (VAR tvp : timeval) : BOOL;
BEGIN
    RETURN (tvp.tv_sec <> 0) OR (tvp.tv_usec <> 0);
END timerisset;

PROCEDURE ["StdCall"] timergt (tvp, uvp : timeval) : BOOL;
BEGIN
    RETURN (tvp.tv_sec > uvp.tv_sec) OR
           (tvp.tv_sec = uvp.tv_sec) AND (tvp.tv_usec > uvp.tv_usec);
END timergt;

PROCEDURE ["StdCall"] timerlt (tvp, uvp : timeval) : BOOL;
BEGIN
    RETURN (tvp.tv_sec < uvp.tv_sec) OR
           (tvp.tv_sec = uvp.tv_sec) AND (tvp.tv_usec < uvp.tv_usec);
END timerlt;

PROCEDURE ["StdCall"] timerequ (tvp, uvp : timeval) : BOOL;
BEGIN
    RETURN (tvp.tv_sec = uvp.tv_sec) AND (tvp.tv_usec = uvp.tv_usec);
END timerequ;

PROCEDURE ["StdCall"] timerclear (VAR tvp : timeval);
BEGIN
    tvp.tv_sec  := 0;
    tvp.tv_usec := 0;
END timerclear;

PROCEDURE ["StdCall"] IN_CLASSA (i : ULONG) : BOOL;
BEGIN
    RETURN SYSTEM.CAST (BITSET, i) * {31} = {};
END IN_CLASSA;

PROCEDURE ["StdCall"] IN_CLASSB (i : ULONG) : BOOL;
BEGIN
    RETURN SYSTEM.CAST (BITSET, i) * {31,30} = {31};
END IN_CLASSB;

PROCEDURE ["StdCall"] IN_CLASSC (i : ULONG) : BOOL;
BEGIN
    RETURN SYSTEM.CAST (BITSET, i) * {31,30,29} = {31,30};
END IN_CLASSC;

PROCEDURE ["StdCall"] / inet_ntoa (in : u_long) : PSTR;

PROCEDURE ["StdCall"] rtl_inet_ntoa (in : in_addr) : PSTR;
BEGIN
    RETURN inet_ntoa (in.S_addr);
END rtl_inet_ntoa;


(* ----------------- MMSystem section -------------------- *)

PROCEDURE ["StdCall"] MAKEFOURCC (ch0, ch1, ch2, ch3 : BYTE) : DWORD;
BEGIN
    RETURN VAL (DWORD, ch0) +
           VAL (DWORD, ch1) * 100H +
           VAL (DWORD, ch2) * 10000H +
           VAL (DWORD, ch3) * 1000000H;
END MAKEFOURCC;

PROCEDURE ["StdCall"] MEVT_EVENTTYPE (x : DWORD) : BYTE;
BEGIN
    RETURN VAL (BYTE, x DIV 1000000H);
END MEVT_EVENTTYPE;

PROCEDURE ["StdCall"] MEVT_EVENTPARM (x : DWORD) : DWORD;
BEGIN
    RETURN x MOD 01000000H;
END MEVT_EVENTPARM;

PROCEDURE ["StdCall"] MCI_MSF_MINUTE (msf : DWORD) : BYTE;
BEGIN
    RETURN VAL (BYTE, msf);
END MCI_MSF_MINUTE;

PROCEDURE ["StdCall"] MCI_MSF_SECOND (msf : DWORD) : BYTE;
BEGIN
    RETURN VAL (BYTE, msf DIV 100H);
END MCI_MSF_SECOND;

PROCEDURE ["StdCall"] MCI_MSF_FRAME (msf : DWORD) : BYTE;
BEGIN
    RETURN VAL (BYTE, msf DIV 10000H);
END MCI_MSF_FRAME;

PROCEDURE ["StdCall"] MCI_MAKE_MSF (m, s, f : BYTE) : DWORD;
BEGIN
    RETURN VAL (DWORD, m) +
           VAL (DWORD, s) * 100H +
           VAL (DWORD, f) * 10000H;
END MCI_MAKE_MSF;

PROCEDURE ["StdCall"] MCI_TMSF_TRACK (tmsf : DWORD) : BYTE;
BEGIN
    RETURN VAL (BYTE, tmsf MOD 100H);
END MCI_TMSF_TRACK;

PROCEDURE ["StdCall"] MCI_TMSF_MINUTE (tmsf : DWORD) : BYTE;
BEGIN
    RETURN VAL (BYTE, tmsf DIV 100H MOD 100H);
END MCI_TMSF_MINUTE;

PROCEDURE ["StdCall"] MCI_TMSF_SECOND (tmsf : DWORD) : BYTE;
BEGIN
    RETURN VAL (BYTE, tmsf DIV 10000H MOD 100H);
END MCI_TMSF_SECOND;

PROCEDURE ["StdCall"] MCI_TMSF_FRAME (tmsf : DWORD) : BYTE;
BEGIN
    RETURN VAL (BYTE, tmsf DIV 1000000H MOD 100H);
END MCI_TMSF_FRAME;

PROCEDURE ["StdCall"] MCI_MAKE_TMSF (t, m, s, f : BYTE) : DWORD;
BEGIN
    RETURN VAL (DWORD, t) +
           VAL (DWORD, m) * 100H +
           VAL (DWORD, s) * 10000H +
           VAL (DWORD, f) * 1000000H;
END MCI_MAKE_TMSF;

PROCEDURE ["StdCall"] MCI_HMS_HOUR (hms : DWORD) : BYTE;
BEGIN
    RETURN VAL (BYTE, hms MOD 100H);
END MCI_HMS_HOUR;

PROCEDURE ["StdCall"] MCI_HMS_MINUTE (hms : DWORD) : BYTE;
BEGIN
    RETURN VAL (BYTE, hms DIV 100H MOD 100H);
END MCI_HMS_MINUTE;

PROCEDURE ["StdCall"] MCI_HMS_SECOND (hms : DWORD) : BYTE;
BEGIN
    RETURN VAL (BYTE, hms DIV 10000H MOD 100H);
END MCI_HMS_SECOND;

PROCEDURE ["StdCall"] MCI_MAKE_HMS (h, m, s : BYTE) : DWORD;
BEGIN
    RETURN VAL (DWORD, h) +
           VAL (DWORD, m) * 100H +
           VAL (DWORD, s) * 10000H;
END MCI_MAKE_HMS;

PROCEDURE ["StdCall"] DIBINDEX (n : WORD) : DWORD;
BEGIN
    RETURN MAKELONG (n, 010FFH);
END DIBINDEX;

<* END *>

END WinRTL.
