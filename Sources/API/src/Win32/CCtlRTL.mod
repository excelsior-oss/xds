<* M2EXTENSIONS+ *>
<* PROCINLINE+ *>
<* CHECKNIL-   *>
<* CHECKRANGE- *>

IMPLEMENTATION MODULE CCtlRTL;

FROM WinDef   IMPORT BOOL, UINT, BYTE, WCHAR, DWORD, WPARAM, LPARAM, LRESULT,
                     PSTR, PWSTR, PCSTR, PCWSTR,
                     HWND, HINSTANCE, HMENU, HICON, HFONT, HCURSOR,
                     COLORREF, POINT, RECT, MAKELONG, PCRECT;
FROM WinUser  IMPORT CreateWindow, SendMessageA, SendMessageW, PostMessage, WS_SET, MSG,
                     MAKELPARAM, LR_SET, NMHDR, PNMHDR, WNDPROC,
                     WM_NOTIFY, IMAGE_TYPE;
FROM WinBase  IMPORT SYSTEMTIME, PSYSTEMTIME;
FROM CommCtrl IMPORT PFN_WM_NOTIFY_HANDLER,
                     HPROPSHEETPAGE, HIMAGELIST,
                     HDITEMA, HDITEMW, HDLAYOUT, HTREEITEM,
                     LVITEMA, LVFINDINFOA, LVBKIMAGEA, LVCOLUMNA,
                     LVITEMW, LVFINDINFOW, LVBKIMAGEW, LVCOLUMNW,
                     LVHITTESTINFO,
                     TVITEMA, TVINSERTSTRUCTA,
                     TVITEMW, TVINSERTSTRUCTW, TVHITTESTINFO, TVSORTCB,
                     TCITEMA, TCITEMW, TCHITTESTINFO, PFNLVCOMPARE,
                     MCHITTESTINFO, MONTHDAYSTATE,
                     PSH_SET, PSWIZB_SET, PSBTN_ENUM, BIT_SET, ILD_SET, TCS_EX_SET,
                     LVIS_SET, LVNI_SET, LVIR_ENUM, LVA_ENUM, LVSICF_SET, LVS_EX_SET, LVSIL_ENUM,
                     TVE_SET, TVGN_ENUM, MCSC_ENUM, GMR_ENUM, GDTR_SET, PGB_ENUM;

IMPORT SYSTEM, CC := CommCtrl;

CONST
  SNDMSG = SendMessageA;
  DLS = "StdCall";


(*  ========== General functions ============ *)

PROCEDURE [DLS] HANDLE_WM_NOTIFY ( hwnd : HWND; wParam : WPARAM;
                                   lParam : LPARAM;
                                   fn : PFN_WM_NOTIFY_HANDLER ): LRESULT;
VAR nm : PNMHDR;
BEGIN
    nm := SYSTEM.CAST (PNMHDR, lParam);
    RETURN fn (hwnd, SYSTEM.CAST (INTEGER, wParam), nm^);
END HANDLE_WM_NOTIFY;

PROCEDURE [DLS] FORWARD_WM_NOTIFY ( hwnd : HWND; idFrom : INTEGER;
                                    VAR nmhdr : NMHDR; fn : WNDPROC ): LRESULT;
BEGIN
    RETURN fn (hwnd, WM_NOTIFY, SYSTEM.CAST (WPARAM, idFrom),
               SYSTEM.CAST (LPARAM, SYSTEM.ADR (nmhdr)));
END FORWARD_WM_NOTIFY;

(*  ========== Animate ============ *)

PROCEDURE [DLS] Animate_Create ( hwnd: HWND; id: UINT; dwStyle: WS_SET;
                                 hInstance: HINSTANCE ): HWND;
BEGIN
  RETURN CreateWindow(CC.ANIMATE_CLASS, "", dwStyle, 0, 0, 0, 0, 
                      hwnd, SYSTEM.CAST(HMENU, id), hInstance, NIL);
END Animate_Create;

PROCEDURE [DLS] Animate_Close ( hwnd: HWND ): BOOL;
BEGIN
  RETURN Animate_OpenA(hwnd, NIL);
END Animate_Close;

PROCEDURE [DLS] Animate_OpenA ( hwnd: HWND; szName: PSTR ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageA(hwnd, CC.ACM_OPENA, 0, SYSTEM.CAST(LPARAM, szName)));
END Animate_OpenA;

PROCEDURE [DLS] Animate_OpenW ( hwnd: HWND; szName: PWSTR ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageW(hwnd, CC.ACM_OPENW, 0, SYSTEM.CAST(LPARAM, szName)));
END Animate_OpenW;

PROCEDURE [DLS] Animate_OpenExA ( hwnd: HWND; hinst: HINSTANCE; szName: PSTR ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageA(hwnd, CC.ACM_OPENA, SYSTEM.CAST(WPARAM, hinst),
                                        SYSTEM.CAST(LPARAM, szName)));
END Animate_OpenExA;

PROCEDURE [DLS] Animate_OpenExW ( hwnd: HWND; hinst: HINSTANCE; szName: PWSTR ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageW(hwnd, CC.ACM_OPENW, SYSTEM.CAST(WPARAM, hinst),
                                        SYSTEM.CAST(LPARAM, szName)));
END Animate_OpenExW;

PROCEDURE [DLS] Animate_Play ( hwnd: HWND; wFrom, wTo, cRepeat: UINT ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwnd, CC.ACM_PLAY, VAL(WPARAM, cRepeat),
                                  SYSTEM.CAST(LPARAM, MAKELONG(wFrom, wTo))));
END Animate_Play;

PROCEDURE [DLS] Animate_Seek ( hwnd: HWND; wFrame: UINT ): BOOL;
BEGIN
  RETURN Animate_Play(hwnd, wFrame, wFrame, 1);
END Animate_Seek;

PROCEDURE [DLS] Animate_Stop ( hwnd: HWND ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwnd, CC.ACM_STOP, 0, 0));
END Animate_Stop;


(*  ============ DateTime ============= *)

PROCEDURE [DLS] DateTime_GetMonthCal ( hwndDP: HWND ): HWND;
BEGIN
  RETURN SYSTEM.CAST(HWND, SNDMSG(hwndDP, CC.DTM_GETMONTHCAL, 0, 0));
END DateTime_GetMonthCal;

PROCEDURE [DLS] DateTime_GetMonthCalColor ( hwndDP: HWND; iColor: MCSC_ENUM ): COLORREF;
BEGIN
  RETURN SYSTEM.CAST(COLORREF, SNDMSG(hwndDP, CC.DTM_GETMCCOLOR, ORD(iColor), 0));
END DateTime_GetMonthCalColor;

PROCEDURE [DLS] DateTime_GetMonthCalFont ( hwndDP: HWND ): HFONT;
BEGIN
  RETURN SYSTEM.CAST(HFONT, SNDMSG(hwndDP, CC.DTM_GETMCFONT, 0, 0));
END DateTime_GetMonthCalFont;

PROCEDURE [DLS] DateTime_GetRange ( hwndDP: HWND; VAR SysTimeArray: ARRAY OF SYSTEMTIME ): GDTR_SET;
BEGIN
  RETURN SYSTEM.CAST(GDTR_SET, SNDMSG(hwndDP, CC.DTM_GETRANGE, 0,
                                      SYSTEM.CAST(LPARAM, SYSTEM.ADR(SysTimeArray))));
END DateTime_GetRange;

PROCEDURE [DLS] DateTime_GetSystemtime ( hwndDP: HWND; VAR SysTime: SYSTEMTIME ): DWORD;
BEGIN
  RETURN SYSTEM.CAST(DWORD, SNDMSG(hwndDP, CC.DTM_GETSYSTEMTIME, 0,
                                   SYSTEM.CAST(LPARAM, SYSTEM.ADR(SysTime))));
END DateTime_GetSystemtime;

PROCEDURE [DLS] DateTime_SetFormatA ( hwndDP: HWND; szFormat: PCSTR ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageA(hwndDP, CC.DTM_SETFORMATA, 0, SYSTEM.CAST(LPARAM, szFormat)));
END DateTime_SetFormatA;

PROCEDURE [DLS] DateTime_SetFormatW ( hwndDP: HWND; szFormat: PCWSTR ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageW(hwndDP, CC.DTM_SETFORMATW, 0, SYSTEM.CAST(LPARAM, szFormat)));
END DateTime_SetFormatW;

PROCEDURE [DLS] DateTime_SetMonthCalColor ( hwndDP: HWND; iColor: MCSC_ENUM; clr: COLORREF ): COLORREF;
BEGIN
  RETURN SYSTEM.CAST(COLORREF, SNDMSG(hwndDP, CC.DTM_SETMCCOLOR, ORD(iColor), SYSTEM.CAST(LPARAM, clr)));
END DateTime_SetMonthCalColor;

PROCEDURE [DLS] DateTime_SetMonthCalFont ( hwndDP: HWND; hFont: HFONT; fRedraw: BOOL );
BEGIN
  SNDMSG(hwndDP, CC.DTM_SETMCFONT, SYSTEM.CAST(WPARAM, hFont), SYSTEM.CAST(LPARAM, fRedraw));
END DateTime_SetMonthCalFont;

PROCEDURE [DLS] DateTime_SetRange ( hwndDP: HWND; flags: GDTR_SET;
                                    SysTimeArray-: ARRAY OF SYSTEMTIME (*!*) ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndDP, CC.DTM_SETRANGE, SYSTEM.CAST(WPARAM, flags),
                                  SYSTEM.CAST(LPARAM, SYSTEM.ADR(SysTimeArray))));
END DateTime_SetRange;

PROCEDURE [DLS] DateTime_SetSystemtime ( hwndDP: HWND; flag: DWORD;
                                         SysTime-: SYSTEMTIME (*!*) ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndDP, CC.DTM_SETSYSTEMTIME, flag, SYSTEM.CAST(LPARAM, SYSTEM.ADR(SysTime))));
END DateTime_SetSystemtime;


(*  ============ Header ============= *)

PROCEDURE [DLS] Header_CreateDragImage ( hwndHD: HWND; iIndex: INTEGER ): HIMAGELIST;
BEGIN
  RETURN SYSTEM.CAST(HIMAGELIST, SNDMSG(hwndHD, CC.HDM_CREATEDRAGIMAGE, iIndex, 0));
END Header_CreateDragImage;

PROCEDURE [DLS] Header_DeleteItem ( hwndHD: HWND; iIndex: INTEGER ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndHD, CC.HDM_DELETEITEM, iIndex, 0));
END Header_DeleteItem;

PROCEDURE [DLS] Header_GetImageList ( hwndHD: HWND ): HIMAGELIST;
BEGIN
  RETURN SYSTEM.CAST(HIMAGELIST, SNDMSG(hwndHD, CC.HDM_GETIMAGELIST, 0, 0));
END Header_GetImageList;

PROCEDURE [DLS] Header_GetItemA ( hwndHD: HWND; iIndex: INTEGER; VAR hdi: HDITEMA ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageA(hwndHD, CC.HDM_GETITEMA, iIndex,
                                        SYSTEM.CAST(LPARAM, SYSTEM.ADR(hdi))));
END Header_GetItemA;

PROCEDURE [DLS] Header_GetItemW ( hwndHD: HWND; iIndex: INTEGER; VAR hdi: HDITEMW ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageW(hwndHD, CC.HDM_GETITEMW, iIndex,
                                        SYSTEM.CAST(LPARAM, SYSTEM.ADR(hdi))));
END Header_GetItemW;

PROCEDURE [DLS] Header_GetItemCount ( hwndHD: HWND ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hwndHD, CC.HDM_GETITEMCOUNT, 0, 0));
END Header_GetItemCount;

PROCEDURE [DLS] Header_GetItemRect ( hwndHD: HWND; iIndex: INTEGER; VAR ItemRect: RECT ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndHD, CC.HDM_GETITEMRECT, iIndex,
                                  SYSTEM.CAST(LPARAM, SYSTEM.ADR(ItemRect))));
END Header_GetItemRect;

PROCEDURE [DLS] Header_GetOrderArray ( hwndHD: HWND; iCount: INTEGER;
                                       VAR iArray: ARRAY OF INTEGER ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndHD, CC.HDM_GETORDERARRAY, iCount,
                                  SYSTEM.CAST(LPARAM, SYSTEM.ADR(iArray))));
END Header_GetOrderArray;

PROCEDURE [DLS] Header_GetUnicodeFormat ( hwndHD: HWND ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndHD, CC.HDM_GETUNICODEFORMAT, 0, 0));
END Header_GetUnicodeFormat;

PROCEDURE [DLS] Header_InsertItemA ( hwndHD: HWND; iIndex: INTEGER; hdi-: HDITEMA (*!*) ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SendMessageA(hwndHD, CC.HDM_INSERTITEMA, iIndex,
                                     SYSTEM.CAST(LPARAM, SYSTEM.ADR(hdi))));
END Header_InsertItemA;

PROCEDURE [DLS] Header_InsertItemW ( hwndHD: HWND; iIndex: INTEGER; hdi-: HDITEMW (*!*) ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SendMessageW(hwndHD, CC.HDM_INSERTITEMW, iIndex,
                                           SYSTEM.CAST(LPARAM, SYSTEM.ADR(hdi))));
END Header_InsertItemW;

PROCEDURE [DLS] Header_Layout ( hwndHD: HWND; VAR layout: HDLAYOUT ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndHD, CC.HDM_LAYOUT, 0,
                                  SYSTEM.CAST(LPARAM, SYSTEM.ADR(layout))));
END Header_Layout;

PROCEDURE [DLS] Header_OrderToIndex ( hwndHD: HWND; iOrder: INTEGER ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hwndHD, CC.HDM_ORDERTOINDEX, iOrder, 0));
END Header_OrderToIndex;

PROCEDURE [DLS] Header_SetHotDivider ( hwndHD: HWND; flag: BOOL; dwInputValue: DWORD ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hwndHD, CC.HDM_SETHOTDIVIDER,
                                     SYSTEM.CAST(WPARAM, flag), dwInputValue));
END Header_SetHotDivider;

PROCEDURE [DLS] Header_SetImageList ( hwndHD: HWND; himl: HIMAGELIST ): HIMAGELIST;
BEGIN
  RETURN SYSTEM.CAST(HIMAGELIST, SNDMSG(hwndHD, CC.HDM_SETIMAGELIST, 0, SYSTEM.CAST(LPARAM, himl)));
END Header_SetImageList;

PROCEDURE [DLS] Header_SetItemA ( hwndHD: HWND; iIndex: INTEGER; hdItem-: HDITEMA (*!*) ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageA(hwndHD, CC.HDM_SETITEMA, iIndex,
                                  SYSTEM.CAST(LPARAM, SYSTEM.ADR(hdItem))));
END Header_SetItemA;

PROCEDURE [DLS] Header_SetItemW ( hwndHD: HWND; iIndex: INTEGER; hdItem-: HDITEMW (*!*) ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageA(hwndHD, CC.HDM_SETITEMW, iIndex,
                                        SYSTEM.CAST(LPARAM, SYSTEM.ADR(hdItem))));
END Header_SetItemW;

PROCEDURE [DLS] Header_SetOrderArray ( hwndHD: HWND; iCount: INTEGER;
                                       iArray-: ARRAY OF INTEGER (*!*) ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndHD, CC.HDM_SETORDERARRAY, iCount,
                                  SYSTEM.CAST(LPARAM, SYSTEM.ADR(iArray))));
END Header_SetOrderArray;

PROCEDURE [DLS] Header_SetUnicodeFormat ( hwndHD: HWND; fUnicode: BOOL ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndHD, CC.HDM_SETUNICODEFORMAT, SYSTEM.CAST(WPARAM, fUnicode), 0));
END Header_SetUnicodeFormat;


(*  ============ ImageList ============= *)

PROCEDURE [DLS] ImageList_AddIcon ( himl: HIMAGELIST; hicon: HICON ): INTEGER;
BEGIN
  RETURN CC.ImageList_ReplaceIcon(himl, -1, hicon);
END ImageList_AddIcon;

PROCEDURE [DLS] ImageList_ExtractIcon ( hi: HINSTANCE; himl: HIMAGELIST; i: INTEGER ): HICON;
BEGIN
  RETURN CC.ImageList_GetIcon(himl, i, ILD_SET{});
END ImageList_ExtractIcon;

PROCEDURE [DLS] ImageList_LoadBitmapA ( hi: HINSTANCE; lpbmp: PCSTR;
                                       cx, cGrow: INTEGER; crMask: COLORREF ): HIMAGELIST;
BEGIN
  RETURN CC.ImageList_LoadImageA(hi, lpbmp, cx, cGrow, crMask, IMAGE_BITMAP, LR_SET{});
END ImageList_LoadBitmapA;

PROCEDURE [DLS] ImageList_LoadBitmapW ( hi: HINSTANCE; lpbmp: PCWSTR;
                                       cx, cGrow: INTEGER; crMask: COLORREF ): HIMAGELIST;
BEGIN
  RETURN CC.ImageList_LoadImageW(hi, lpbmp, cx, cGrow, crMask, IMAGE_BITMAP, LR_SET{});
END ImageList_LoadBitmapW;

PROCEDURE [DLS] ImageList_RemoveAll ( himl: HIMAGELIST ): BOOL;
BEGIN
  RETURN CC.ImageList_Remove(himl, -1);
END ImageList_RemoveAll;

PROCEDURE [DLS] INDEXTOOVERLAYMASK ( iOverlay: UINT ): BIT_SET;
BEGIN
  RETURN SYSTEM.SHIFT(SYSTEM.CAST(BIT_SET, iOverlay), 8);
END INDEXTOOVERLAYMASK;


(*  ============ IP Address ============= *)

PROCEDURE [DLS] MAKEIPRANGE ( low, high: BYTE ): LPARAM;
BEGIN
  RETURN SYSTEM.CAST(LPARAM, SYSTEM.SHIFT(SYSTEM.CAST(BITSET, VAL(DWORD, high)), 8) +
                             SYSTEM.CAST(BITSET, VAL(DWORD, low)));
END MAKEIPRANGE;

PROCEDURE [DLS] MAKEIPADDRESS ( b1, b2, b3, b4: BYTE ): LPARAM;
BEGIN
  RETURN SYSTEM.CAST(LPARAM, SYSTEM.SHIFT(SYSTEM.CAST(BITSET, VAL(DWORD, b1)), 24) +
                             SYSTEM.SHIFT(SYSTEM.CAST(BITSET, VAL(DWORD, b2)), 16) +
                             SYSTEM.SHIFT(SYSTEM.CAST(BITSET, VAL(DWORD, b3)), 8) +
                             SYSTEM.CAST(BITSET, VAL(DWORD, b4)));
END MAKEIPADDRESS;

PROCEDURE [DLS] FIRST_IPADDRESS ( lParam: LPARAM ): BYTE;
BEGIN
  RETURN SYSTEM.CAST(BYTE, SYSTEM.SHIFT(SYSTEM.CAST(BITSET, lParam), -24));
END FIRST_IPADDRESS;

PROCEDURE [DLS] SECOND_IPADDRESS ( lParam: LPARAM ): BYTE;
BEGIN
  RETURN SYSTEM.CAST(BYTE, SYSTEM.SHIFT(SYSTEM.CAST(BITSET, lParam), -16));
END SECOND_IPADDRESS;

PROCEDURE [DLS] THIRD_IPADDRESS ( lParam: LPARAM ): BYTE;
BEGIN
  RETURN SYSTEM.CAST(BYTE, SYSTEM.SHIFT(SYSTEM.CAST(BITSET, lParam), -8));
END THIRD_IPADDRESS;

PROCEDURE [DLS] FOURTH_IPADDRESS ( lParam: LPARAM ): BYTE;
BEGIN
  RETURN SYSTEM.CAST(BYTE, lParam);
END FOURTH_IPADDRESS;

(* ====== DRAG LIST CONTROL =========== *)

PROCEDURE [DLS] / LBItemFromPt ( hLB: HWND; pt: DWORD; bAutoScroll: BOOL ): INTEGER;

PROCEDURE [DLS] rtlLBItemFromPt ( hLB: HWND; pt: POINT; bAutoScroll: BOOL ): INTEGER;
BEGIN
    RETURN LBItemFromPt (hLB, SYSTEM.CAST (DWORD, pt), bAutoScroll);
END rtlLBItemFromPt;

(*  ============ ListView ============= *)

PROCEDURE [DLS] ListView_ApproximateViewRect ( hwndLV: HWND; cx, cy: INTEGER;
                                               iCount: INTEGER ): DWORD;
BEGIN
  RETURN SYSTEM.CAST(DWORD, SNDMSG(hwndLV, CC.LVM_APPROXIMATEVIEWRECT, iCount, MAKELPARAM(cx, cy)));
END ListView_ApproximateViewRect;

PROCEDURE [DLS] ListView_Arrange ( hwndLV: HWND; code: LVA_ENUM ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndLV, CC.LVM_ARRANGE, ORD(code), 0));
END ListView_Arrange;

PROCEDURE [DLS] ListView_CreateDragImage ( hwndLV: HWND; iItem: INTEGER;
                                           VAR ptUpLeft: POINT ): HIMAGELIST;
BEGIN
  RETURN SYSTEM.CAST(HIMAGELIST, SNDMSG(hwndLV, CC.LVM_CREATEDRAGIMAGE, iItem,
                                        SYSTEM.CAST(LPARAM, SYSTEM.ADR(ptUpLeft))));
END ListView_CreateDragImage;

PROCEDURE [DLS] ListView_DeleteAllItems ( hwndLV: HWND ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndLV, CC.LVM_DELETEALLITEMS, 0, 0));
END ListView_DeleteAllItems;

PROCEDURE [DLS] ListView_DeleteColumn ( hwndLV: HWND; iCol: INTEGER ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndLV, CC.LVM_DELETECOLUMN, iCol, 0));
END ListView_DeleteColumn;

PROCEDURE [DLS] ListView_DeleteItem ( hwndLV: HWND; iItem: INTEGER ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndLV, CC.LVM_DELETEITEM, iItem, 0));
END ListView_DeleteItem;

PROCEDURE [DLS] ListView_EditLabelA ( hwndLV: HWND; iItem: INTEGER ): HWND;
BEGIN
  RETURN SYSTEM.CAST(HWND, SendMessageA(hwndLV, CC.LVM_EDITLABELA, iItem, 0));
END ListView_EditLabelA;

PROCEDURE [DLS] ListView_EditLabelW ( hwndLV: HWND; iItem: INTEGER ): HWND;
BEGIN
  RETURN SYSTEM.CAST(HWND, SendMessageW(hwndLV, CC.LVM_EDITLABELW, iItem, 0));
END ListView_EditLabelW;

PROCEDURE [DLS] ListView_EnsureVisible ( hwndLV: HWND; iItem: INTEGER; fPartialOK: BOOL ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndLV, CC.LVM_ENSUREVISIBLE, iItem, SYSTEM.CAST(LPARAM, fPartialOK)));
END ListView_EnsureVisible;

PROCEDURE [DLS] ListView_FindItemA ( hwndLV: HWND; iStart: INTEGER; lvfi-: LVFINDINFOA (*!*) ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SendMessageA(hwndLV, CC.LVM_FINDITEMA, iStart,
                                           SYSTEM.CAST(LPARAM, SYSTEM.ADR(lvfi))));
END ListView_FindItemA;

PROCEDURE [DLS] ListView_FindItemW ( hwndLV: HWND; iStart: INTEGER; lvfi-: LVFINDINFOW (*!*) ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SendMessageW(hwndLV, CC.LVM_FINDITEMW, iStart,
                                           SYSTEM.CAST(LPARAM, SYSTEM.ADR(lvfi))));
END ListView_FindItemW;

PROCEDURE [DLS] ListView_GetBkColor ( hwndLV: HWND ): COLORREF;
BEGIN
  RETURN SYSTEM.CAST(COLORREF, SNDMSG(hwndLV, CC.LVM_GETBKCOLOR, 0, 0));
END ListView_GetBkColor;

PROCEDURE [DLS] ListView_GetBkImageA ( hwndLV: HWND; VAR lvbki: LVBKIMAGEA ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageA(hwndLV, CC.LVM_GETBKIMAGEA, 0,
                                        SYSTEM.CAST(LPARAM, SYSTEM.ADR(lvbki))));
END ListView_GetBkImageA;

PROCEDURE [DLS] ListView_GetBkImageW ( hwndLV: HWND; VAR lvbki: LVBKIMAGEW ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageW(hwndLV, CC.LVM_GETBKIMAGEW, 0,
                                        SYSTEM.CAST(LPARAM, SYSTEM.ADR(lvbki))));
END ListView_GetBkImageW;

PROCEDURE [DLS] ListView_GetCallbackMask ( hwndLV: HWND ): LVIS_SET;
BEGIN
  RETURN SYSTEM.CAST(LVIS_SET, SNDMSG(hwndLV, CC.LVM_GETCALLBACKMASK, 0, 0));
END ListView_GetCallbackMask;

PROCEDURE [DLS] ListView_GetCheckState ( hwndLV: HWND; iIndex: UINT ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SYSTEM.CAST(UINT, SYSTEM.SHIFT(SYSTEM.CAST(BITSET,
                           SNDMSG(hwndLV, CC.LVM_GETITEMSTATE, iIndex,
                                  SYSTEM.CAST(LPARAM, CC.LVIS_STATEIMAGEMASK))), -12)) -1);
END ListView_GetCheckState;

PROCEDURE [DLS] ListView_GetColumnA ( hwndLV: HWND; iCol: INTEGER; VAR col: LVCOLUMNA ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageA(hwndLV, CC.LVM_GETCOLUMNA, iCol,
                                        SYSTEM.CAST(LPARAM, SYSTEM.ADR(col))));
END ListView_GetColumnA;

PROCEDURE [DLS] ListView_GetColumnW ( hwndLV: HWND; iCol: INTEGER; VAR col: LVCOLUMNW ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageW(hwndLV, CC.LVM_GETCOLUMNW, iCol,
                                        SYSTEM.CAST(LPARAM, SYSTEM.ADR(col))));
END ListView_GetColumnW;

PROCEDURE [DLS] ListView_GetColumnOrderArray ( hwndLV: HWND; iCount: INTEGER;
                                               VAR iArray: ARRAY OF INTEGER ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndLV, CC.LVM_GETCOLUMNORDERARRAY, iCount,
                                  SYSTEM.CAST(LPARAM, SYSTEM.ADR(iArray))));
END ListView_GetColumnOrderArray;

PROCEDURE [DLS] ListView_GetColumnWidth ( hwndLV: HWND; iCol: INTEGER ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hwndLV, CC.LVM_GETCOLUMNWIDTH, iCol, 0));
END ListView_GetColumnWidth;

PROCEDURE [DLS] ListView_GetCountPerPage ( hwndLV: HWND ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hwndLV, CC.LVM_GETCOUNTPERPAGE, 0, 0));
END ListView_GetCountPerPage;

PROCEDURE [DLS] ListView_GetEditControl ( hwndLV: HWND ): HWND;
BEGIN
  RETURN SYSTEM.CAST(HWND, SNDMSG(hwndLV, CC.LVM_GETEDITCONTROL, 0, 0));
END ListView_GetEditControl;

PROCEDURE [DLS] ListView_GetExtendedListViewStyle ( hwndLV: HWND ): LVS_EX_SET;
BEGIN
  RETURN SYSTEM.CAST(LVS_EX_SET, SNDMSG(hwndLV, CC.LVM_GETEXTENDEDLISTVIEWSTYLE, 0, 0));
END ListView_GetExtendedListViewStyle;

PROCEDURE [DLS] ListView_GetHeader ( hwndLV: HWND ): HWND;
BEGIN
  RETURN SYSTEM.CAST(HWND, SNDMSG(hwndLV, CC.LVM_GETHEADER, 0, 0));
END ListView_GetHeader;

PROCEDURE [DLS] ListView_GetHotCursor ( hwndLV: HWND ): HCURSOR;
BEGIN
  RETURN SYSTEM.CAST(HCURSOR, SNDMSG(hwndLV, CC.LVM_GETHOTCURSOR, 0, 0));
END ListView_GetHotCursor;

PROCEDURE [DLS] ListView_GetHotItem ( hwndLV: HWND ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hwndLV, CC.LVM_GETHOTITEM, 0, 0));
END ListView_GetHotItem;

PROCEDURE [DLS] ListView_GetHoverTime ( hwndLV: HWND ): DWORD;
BEGIN
  RETURN SYSTEM.CAST(DWORD, SNDMSG(hwndLV, CC.LVM_GETHOVERTIME, 0, 0));
END ListView_GetHoverTime;

PROCEDURE [DLS] ListView_GetImageList ( hwndLV: HWND; iImageList: LVSIL_ENUM ): HIMAGELIST;
BEGIN
  RETURN SYSTEM.CAST(HIMAGELIST, SNDMSG(hwndLV, CC.LVM_GETIMAGELIST, ORD(iImageList), 0));
END ListView_GetImageList;

PROCEDURE [DLS] ListView_GetISearchStringA ( hwndLV: HWND; szText: PSTR ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageA(hwndLV, CC.LVM_GETISEARCHSTRINGA, 0, SYSTEM.CAST(LPARAM, szText)));
END ListView_GetISearchStringA;

PROCEDURE [DLS] ListView_GetISearchStringW ( hwndLV: HWND; szText: PWSTR ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageW(hwndLV, CC.LVM_GETISEARCHSTRINGW, 0, SYSTEM.CAST(LPARAM, szText)));
END ListView_GetISearchStringW;

PROCEDURE [DLS] ListView_GetItemA ( hwndLV: HWND; VAR item: LVITEMA ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageA(hwndLV, CC.LVM_GETITEMA, 0, SYSTEM.CAST(LPARAM, SYSTEM.ADR(item))));
END ListView_GetItemA;

PROCEDURE [DLS] ListView_GetItemW ( hwndLV: HWND; VAR item: LVITEMW ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageW(hwndLV, CC.LVM_GETITEMW, 0, SYSTEM.CAST(LPARAM, SYSTEM.ADR(item))));
END ListView_GetItemW;

PROCEDURE [DLS] ListView_GetItemCount ( hwndLV: HWND ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hwndLV, CC.LVM_GETITEMCOUNT, 0, 0));
END ListView_GetItemCount;

PROCEDURE [DLS] ListView_GetItemPosition ( hwndLV: HWND; iItem: INTEGER; VAR pt: POINT ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndLV, CC.LVM_GETITEMPOSITION, iItem, SYSTEM.CAST(LPARAM, SYSTEM.ADR(pt))));
END ListView_GetItemPosition;

PROCEDURE [DLS] ListView_GetItemRect ( hwndLV: HWND; iItem: INTEGER; VAR rc: RECT;
                                       code: LVIR_ENUM ): BOOL;
BEGIN
  rc.left := ORD(code);
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndLV, CC.LVM_GETITEMPOSITION, iItem, SYSTEM.CAST(LPARAM, SYSTEM.ADR(rc))));
END ListView_GetItemRect;

PROCEDURE [DLS] ListView_GetItemSpacing ( hwndLV: HWND; fSmall: BOOL ): DWORD;
BEGIN
  RETURN SYSTEM.CAST(DWORD, SNDMSG(hwndLV, CC.LVM_GETITEMSPACING, VAL(WPARAM, fSmall), 0));
END ListView_GetItemSpacing;

PROCEDURE [DLS] ListView_GetItemState ( hwndLV: HWND; iItem: INTEGER; mask: LVIS_SET ): LVIS_SET;
BEGIN
  RETURN SYSTEM.CAST(LVIS_SET, SNDMSG(hwndLV, CC.LVM_GETITEMSTATE, iItem, SYSTEM.CAST(LPARAM, mask)));
END ListView_GetItemState;

PROCEDURE [DLS] ListView_GetItemTextA ( hwndLV: HWND; iItem, iSubItem: INTEGER;
                                        VAR szText: ARRAY OF CHAR; cchTextMax: INTEGER );
VAR lvi: LVITEMA;
BEGIN
  lvi.iSubItem := iSubItem;
  lvi.cchTextMax := cchTextMax;
  lvi.pszText := SYSTEM.ADR(szText);
  SendMessageA(hwndLV, CC.LVM_GETITEMTEXTA, iItem, SYSTEM.CAST(LPARAM, SYSTEM.ADR(lvi)));
END ListView_GetItemTextA;

PROCEDURE [DLS] ListView_GetItemTextW ( hwndLV: HWND; iItem, iSubItem: INTEGER;
                                        VAR szText: ARRAY OF WCHAR; cchTextMax: INTEGER );
VAR lvi: LVITEMW;
BEGIN
  lvi.iSubItem := iSubItem;
  lvi.cchTextMax := cchTextMax;
  lvi.pszText := SYSTEM.ADR(szText);
  SendMessageW(hwndLV, CC.LVM_GETITEMTEXTW, iItem, SYSTEM.CAST(LPARAM, SYSTEM.ADR(lvi)));
END ListView_GetItemTextW;

PROCEDURE [DLS] ListView_GetNextItem ( hwndLV: HWND; iStart: INTEGER; flags: LVNI_SET ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hwndLV, CC.LVM_GETNEXTITEM, iStart, SYSTEM.CAST(LPARAM, flags)));
END ListView_GetNextItem;

PROCEDURE [DLS] ListView_GetNumberOfWorkAreas ( hwndLV: HWND; VAR WorkAreas: UINT ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndLV, CC.LVM_GETNUMBEROFWORKAREAS, 0,
                                  SYSTEM.CAST(LPARAM, SYSTEM.ADR(WorkAreas))));
END ListView_GetNumberOfWorkAreas;

PROCEDURE [DLS] ListView_GetOrigin ( hwndLV: HWND; VAR ptOrg: POINT ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndLV, CC.LVM_GETORIGIN, 0,
                                  SYSTEM.CAST(LPARAM, SYSTEM.ADR(ptOrg))));
END ListView_GetOrigin;

PROCEDURE [DLS] ListView_GetSelectedCount ( hwndLV: HWND ): UINT;
BEGIN
  RETURN SYSTEM.CAST(UINT, SNDMSG(hwndLV, CC.LVM_GETSELECTEDCOUNT, 0, 0));
END ListView_GetSelectedCount;

PROCEDURE [DLS] ListView_GetSelectionMark ( hwndLV: HWND ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hwndLV, CC.LVM_GETSELECTIONMARK, 0, 0));
END ListView_GetSelectionMark;

PROCEDURE [DLS] ListView_GetStringWidthA ( hwndLV: HWND; szText-: ARRAY OF CHAR ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SendMessageA(hwndLV, CC.LVM_GETSTRINGWIDTHA, 0, SYSTEM.CAST(LPARAM, SYSTEM.ADR(szText))));
END ListView_GetStringWidthA;

PROCEDURE [DLS] ListView_GetStringWidthW ( hwndLV: HWND; szText-: ARRAY OF WCHAR ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SendMessageW(hwndLV, CC.LVM_GETSTRINGWIDTHW, 0, SYSTEM.CAST(LPARAM, SYSTEM.ADR(szText))));
END ListView_GetStringWidthW;

PROCEDURE [DLS] ListView_GetSubItemRect ( hwndLV: HWND; iItem, iSubItem: INTEGER;
                                          code: LVIR_ENUM; VAR rc: RECT ): BOOL;
BEGIN
  rc.top := iSubItem;
  rc.left := ORD(code);
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndLV, CC.LVM_GETSUBITEMRECT, iItem, SYSTEM.CAST(LPARAM, SYSTEM.ADR(rc))));
END ListView_GetSubItemRect;

PROCEDURE [DLS] ListView_GetTextBkColor ( hwndLV: HWND ): COLORREF;
BEGIN
  RETURN SYSTEM.CAST(COLORREF, SNDMSG(hwndLV, CC.LVM_GETTEXTBKCOLOR, 0, 0));
END ListView_GetTextBkColor;

PROCEDURE [DLS] ListView_GetTextColor ( hwndLV: HWND ): COLORREF;
BEGIN
  RETURN SYSTEM.CAST(COLORREF, SNDMSG(hwndLV, CC.LVM_GETTEXTCOLOR, 0, 0));
END ListView_GetTextColor;

PROCEDURE [DLS] ListView_GetToolTips ( hwndLV: HWND ): HWND;
BEGIN
  RETURN SYSTEM.CAST(HWND, SNDMSG(hwndLV, CC.LVM_GETTOOLTIPS, 0, 0));
END ListView_GetToolTips;

PROCEDURE [DLS] ListView_GetTopIndex ( hwndLV: HWND ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hwndLV, CC.LVM_GETTOPINDEX, 0, 0));
END ListView_GetTopIndex;

PROCEDURE [DLS] ListView_GetUnicodeFormat ( hwndLV: HWND ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndLV, CC.LVM_GETUNICODEFORMAT, 0, 0));
END ListView_GetUnicodeFormat;

PROCEDURE [DLS] ListView_GetViewRect ( hwndLV: HWND; VAR rc: RECT ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndLV, CC.LVM_GETVIEWRECT, 0, SYSTEM.CAST(LPARAM, SYSTEM.ADR(rc))));
END ListView_GetViewRect;

PROCEDURE [DLS] ListView_GetWorkAreas ( hwndLV: HWND; nWorkAreas: INTEGER;
                                        VAR rc: ARRAY OF RECT );
BEGIN
  SNDMSG(hwndLV, CC.LVM_GETWORKAREAS, nWorkAreas, SYSTEM.CAST(LPARAM, SYSTEM.ADR(rc)));
END ListView_GetWorkAreas;

PROCEDURE [DLS] ListView_HitTest ( hwndLV: HWND; VAR info: LVHITTESTINFO ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hwndLV, CC.LVM_HITTEST, 0, SYSTEM.CAST(LPARAM, SYSTEM.ADR(info))));
END ListView_HitTest;

PROCEDURE [DLS] ListView_InsertColumnA ( hwndLV: HWND; iCol: INTEGER;
                                         col-: LVCOLUMNA (*!*) ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SendMessageA(hwndLV, CC.LVM_INSERTCOLUMNA, iCol, SYSTEM.CAST(LPARAM, SYSTEM.ADR(col))));
END ListView_InsertColumnA;

PROCEDURE [DLS] ListView_InsertColumnW ( hwndLV: HWND; iCol: INTEGER;
                                         col-: LVCOLUMNW (*!*) ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SendMessageW(hwndLV, CC.LVM_INSERTCOLUMNW, iCol, SYSTEM.CAST(LPARAM, SYSTEM.ADR(col))));
END ListView_InsertColumnW;

PROCEDURE [DLS] ListView_InsertItemA ( hwndLV: HWND; item-: LVITEMA (*!*) ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SendMessageA(hwndLV, CC.LVM_INSERTITEMA, 0, SYSTEM.CAST(LPARAM, SYSTEM.ADR(item))));
END ListView_InsertItemA;

PROCEDURE [DLS] ListView_InsertItemW ( hwndLV: HWND; item-: LVITEMW (*!*) ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SendMessageW(hwndLV, CC.LVM_INSERTITEMW, 0, SYSTEM.CAST(LPARAM, SYSTEM.ADR(item))));
END ListView_InsertItemW;

PROCEDURE [DLS] ListView_RedrawItems ( hwndLV: HWND; iFirst, iLast: INTEGER ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndLV, CC.LVM_REDRAWITEMS, iFirst, iLast));
END ListView_RedrawItems;

PROCEDURE [DLS] ListView_Scroll ( hwndLV: HWND; dx, dy: INTEGER ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndLV, CC.LVM_SCROLL, dx, dy));
END ListView_Scroll;

PROCEDURE [DLS] ListView_SetBkColor ( hwndLV: HWND; clrBk: COLORREF ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndLV, CC.LVM_SETBKCOLOR, 0, SYSTEM.CAST(LPARAM, clrBk)));
END ListView_SetBkColor;

PROCEDURE [DLS] ListView_SetBkImageA ( hwndLV: HWND; lvbki-: LVBKIMAGEA (*!*) ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageA(hwndLV, CC.LVM_SETBKIMAGEA, 0, SYSTEM.CAST(LPARAM, SYSTEM.ADR(lvbki))));
END ListView_SetBkImageA;

PROCEDURE [DLS] ListView_SetBkImageW ( hwndLV: HWND; lvbki-: LVBKIMAGEW (*!*) ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageW(hwndLV, CC.LVM_SETBKIMAGEW, 0, SYSTEM.CAST(LPARAM, SYSTEM.ADR(lvbki))));
END ListView_SetBkImageW;

PROCEDURE [DLS] ListView_SetCallbackMask ( hwndLV: HWND; mask: LVIS_SET ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndLV, CC.LVM_SETCALLBACKMASK, SYSTEM.CAST(WPARAM, mask), 0));
END ListView_SetCallbackMask;

PROCEDURE [DLS] ListView_SetColumnA ( hwndLV: HWND; iCol: INTEGER; col-: LVCOLUMNA (*!*) ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageA(hwndLV, CC.LVM_SETCOLUMNA, iCol, SYSTEM.CAST(LPARAM, SYSTEM.ADR(col))));
END ListView_SetColumnA;

PROCEDURE [DLS] ListView_SetColumnW ( hwndLV: HWND; iCol: INTEGER; col-: LVCOLUMNW (*!*) ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageW(hwndLV, CC.LVM_SETCOLUMNW, iCol, SYSTEM.CAST(LPARAM, SYSTEM.ADR(col))));
END ListView_SetColumnW;

PROCEDURE [DLS] ListView_SetColumnOrderArray ( hwndLV: HWND; iCount: INTEGER;
                                               iArray-: ARRAY OF INTEGER (*!*) ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndLV, CC.LVM_SETCOLUMNORDERARRAY, iCount,
                                  SYSTEM.CAST(LPARAM, SYSTEM.ADR(iArray))));
END ListView_SetColumnOrderArray;

PROCEDURE [DLS] ListView_SetColumnWidth ( hwndLV: HWND; iCol: INTEGER; cx: INTEGER ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndLV, CC.LVM_SETCOLUMNWIDTH, iCol, cx));
END ListView_SetColumnWidth;

PROCEDURE [DLS] ListView_SetExtendedListViewStyle ( hwndLV: HWND; dwExStyle: LVS_EX_SET ): LVS_EX_SET;
BEGIN
  RETURN SYSTEM.CAST(LVS_EX_SET, SNDMSG(hwndLV, CC.LVM_SETEXTENDEDLISTVIEWSTYLE, 0, SYSTEM.CAST(LPARAM, dwExStyle)));
END ListView_SetExtendedListViewStyle;

PROCEDURE [DLS] ListView_SetExtendedListViewStyleEx ( hwndLV: HWND; dwExMask, dwExStyle: LVS_EX_SET ): LVS_EX_SET;
BEGIN
  RETURN SYSTEM.CAST(LVS_EX_SET, SNDMSG(hwndLV, CC.LVM_SETEXTENDEDLISTVIEWSTYLE,
                                        SYSTEM.CAST(WPARAM, dwExMask), SYSTEM.CAST(LPARAM, dwExStyle)));
END ListView_SetExtendedListViewStyleEx;

PROCEDURE [DLS] ListView_SetHotCursor ( hwndLV: HWND; hCursor: HCURSOR ): HCURSOR;
BEGIN
  RETURN SYSTEM.CAST(HCURSOR, SNDMSG(hwndLV, CC.LVM_SETHOTCURSOR, 0, SYSTEM.CAST(LPARAM, hCursor)));
END ListView_SetHotCursor;

PROCEDURE [DLS] ListView_SetHotItem ( hwndLV: HWND; iIndex: INTEGER ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hwndLV, CC.LVM_SETHOTITEM, iIndex, 0));
END ListView_SetHotItem;

PROCEDURE [DLS] ListView_SetHoverTime ( hwndLV: HWND; dwHoverTime: DWORD ): DWORD;
BEGIN
  RETURN SYSTEM.CAST(DWORD, SNDMSG(hwndLV, CC.LVM_SETHOVERTIME, 0, dwHoverTime));
END ListView_SetHoverTime;

PROCEDURE [DLS] ListView_SetIconSpacing ( hwndLV: HWND; cx, cy: INTEGER ): DWORD;
BEGIN
  RETURN SYSTEM.CAST(DWORD, SNDMSG(hwndLV, CC.LVM_SETICONSPACING, 0, MAKELONG(cx, cy)));
END ListView_SetIconSpacing;

PROCEDURE [DLS] ListView_SetImageList ( hwndLV: HWND; himl: HIMAGELIST;
                                        iImageList: LVSIL_ENUM ): HIMAGELIST;
BEGIN
  RETURN SYSTEM.CAST(HIMAGELIST, SNDMSG(hwndLV, CC.LVM_SETIMAGELIST, ORD(iImageList), SYSTEM.CAST(LPARAM, himl)));
END ListView_SetImageList;

PROCEDURE [DLS] ListView_SetItemA ( hwndLV: HWND; item-: LVITEMA (*!*) ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageA(hwndLV, CC.LVM_SETITEMA, 0, SYSTEM.CAST(LPARAM, SYSTEM.ADR(item))));
END ListView_SetItemA;

PROCEDURE [DLS] ListView_SetItemW ( hwndLV: HWND; item-: LVITEMW (*!*) ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageW(hwndLV, CC.LVM_SETITEMW, 0, SYSTEM.CAST(LPARAM, SYSTEM.ADR(item))));
END ListView_SetItemW;

PROCEDURE [DLS] ListView_SetItemCount ( hwndLV: HWND; cItems: INTEGER ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndLV, CC.LVM_SETITEMCOUNT, cItems, 0));
END ListView_SetItemCount;

PROCEDURE [DLS] ListView_SetItemCountEx ( hwndLV: HWND; cItems: INTEGER; dwFlags: LVSICF_SET ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndLV, CC.LVM_SETITEMCOUNT, cItems, SYSTEM.CAST(LPARAM, dwFlags)));
END ListView_SetItemCountEx;

PROCEDURE [DLS] ListView_SetItemPosition ( hwndLV: HWND; iItem: INTEGER; x, y: INTEGER ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndLV, CC.LVM_SETITEMPOSITION, iItem, MAKELPARAM(x, y)));
END ListView_SetItemPosition;

PROCEDURE [DLS] ListView_SetItemPosition32 ( hwndLV: HWND; iItem: INTEGER; x, y: INTEGER );
VAR ptNewPos: POINT;
BEGIN
  ptNewPos.x := x;
  ptNewPos.y := y;
  SNDMSG(hwndLV, CC.LVM_SETITEMPOSITION32, iItem, SYSTEM.CAST(LPARAM, SYSTEM.ADR(ptNewPos)));
END ListView_SetItemPosition32;

PROCEDURE [DLS] ListView_SetItemState ( hwndLV: HWND; iItem: INTEGER; state, mask: LVIS_SET ): BOOL;
VAR lvi: LVITEMA;
BEGIN
  lvi.stateMask := mask;
  lvi.state := state;
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndLV, CC.LVM_SETITEMSTATE, iItem,
                                  SYSTEM.CAST(LPARAM, SYSTEM.ADR(lvi))));
END ListView_SetItemState;

PROCEDURE [DLS] ListView_SetItemTextA ( hwndLV: HWND; iItem, iSubItem: INTEGER; szText: PCSTR ): BOOL;
VAR lvi: LVITEMA;
BEGIN
  lvi.iSubItem := iSubItem;
  lvi.pszText := szText;
  RETURN SYSTEM.CAST(BOOL, SendMessageA(hwndLV, CC.LVM_SETITEMTEXTA, iItem,
                                        SYSTEM.CAST(LPARAM, SYSTEM.ADR(lvi))));
END ListView_SetItemTextA;

PROCEDURE [DLS] ListView_SetItemTextW ( hwndLV: HWND; iItem, iSubItem: INTEGER; szText: PCWSTR ): BOOL;
VAR lvi: LVITEMW;
BEGIN
  lvi.iSubItem := iSubItem;
  lvi.pszText := szText;
  RETURN SYSTEM.CAST(BOOL, SendMessageW(hwndLV, CC.LVM_SETITEMTEXTW, iItem,
                                        SYSTEM.CAST(LPARAM, SYSTEM.ADR(lvi))));
END ListView_SetItemTextW;

PROCEDURE [DLS] ListView_SetSelectionMark ( hwndLV: HWND; iIndex: INTEGER ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hwndLV, CC.LVM_SETSELECTIONMARK, 0, iIndex));
END ListView_SetSelectionMark;

PROCEDURE [DLS] ListView_SetTextBkColor ( hwndLV: HWND; clrText: COLORREF ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndLV, CC.LVM_SETTEXTBKCOLOR, 0, SYSTEM.CAST(LPARAM, clrText)));
END ListView_SetTextBkColor;

PROCEDURE [DLS] ListView_SetTextColor ( hwndLV: HWND; clrText: COLORREF ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndLV, CC.LVM_SETTEXTCOLOR, 0, SYSTEM.CAST(LPARAM, clrText)));
END ListView_SetTextColor;

PROCEDURE [DLS] ListView_SetToolTips ( hwndLV: HWND; hwndToolTip: HWND ): HWND;
BEGIN
  RETURN SYSTEM.CAST(HWND, SNDMSG(hwndLV, CC.LVM_SETTOOLTIPS, SYSTEM.CAST(WPARAM, hwndToolTip), 0));
END ListView_SetToolTips;

PROCEDURE [DLS] ListView_SetUnicodeFormat ( hwndLV: HWND; fUnicode: BOOL ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndLV, CC.LVM_SETUNICODEFORMAT, SYSTEM.CAST(WPARAM, fUnicode), 0));
END ListView_SetUnicodeFormat;

PROCEDURE [DLS] ListView_SetWorkAreas ( hwndLV: HWND; nWorkAreas: INTEGER; prc: PCRECT );
BEGIN
  SNDMSG(hwndLV, CC.LVM_SETWORKAREAS, nWorkAreas, SYSTEM.CAST(LPARAM, prc));
END ListView_SetWorkAreas;

PROCEDURE [DLS] ListView_SortItems ( hwndLV: HWND; pfnCompare: PFNLVCOMPARE;
                                     lParamSort: LPARAM ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndLV, CC.LVM_SORTITEMS, lParamSort, SYSTEM.CAST(LPARAM, pfnCompare)));
END ListView_SortItems;

PROCEDURE [DLS] ListView_SubItemHitTest ( hwndLV: HWND; VAR Info: LVHITTESTINFO ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hwndLV, CC.LVM_SUBITEMHITTEST, 0,
                                     SYSTEM.CAST(LPARAM, SYSTEM.ADR(Info))));
END ListView_SubItemHitTest;

PROCEDURE [DLS] ListView_Update ( hwndLV: HWND; iItem: INTEGER ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndLV, CC.LVM_UPDATE, iItem, 0));
END ListView_Update;

PROCEDURE [DLS] INDEXTOSTATEIMAGEMASK ( i: UINT ): BIT_SET;
BEGIN
  RETURN SYSTEM.SHIFT(SYSTEM.CAST(BIT_SET, i), 12);
END INDEXTOSTATEIMAGEMASK;


(*  ============ MonthCal ============= *)

PROCEDURE [DLS] MonthCal_GetColor ( hwndMC: HWND; iColor: MCSC_ENUM ): COLORREF;
BEGIN
  RETURN SYSTEM.CAST(COLORREF, SNDMSG(hwndMC, CC.MCM_SETCOLOR, ORD(iColor), 0));
END MonthCal_GetColor;

PROCEDURE [DLS] MonthCal_GetCurSel ( hwndMC: HWND; VAR SysTime: SYSTEMTIME ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndMC, CC.MCM_GETCURSEL, 0, SYSTEM.CAST(LPARAM, SYSTEM.ADR(SysTime))));
END MonthCal_GetCurSel;

PROCEDURE [DLS] MonthCal_GetFirstDayOfWeek ( hwndMC: HWND ): DWORD;
BEGIN
  RETURN SYSTEM.CAST(DWORD, SNDMSG(hwndMC, CC.MCM_GETFIRSTDAYOFWEEK, 0, 0));
END MonthCal_GetFirstDayOfWeek;

PROCEDURE [DLS] MonthCal_GetMaxSelCount ( hwndMC: HWND ): DWORD;
BEGIN
  RETURN SYSTEM.CAST(DWORD, SNDMSG(hwndMC, CC.MCM_GETMAXSELCOUNT, 0, 0));
END MonthCal_GetMaxSelCount;

PROCEDURE [DLS] MonthCal_GetMaxTodayWidth ( hwndMC: HWND ): DWORD;
BEGIN
  RETURN SYSTEM.CAST(DWORD, SNDMSG(hwndMC, CC.MCM_GETMAXTODAYWIDTH, 0, 0));
END MonthCal_GetMaxTodayWidth;

PROCEDURE [DLS] MonthCal_GetMinReqRect ( hwndMC: HWND; VAR RectInfo: RECT ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndMC, CC.MCM_GETMINREQRECT, 0, SYSTEM.CAST(LPARAM, SYSTEM.ADR(RectInfo))));
END MonthCal_GetMinReqRect;

PROCEDURE [DLS] MonthCal_GetMonthDelta ( hwndMC: HWND ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hwndMC, CC.MCM_GETMONTHDELTA, 0, 0));
END MonthCal_GetMonthDelta;

PROCEDURE [DLS] MonthCal_GetMonthRange ( hwndMC: HWND; dwFlag: GMR_ENUM;
                                         rgSysTimeArray: PSYSTEMTIME (* NIL *)): DWORD;
BEGIN
  RETURN SYSTEM.CAST(DWORD, SNDMSG(hwndMC, CC.MCM_GETMONTHRANGE, ORD(dwFlag),
                                   SYSTEM.CAST(LPARAM, rgSysTimeArray)));
END MonthCal_GetMonthRange;

PROCEDURE [DLS] MonthCal_GetRange ( hwndMC: HWND; VAR rgSysTimeArray: ARRAY OF SYSTEMTIME ): GDTR_SET;
BEGIN
  RETURN SYSTEM.CAST(GDTR_SET, SNDMSG(hwndMC, CC.MCM_GETRANGE, 0,
                                      SYSTEM.CAST(LPARAM, SYSTEM.ADR(rgSysTimeArray))));
END MonthCal_GetRange;

PROCEDURE [DLS] MonthCal_GetSelRange ( hwndMC: HWND; VAR rgSysTimeArray: ARRAY OF SYSTEMTIME ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndMC, CC.MCM_GETSELRANGE, 0,
                                  SYSTEM.CAST(LPARAM, SYSTEM.ADR(rgSysTimeArray))));
END MonthCal_GetSelRange;

PROCEDURE [DLS] MonthCal_GetToday ( hwndMC: HWND; VAR Today: SYSTEMTIME ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndMC, CC.MCM_GETTODAY, 0,
                                  SYSTEM.CAST(LPARAM, SYSTEM.ADR(Today))));
END MonthCal_GetToday;

PROCEDURE [DLS] MonthCal_GetUnicodeFormat ( hwndMC: HWND ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndMC, CC.MCM_GETUNICODEFORMAT, 0, 0));
END MonthCal_GetUnicodeFormat;

PROCEDURE [DLS] MonthCal_HitTest ( hwndMC: HWND; VAR MCHitTest: MCHITTESTINFO ): DWORD;
BEGIN
  RETURN SYSTEM.CAST(DWORD, SNDMSG(hwndMC, CC.MCM_HITTEST, 0,
                                   SYSTEM.CAST(LPARAM, SYSTEM.ADR(MCHitTest))));
END MonthCal_HitTest;

PROCEDURE [DLS] MonthCal_SetColor ( hwndMC: HWND; iColor: MCSC_ENUM; clr: COLORREF ): COLORREF;
BEGIN
  RETURN SYSTEM.CAST(COLORREF, SNDMSG(hwndMC, CC.MCM_SETCOLOR, ORD(iColor), SYSTEM.CAST(LPARAM, clr)));
END MonthCal_SetColor;

PROCEDURE [DLS] MonthCal_SetCurSel ( hwndMC: HWND; SysTime-: SYSTEMTIME (*!*) ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndMC, CC.MCM_SETCURSEL, 0,
                                  SYSTEM.CAST(LPARAM, SYSTEM.ADR(SysTime))));
END MonthCal_SetCurSel;

PROCEDURE [DLS] MonthCal_SetDayState ( hwndMC: HWND; iMonths: INTEGER;
                                       DayStateArray-: ARRAY OF MONTHDAYSTATE (*!*) ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndMC, CC.MCM_SETDAYSTATE, iMonths,
                                  SYSTEM.CAST(LPARAM, SYSTEM.ADR(DayStateArray))));
END MonthCal_SetDayState;

PROCEDURE [DLS] MonthCal_SetFirstDayOfWeek ( hwndMC: HWND; iDay: INTEGER ): DWORD;
BEGIN
  RETURN SYSTEM.CAST(DWORD, SNDMSG(hwndMC, CC.MCM_SETFIRSTDAYOFWEEK, 0, iDay));
END MonthCal_SetFirstDayOfWeek;

PROCEDURE [DLS] MonthCal_SetMaxSelCount ( hwndMC: HWND; iMax: UINT ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndMC, CC.MCM_SETMAXSELCOUNT, iMax, 0));
END MonthCal_SetMaxSelCount;

PROCEDURE [DLS] MonthCal_SetMonthDelta ( hwndMC: HWND; iDelta: INTEGER ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hwndMC, CC.MCM_SETMONTHDELTA, iDelta, 0));
END MonthCal_SetMonthDelta;

PROCEDURE [DLS] MonthCal_SetRange ( hwndMC: HWND; fWhichLimit: GDTR_SET;
                                    rgSysTimeArray-: ARRAY OF SYSTEMTIME (*!*) ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndMC, CC.MCM_SETRANGE, SYSTEM.CAST(WPARAM, fWhichLimit),
                                  SYSTEM.CAST(LPARAM, SYSTEM.ADR(rgSysTimeArray))));
END MonthCal_SetRange;

PROCEDURE [DLS] MonthCal_SetSelRange ( hwndMC: HWND; rgSysTimeArray-: ARRAY OF SYSTEMTIME (*!*) ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndMC, CC.MCM_SETSELRANGE, 0,
                                  SYSTEM.CAST(LPARAM, SYSTEM.ADR(rgSysTimeArray))));
END MonthCal_SetSelRange;

PROCEDURE [DLS] MonthCal_SetToday ( hwndMC: HWND; pSysTime: PSYSTEMTIME );
BEGIN
  SNDMSG(hwndMC, CC.MCM_SETTODAY, 0, SYSTEM.CAST(LPARAM, pSysTime));
END MonthCal_SetToday;

PROCEDURE [DLS] MonthCal_SetUnicodeFormat ( hwndMC: HWND; fUnicode: BOOL ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndMC, CC.MCM_SETUNICODEFORMAT, SYSTEM.CAST(WPARAM, fUnicode), 0));
END MonthCal_SetUnicodeFormat;


(*  ============ Pager ============= *)

PROCEDURE [DLS] Pager_ForwardMouse ( hwndPager: HWND; bForward: BOOL );
BEGIN
  SNDMSG(hwndPager, CC.PGM_FORWARDMOUSE, SYSTEM.CAST(WPARAM, bForward), 0);
END Pager_ForwardMouse;

PROCEDURE [DLS] Pager_GetBkColor ( hwndPager: HWND ): COLORREF;
BEGIN
  RETURN SYSTEM.CAST(COLORREF, SNDMSG(hwndPager, CC.PGM_GETBKCOLOR, 0, 0));
END Pager_GetBkColor;

PROCEDURE [DLS] Pager_GetBorder ( hwndPager: HWND ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hwndPager, CC.PGM_GETBORDER, 0, 0));
END Pager_GetBorder;

PROCEDURE [DLS] Pager_GetButtonSize ( hwndPager: HWND ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hwndPager, CC.PGM_GETBUTTONSIZE, 0, 0));
END Pager_GetButtonSize;

PROCEDURE [DLS] Pager_GetButtonState ( hwndPager: HWND; iButton: PGB_ENUM ): DWORD;
BEGIN
  RETURN SYSTEM.CAST(DWORD, SNDMSG(hwndPager, CC.PGM_GETBUTTONSTATE, 0, ORD(iButton)));
END Pager_GetButtonState;

PROCEDURE [DLS] Pager_GetDropTarget ( hwndPager: HWND; VAR DropTarget: SYSTEM.ADDRESS );
BEGIN
  SNDMSG(hwndPager, CC.PGM_GETDROPTARGET, 0, SYSTEM.CAST(LPARAM, SYSTEM.ADR(DropTarget)));
END Pager_GetDropTarget;

PROCEDURE [DLS] Pager_GetPos ( hwndPager: HWND ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hwndPager, CC.PGM_GETPOS, 0, 0));
END Pager_GetPos;

PROCEDURE [DLS] Pager_RecalcSize ( hwndPager: HWND );
BEGIN
  SNDMSG(hwndPager, CC.PGM_RECALCSIZE, 0, 0);
END Pager_RecalcSize;

PROCEDURE [DLS] Pager_SetBkColor ( hwndPager: HWND; clrBk: COLORREF ): COLORREF;
BEGIN
  RETURN SYSTEM.CAST(COLORREF, SNDMSG(hwndPager, CC.PGM_SETBKCOLOR, 0, SYSTEM.CAST(LPARAM, clrBk)));
END Pager_SetBkColor;

PROCEDURE [DLS] Pager_SetBorder ( hwndPager: HWND; iBorder: INTEGER ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hwndPager, CC.PGM_SETBORDER, 0, iBorder));
END Pager_SetBorder;

PROCEDURE [DLS] Pager_SetButtonSize ( hwndPager: HWND; iButtonSize: INTEGER ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hwndPager, CC.PGM_SETBUTTONSIZE, 0, iButtonSize));
END Pager_SetButtonSize;

PROCEDURE [DLS] Pager_SetChild ( hwndPager: HWND; hwndChild: HWND );
BEGIN
  SNDMSG(hwndPager, CC.PGM_SETCHILD, 0, SYSTEM.CAST(LPARAM, hwndChild));
END Pager_SetChild;

PROCEDURE [DLS] Pager_SetPos ( hwndPager: HWND; iPos: INTEGER ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hwndPager, CC.PGM_SETPOS, 0, iPos));
END Pager_SetPos;


(*  ============ PropSheet ============ *)

PROCEDURE [DLS] PropSheet_AddPage ( hPropSheetDlg: HWND; hpage: HPROPSHEETPAGE ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hPropSheetDlg, CC.PSM_ADDPAGE, 0, SYSTEM.CAST(LPARAM, hpage)));
END PropSheet_AddPage;

PROCEDURE [DLS] PropSheet_Apply ( hPropSheetDlg: HWND ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hPropSheetDlg, CC.PSM_APPLY, 0, 0));
END PropSheet_Apply;

PROCEDURE [DLS] PropSheet_CancelToClose ( hPropSheetDlg: HWND );
BEGIN
  PostMessage(hPropSheetDlg, CC.PSM_CANCELTOCLOSE, 0, 0);
END PropSheet_CancelToClose;

PROCEDURE [DLS] PropSheet_Changed ( hPropSheetDlg: HWND; hwndPage: HWND ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hPropSheetDlg, CC.PSM_CHANGED, SYSTEM.CAST(WPARAM, hwndPage), 0));
END PropSheet_Changed;

PROCEDURE [DLS] PropSheet_GetCurrentPageHwnd ( hPropSheetDlg: HWND ): HWND;
BEGIN
  RETURN SYSTEM.CAST(HWND, SNDMSG(hPropSheetDlg, CC.PSM_GETCURRENTPAGEHWND, 0, 0));
END PropSheet_GetCurrentPageHwnd;

PROCEDURE [DLS] PropSheet_GetTabControl ( hPropSheetDlg: HWND ): HWND;
BEGIN
  RETURN SYSTEM.CAST(HWND, SNDMSG(hPropSheetDlg, CC.PSM_GETTABCONTROL, 0, 0));
END PropSheet_GetTabControl;

PROCEDURE [DLS] PropSheet_IsDialogMessage ( hPropSheetDlg: HWND; VAR Msg: MSG ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hPropSheetDlg, CC.PSM_ISDIALOGMESSAGE, 0,
                                  SYSTEM.CAST(LPARAM, SYSTEM.ADR(Msg))));
END PropSheet_IsDialogMessage;

PROCEDURE [DLS] PropSheet_PressButton ( hPropSheetDlg: HWND; iButton: PSBTN_ENUM ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, PostMessage(hPropSheetDlg, CC.PSM_PRESSBUTTON, SYSTEM.CAST(WPARAM, iButton), 0));
END PropSheet_PressButton;

PROCEDURE [DLS] PropSheet_QuerySiblings ( hPropSheetDlg: HWND; wParam: WPARAM; lParam: LPARAM ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hPropSheetDlg, CC.PSM_QUERYSIBLINGS, wParam, lParam));
END PropSheet_QuerySiblings;

PROCEDURE [DLS] PropSheet_RebootSystem ( hPropSheetDlg: HWND );
BEGIN
  SNDMSG(hPropSheetDlg, CC.PSM_REBOOTSYSTEM, 0, 0);
END PropSheet_RebootSystem;

PROCEDURE [DLS] PropSheet_RemovePage ( hPropSheetDlg: HWND; index: INTEGER; hpage: HPROPSHEETPAGE );
BEGIN
  SNDMSG(hPropSheetDlg, CC.PSM_REMOVEPAGE, index, SYSTEM.CAST(LPARAM, hpage));
END PropSheet_RemovePage;

PROCEDURE [DLS] PropSheet_RestartWindows ( hPropSheetDlg: HWND );
BEGIN
  SNDMSG(hPropSheetDlg, CC.PSM_RESTARTWINDOWS, 0, 0);
END PropSheet_RestartWindows;

PROCEDURE [DLS] PropSheet_SetCurSel ( hPropSheetDlg: HWND; hpage: HPROPSHEETPAGE; index: INTEGER ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hPropSheetDlg, CC.PSM_SETCURSEL, index, SYSTEM.CAST(LPARAM, hpage)));
END PropSheet_SetCurSel;

PROCEDURE [DLS] PropSheet_SetCurSelByID ( hPropSheetDlg: HWND; id: INTEGER ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hPropSheetDlg, CC.PSM_SETCURSELID, 0, id));
END PropSheet_SetCurSelByID;

PROCEDURE [DLS] PropSheet_SetFinishTextA ( hPropSheetDlg: HWND; szText: PCSTR (* NIL *) );
BEGIN
  SendMessageA(hPropSheetDlg, CC.PSM_SETFINISHTEXTA, 0, SYSTEM.CAST(LPARAM, szText));
END PropSheet_SetFinishTextA;

PROCEDURE [DLS] PropSheet_SetFinishTextW ( hPropSheetDlg: HWND; szText: PCWSTR (* NIL *) );
BEGIN
  SendMessageW(hPropSheetDlg, CC.PSM_SETFINISHTEXTW, 0, SYSTEM.CAST(LPARAM, szText));
END PropSheet_SetFinishTextW;

PROCEDURE [DLS] PropSheet_SetTitleA ( hPropSheetDlg: HWND; dwStyle: PSH_SET; szText: PCSTR (* NIL *) );
BEGIN
  SendMessageA(hPropSheetDlg, CC.PSM_SETTITLEA, SYSTEM.CAST(WPARAM, dwStyle), SYSTEM.CAST(LPARAM, szText));
END PropSheet_SetTitleA;

PROCEDURE [DLS] PropSheet_SetTitleW ( hPropSheetDlg: HWND; dwStyle: PSH_SET; szText: PCWSTR (* NIL *) );
BEGIN
  SendMessageW(hPropSheetDlg, CC.PSM_SETTITLEW, SYSTEM.CAST(WPARAM, dwStyle), SYSTEM.CAST(LPARAM, szText));
END PropSheet_SetTitleW;

PROCEDURE [DLS] PropSheet_SetWizButtons ( hPropSheetDlg: HWND; dwFlags: PSWIZB_SET );
BEGIN
  PostMessage(hPropSheetDlg, CC.PSM_SETWIZBUTTONS, 0, SYSTEM.CAST(LPARAM, dwFlags));
END PropSheet_SetWizButtons;

PROCEDURE [DLS] PropSheet_UnChanged ( hPropSheetDlg: HWND; hwndPage: HWND );
BEGIN
  SNDMSG(hPropSheetDlg, CC.PSM_UNCHANGED, SYSTEM.CAST(WPARAM, hwndPage), 0);
END PropSheet_UnChanged;


(*  ============ TabCtrl ============= *)

PROCEDURE [DLS] TabCtrl_AdjustRect ( hwndTab: HWND; fLarger: BOOL; VAR rc: RECT );
BEGIN
  SNDMSG(hwndTab, CC.TCM_ADJUSTRECT, SYSTEM.CAST(WPARAM, fLarger), SYSTEM.CAST(LPARAM, SYSTEM.ADR(rc)));
END TabCtrl_AdjustRect;

PROCEDURE [DLS] TabCtrl_DeleteAllItems ( hwndTab: HWND ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndTab, CC.TCM_DELETEALLITEMS, 0, 0));
END TabCtrl_DeleteAllItems;

PROCEDURE [DLS] TabCtrl_DeleteItem ( hwndTab: HWND; iItem: INTEGER ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndTab, CC.TCM_DELETEITEM, iItem, 0));
END TabCtrl_DeleteItem;

PROCEDURE [DLS] TabCtrl_DeselectAll ( hwndTab: HWND; fExcludeFocus: BOOL );
BEGIN
  SNDMSG(hwndTab, CC.TCM_DESELECTALL, SYSTEM.CAST(WPARAM, fExcludeFocus), 0);
END TabCtrl_DeselectAll;

PROCEDURE [DLS] TabCtrl_GetCurFocus ( hwndTab: HWND ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hwndTab, CC.TCM_GETCURFOCUS, 0, 0));
END TabCtrl_GetCurFocus;

PROCEDURE [DLS] TabCtrl_GetCurSel ( hwndTab: HWND ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hwndTab, CC.TCM_GETCURSEL, 0, 0));
END TabCtrl_GetCurSel;

PROCEDURE [DLS] TabCtrl_GetExtendedStyle ( hwndTab: HWND ): TCS_EX_SET;
BEGIN
  RETURN SYSTEM.CAST(TCS_EX_SET, SNDMSG(hwndTab, CC.TCM_GETEXTENDEDSTYLE, 0, 0));
END TabCtrl_GetExtendedStyle;

PROCEDURE [DLS] TabCtrl_GetImageList ( hwndTab: HWND ): HIMAGELIST;
BEGIN
  RETURN SYSTEM.CAST(HIMAGELIST, SNDMSG(hwndTab, CC.TCM_GETIMAGELIST, 0, 0));
END TabCtrl_GetImageList;

PROCEDURE [DLS] TabCtrl_GetItemA ( hwndTab: HWND; iItem: INTEGER; VAR item: TCITEMA ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageA(hwndTab, CC.TCM_GETITEMA, iItem, SYSTEM.CAST(LPARAM, SYSTEM.ADR(item))));
END TabCtrl_GetItemA;

PROCEDURE [DLS] TabCtrl_GetItemW ( hwndTab: HWND; iItem: INTEGER; VAR item: TCITEMW ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageW(hwndTab, CC.TCM_GETITEMW, iItem, SYSTEM.CAST(LPARAM, SYSTEM.ADR(item))));
END TabCtrl_GetItemW;

PROCEDURE [DLS] TabCtrl_GetItemCount ( hwndTab: HWND ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hwndTab, CC.TCM_GETITEMCOUNT, 0, 0));
END TabCtrl_GetItemCount;

PROCEDURE [DLS] TabCtrl_GetItemRect ( hwndTab: HWND; iItem: INTEGER; VAR rc: RECT ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndTab, CC.TCM_GETITEMRECT, iItem, SYSTEM.CAST(LPARAM, SYSTEM.ADR(rc))));
END TabCtrl_GetItemRect;

PROCEDURE [DLS] TabCtrl_GetRowCount ( hwndTab: HWND ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hwndTab, CC.TCM_GETROWCOUNT, 0, 0));
END TabCtrl_GetRowCount;

PROCEDURE [DLS] TabCtrl_GetToolTips ( hwndTab: HWND ): HWND;
BEGIN
  RETURN SYSTEM.CAST(HWND, SNDMSG(hwndTab, CC.TCM_GETTOOLTIPS, 0, 0));
END TabCtrl_GetToolTips;

PROCEDURE [DLS] TabCtrl_GetUnicodeFormat ( hwndTab: HWND ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndTab, CC.TCM_GETUNICODEFORMAT, 0, 0));
END TabCtrl_GetUnicodeFormat;

PROCEDURE [DLS] TabCtrl_HighlightItem ( hwndTab: HWND; iItem: INTEGER; fHighlight: BOOL ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndTab, CC.TCM_HIGHLIGHTITEM, iItem, SYSTEM.CAST(LPARAM, fHighlight)));
END TabCtrl_HighlightItem;

PROCEDURE [DLS] TabCtrl_HitTest ( hwndTab: HWND; VAR info: TCHITTESTINFO ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hwndTab, CC.TCM_HITTEST, 0, SYSTEM.CAST(LPARAM, SYSTEM.ADR(info))));
END TabCtrl_HitTest;

PROCEDURE [DLS] TabCtrl_InsertItemA ( hwndTab: HWND; iItem: INTEGER; item-: TCITEMA (*!*) ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SendMessageA(hwndTab, CC.TCM_INSERTITEMA, iItem, SYSTEM.CAST(LPARAM, SYSTEM.ADR(item))));
END TabCtrl_InsertItemA;

PROCEDURE [DLS] TabCtrl_InsertItemW ( hwndTab: HWND; iItem: INTEGER; item-: TCITEMW (*!*) ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SendMessageW(hwndTab, CC.TCM_INSERTITEMW, iItem, SYSTEM.CAST(LPARAM, SYSTEM.ADR(item))));
END TabCtrl_InsertItemW;

PROCEDURE [DLS] TabCtrl_RemoveImage ( hwndTab: HWND; iImage: INTEGER );
BEGIN
  SNDMSG(hwndTab, CC.TCM_REMOVEIMAGE, iImage, 0);
END TabCtrl_RemoveImage;

PROCEDURE [DLS] TabCtrl_SetCurFocus ( hwndTab: HWND; iItem: INTEGER );
BEGIN
  SNDMSG(hwndTab, CC.TCM_SETCURFOCUS, iItem, 0);
END TabCtrl_SetCurFocus;

PROCEDURE [DLS] TabCtrl_SetCurSel ( hwndTab: HWND; iItem: INTEGER ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hwndTab, CC.TCM_SETCURSEL, iItem, 0));
END TabCtrl_SetCurSel;

PROCEDURE [DLS] TabCtrl_SetExtendedStyle ( hwndTab: HWND; dwExStyle: TCS_EX_SET ): TCS_EX_SET;
BEGIN
  RETURN SYSTEM.CAST(TCS_EX_SET, SNDMSG(hwndTab, CC.TCM_SETEXTENDEDSTYLE, 0, SYSTEM.CAST(LPARAM, dwExStyle)));
END TabCtrl_SetExtendedStyle;

PROCEDURE [DLS] TabCtrl_SetImageList ( hwndTab: HWND; himl: HIMAGELIST ): HIMAGELIST;
BEGIN
  RETURN SYSTEM.CAST(HIMAGELIST, SNDMSG(hwndTab, CC.TCM_SETIMAGELIST, 0, SYSTEM.CAST(LPARAM, himl)));
END TabCtrl_SetImageList;

PROCEDURE [DLS] TabCtrl_SetItemA ( hwndTab: HWND; iItem: INTEGER; item-: TCITEMA (*!*) ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageA(hwndTab, CC.TCM_SETITEMA, iItem, SYSTEM.CAST(LPARAM, SYSTEM.ADR(item))));
END TabCtrl_SetItemA;

PROCEDURE [DLS] TabCtrl_SetItemW ( hwndTab: HWND; iItem: INTEGER; item-: TCITEMW (*!*) ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageW(hwndTab, CC.TCM_SETITEMW, iItem, SYSTEM.CAST(LPARAM, SYSTEM.ADR(item))));
END TabCtrl_SetItemW;

PROCEDURE [DLS] TabCtrl_SetItemExtra ( hwndTab: HWND; cb: INTEGER ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndTab, CC.TCM_SETITEMEXTRA, cb, 0));
END TabCtrl_SetItemExtra;

PROCEDURE [DLS] TabCtrl_SetItemSize ( hwndTab: HWND; cx, cy: INTEGER ): DWORD;
BEGIN
  RETURN SYSTEM.CAST(DWORD, SNDMSG(hwndTab, CC.TCM_SETITEMSIZE, 0, MAKELPARAM(cx, cy)));
END TabCtrl_SetItemSize;

PROCEDURE [DLS] TabCtrl_SetMinTabWidth ( hwndTab: HWND; cx: INTEGER ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hwndTab, CC.TCM_SETMINTABWIDTH, 0, cx));
END TabCtrl_SetMinTabWidth;

PROCEDURE [DLS] TabCtrl_SetPadding ( hwndTab: HWND; cx, cy: INTEGER );
BEGIN
  SNDMSG(hwndTab, CC.TCM_SETPADDING, 0, MAKELPARAM(cx, cy));
END TabCtrl_SetPadding;

PROCEDURE [DLS] TabCtrl_SetToolTips ( hwndTab: HWND; hwndTT: HWND );
BEGIN
  SNDMSG(hwndTab, CC.TCM_SETTOOLTIPS, SYSTEM.CAST(WPARAM, hwndTT), 0);
END TabCtrl_SetToolTips;

PROCEDURE [DLS] TabCtrl_SetUnicodeFormat ( hwndTab: HWND; fUnicode: BOOL ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndTab, CC.TCM_SETUNICODEFORMAT, SYSTEM.CAST(WPARAM, fUnicode), 0));
END TabCtrl_SetUnicodeFormat;


(*  ============ TreeView ============= *)

PROCEDURE [DLS] TreeView_CreateDragImage ( hwndTV: HWND; hitem: HTREEITEM ): HIMAGELIST;
BEGIN
  RETURN SYSTEM.CAST(HIMAGELIST, SNDMSG(hwndTV, CC.TVM_CREATEDRAGIMAGE, 0, SYSTEM.CAST(LPARAM, hitem)));
END TreeView_CreateDragImage;

PROCEDURE [DLS] TreeView_DeleteAllItems ( hwndTV: HWND ): BOOL;
BEGIN
  RETURN TreeView_DeleteItem(hwndTV, CC.TVI_ROOT);
END TreeView_DeleteAllItems;

PROCEDURE [DLS] TreeView_DeleteItem ( hwndTV: HWND; hitem: HTREEITEM ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndTV, CC.TVM_DELETEITEM, 0, SYSTEM.CAST(LPARAM, hitem)));
END TreeView_DeleteItem;

PROCEDURE [DLS] TreeView_EditLabelA ( hwndTV: HWND; hitem: HTREEITEM ): HWND;
BEGIN
  RETURN SYSTEM.CAST(HWND, SendMessageA(hwndTV, CC.TVM_EDITLABELA, 0, SYSTEM.CAST(LPARAM, hitem)));
END TreeView_EditLabelA;

PROCEDURE [DLS] TreeView_EditLabelW ( hwndTV: HWND; hitem: HTREEITEM ): HWND;
BEGIN
  RETURN SYSTEM.CAST(HWND, SendMessageW(hwndTV, CC.TVM_EDITLABELW, 0, SYSTEM.CAST(LPARAM, hitem)));
END TreeView_EditLabelW;

PROCEDURE [DLS] TreeView_EndEditLabelNow ( hwndTV: HWND; fCancel: BOOL ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndTV, CC.TVM_ENDEDITLABELNOW, SYSTEM.CAST(WPARAM, fCancel), 0));
END TreeView_EndEditLabelNow;

PROCEDURE [DLS] TreeView_EnsureVisible ( hwndTV: HWND; hitem: HTREEITEM ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndTV, CC.TVM_ENSUREVISIBLE, 0, SYSTEM.CAST(LPARAM, hitem)));
END TreeView_EnsureVisible;

PROCEDURE [DLS] TreeView_Expand ( hwndTV: HWND; hItem: HTREEITEM; flag: TVE_SET ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndTV, CC.TVM_EXPAND, SYSTEM.CAST(WPARAM, flag), SYSTEM.CAST(LPARAM, hItem)));
END TreeView_Expand;

PROCEDURE [DLS] TreeView_GetBkColor ( hwndTV: HWND ): COLORREF;
BEGIN
  RETURN SYSTEM.CAST(COLORREF, SNDMSG(hwndTV, CC.TVM_GETBKCOLOR, 0, 0));
END TreeView_GetBkColor;

PROCEDURE [DLS] TreeView_GetChild ( hwndTV: HWND; hitem: HTREEITEM ): HTREEITEM;
BEGIN
  RETURN TreeView_GetNextItem(hwndTV, hitem, CC.TVGN_CHILD);
END TreeView_GetChild;

PROCEDURE [DLS] TreeView_GetCount ( hwndTV: HWND ): UINT;
BEGIN
  RETURN SYSTEM.CAST(UINT, SNDMSG(hwndTV, CC.TVM_GETCOUNT, 0, 0));
END TreeView_GetCount;

PROCEDURE [DLS] TreeView_GetDropHilight ( hwndTV: HWND ): HTREEITEM;
BEGIN
  RETURN TreeView_GetNextItem(hwndTV, NIL, CC.TVGN_DROPHILITE);
END TreeView_GetDropHilight;

PROCEDURE [DLS] TreeView_GetEditControl ( hwndTV: HWND ): HWND;
BEGIN
  RETURN SYSTEM.CAST(HWND, SNDMSG(hwndTV, CC.TVM_GETEDITCONTROL, 0, 0));
END TreeView_GetEditControl;

PROCEDURE [DLS] TreeView_GetFirstVisible ( hwndTV: HWND ): HTREEITEM;
BEGIN
  RETURN TreeView_GetNextItem(hwndTV, NIL, CC.TVGN_FIRSTVISIBLE);
END TreeView_GetFirstVisible;

PROCEDURE [DLS] TreeView_GetImageList ( hwndTV: HWND; iImage: INTEGER ): HIMAGELIST;
BEGIN
  RETURN SYSTEM.CAST(HIMAGELIST, SNDMSG(hwndTV, CC.TVM_GETIMAGELIST, iImage, 0));
END TreeView_GetImageList;

PROCEDURE [DLS] TreeView_GetIndent ( hwndTV: HWND ): UINT;
BEGIN
  RETURN SYSTEM.CAST(UINT, SNDMSG(hwndTV, CC.TVM_GETINDENT, 0, 0));
END TreeView_GetIndent;

PROCEDURE [DLS] TreeView_GetInsertMarkColor ( hwndTV: HWND ): COLORREF;
BEGIN
  RETURN SYSTEM.CAST(COLORREF, SNDMSG(hwndTV, CC.TVM_GETINSERTMARKCOLOR, 0, 0));
END TreeView_GetInsertMarkColor;

PROCEDURE [DLS] TreeView_GetISearchStringA ( hwndTV: HWND; szText: PSTR (* NIL *) ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageA(hwndTV, CC.TVM_GETISEARCHSTRINGA, 0, SYSTEM.CAST(LPARAM, szText)));
END TreeView_GetISearchStringA;

PROCEDURE [DLS] TreeView_GetISearchStringW ( hwndTV: HWND; szText: PWSTR (* NIL *) ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageW(hwndTV, CC.TVM_GETISEARCHSTRINGW, 0, SYSTEM.CAST(LPARAM, szText)));
END TreeView_GetISearchStringW;

PROCEDURE [DLS] TreeView_GetItemA ( hwndTV: HWND; VAR item: TVITEMA ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageA(hwndTV, CC.TVM_GETITEMA, 0, SYSTEM.CAST(LPARAM, SYSTEM.ADR(item))));
END TreeView_GetItemA;

PROCEDURE [DLS] TreeView_GetItemW ( hwndTV: HWND; VAR item: TVITEMW ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageW(hwndTV, CC.TVM_GETITEMW, 0, SYSTEM.CAST(LPARAM, SYSTEM.ADR(item))));
END TreeView_GetItemW;

PROCEDURE [DLS] TreeView_GetItemHeight ( hwndTV: HWND ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hwndTV, CC.TVM_GETITEMHEIGHT, 0, 0));
END TreeView_GetItemHeight;

PROCEDURE [DLS] TreeView_GetItemRect ( hwndTV: HWND; hitem: HTREEITEM; VAR rc: RECT;
                                       fItemRect: BOOL ): BOOL;
BEGIN
  rc.left := SYSTEM.CAST(INTEGER, hitem);
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndTV, CC.TVM_GETITEMRECT, SYSTEM.CAST(WPARAM, fItemRect),
                                  SYSTEM.CAST(LPARAM, SYSTEM.ADR(rc))));
END TreeView_GetItemRect;

PROCEDURE [DLS] TreeView_GetLastVisible ( hwndTV: HWND ): HTREEITEM;
BEGIN
  RETURN TreeView_GetNextItem(hwndTV, NIL, CC.TVGN_LASTVISIBLE);
END TreeView_GetLastVisible;

PROCEDURE [DLS] TreeView_GetNextItem ( hwndTV: HWND; hitem: HTREEITEM;
                                       flag: TVGN_ENUM ): HTREEITEM;
BEGIN
  RETURN SYSTEM.CAST(HTREEITEM, SNDMSG(hwndTV, CC.TVM_GETNEXTITEM, ORD(flag), SYSTEM.CAST(LPARAM, hitem)));
END TreeView_GetNextItem;

PROCEDURE [DLS] TreeView_GetNextSibling ( hwndTV: HWND; hitem: HTREEITEM ): HTREEITEM;
BEGIN
  RETURN TreeView_GetNextItem(hwndTV, hitem, CC.TVGN_NEXT);
END TreeView_GetNextSibling;

PROCEDURE [DLS] TreeView_GetNextVisible ( hwndTV: HWND; hitem: HTREEITEM ): HTREEITEM;
BEGIN
  RETURN TreeView_GetNextItem(hwndTV, hitem, CC.TVGN_NEXTVISIBLE);
END TreeView_GetNextVisible;

PROCEDURE [DLS] TreeView_GetParent ( hwndTV: HWND; hitem: HTREEITEM ): HTREEITEM;
BEGIN
  RETURN TreeView_GetNextItem(hwndTV, hitem, CC.TVGN_PARENT);
END TreeView_GetParent;

PROCEDURE [DLS] TreeView_GetPrevSibling ( hwndTV: HWND; hitem: HTREEITEM ): HTREEITEM;
BEGIN
  RETURN TreeView_GetNextItem(hwndTV, hitem, CC.TVGN_PREVIOUS);
END TreeView_GetPrevSibling;

PROCEDURE [DLS] TreeView_GetPrevVisible ( hwndTV: HWND; hitem: HTREEITEM ): HTREEITEM;
BEGIN
  RETURN TreeView_GetNextItem(hwndTV, hitem, CC.TVGN_PREVIOUSVISIBLE);
END TreeView_GetPrevVisible;

PROCEDURE [DLS] TreeView_GetRoot ( hwndTV: HWND ): HTREEITEM;
BEGIN
  RETURN TreeView_GetNextItem(hwndTV, NIL, CC.TVGN_ROOT);
END TreeView_GetRoot;

PROCEDURE [DLS] TreeView_GetScrollTime ( hwndTV: HWND ): UINT;
BEGIN
  RETURN SYSTEM.CAST(UINT, SNDMSG(hwndTV, CC.TVM_GETSCROLLTIME, 0, 0));
END TreeView_GetScrollTime;

PROCEDURE [DLS] TreeView_GetSelection ( hwndTV: HWND ): HTREEITEM;
BEGIN
  RETURN TreeView_GetNextItem(hwndTV, NIL, CC.TVGN_CARET);
END TreeView_GetSelection;

PROCEDURE [DLS] TreeView_GetTextColor ( hwndTV: HWND ): COLORREF;
BEGIN
  RETURN SYSTEM.CAST(COLORREF, SNDMSG(hwndTV, CC.TVM_GETTEXTCOLOR, 0, 0));
END TreeView_GetTextColor;

PROCEDURE [DLS] TreeView_GetToolTips ( hwndTV: HWND ): HWND;
BEGIN
  RETURN SYSTEM.CAST(HWND, SNDMSG(hwndTV, CC.TVM_GETTOOLTIPS, 0, 0));
END TreeView_GetToolTips;

PROCEDURE [DLS] TreeView_GetUnicodeFormat ( hwndTV: HWND ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndTV, CC.TVM_GETUNICODEFORMAT, 0, 0));
END TreeView_GetUnicodeFormat;

PROCEDURE [DLS] TreeView_GetVisibleCount ( hwndTV: HWND ): UINT;
BEGIN
  RETURN SYSTEM.CAST(UINT, SNDMSG(hwndTV, CC.TVM_GETVISIBLECOUNT, 0, 0));
END TreeView_GetVisibleCount;

PROCEDURE [DLS] TreeView_HitTest ( hwndTV: HWND; VAR ht: TVHITTESTINFO ): HTREEITEM;
BEGIN
  RETURN SYSTEM.CAST(HTREEITEM, SNDMSG(hwndTV, CC.TVM_HITTEST, 0, SYSTEM.CAST(LPARAM, SYSTEM.ADR(ht))));
END TreeView_HitTest;

PROCEDURE [DLS] TreeView_InsertItemA ( hwndTV: HWND; is-: TVINSERTSTRUCTA (*!*) ): HTREEITEM;
BEGIN
  RETURN SYSTEM.CAST(HTREEITEM, SendMessageA(hwndTV, CC.TVM_INSERTITEMA, 0, SYSTEM.CAST(LPARAM, SYSTEM.ADR(is))));
END TreeView_InsertItemA;

PROCEDURE [DLS] TreeView_InsertItemW ( hwndTV: HWND; is-: TVINSERTSTRUCTW (*!*) ): HTREEITEM;
BEGIN
  RETURN SYSTEM.CAST(HTREEITEM, SendMessageW(hwndTV, CC.TVM_INSERTITEMW, 0, SYSTEM.CAST(LPARAM, SYSTEM.ADR(is))));
END TreeView_InsertItemW;

PROCEDURE [DLS] TreeView_Select ( hwndTV: HWND; hitem: HTREEITEM; flag: TVGN_ENUM ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndTV, CC.TVM_SELECTITEM, ORD(flag), SYSTEM.CAST(LPARAM, hitem)));
END TreeView_Select;

PROCEDURE [DLS] TreeView_SelectDropTarget ( hwndTV: HWND; hitem: HTREEITEM ): BOOL;
BEGIN
  RETURN TreeView_Select(hwndTV, hitem, CC.TVGN_DROPHILITE);
END TreeView_SelectDropTarget;

PROCEDURE [DLS] TreeView_SelectItem ( hwndTV: HWND; hitem: HTREEITEM ): BOOL;
BEGIN
  RETURN TreeView_Select(hwndTV, hitem, CC.TVGN_CARET);
END TreeView_SelectItem;

PROCEDURE [DLS] TreeView_SelectSetFirstVisible ( hwndTV: HWND; hitem: HTREEITEM ): BOOL;
BEGIN
  RETURN TreeView_Select(hwndTV, hitem, CC.TVGN_FIRSTVISIBLE);
END TreeView_SelectSetFirstVisible;

PROCEDURE [DLS] TreeView_SetBkColor ( hwndTV: HWND; clrBk: COLORREF ): COLORREF;
BEGIN
  RETURN SYSTEM.CAST(COLORREF, SNDMSG(hwndTV, CC.TVM_SETBKCOLOR, 0, SYSTEM.CAST(LPARAM, clrBk)));
END TreeView_SetBkColor;

PROCEDURE [DLS] TreeView_SetImageList ( hwndTV: HWND; himl: HIMAGELIST; iImage: INTEGER ): HIMAGELIST;
BEGIN
  RETURN SYSTEM.CAST(HIMAGELIST, SNDMSG(hwndTV, CC.TVM_SETIMAGELIST, iImage, SYSTEM.CAST(LPARAM, himl)));
END TreeView_SetImageList;

PROCEDURE [DLS] TreeView_SetIndent ( hwndTV: HWND; indent: INTEGER ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndTV, CC.TVM_SETINDENT, indent, 0));
END TreeView_SetIndent;

PROCEDURE [DLS] TreeView_SetInsertMark ( hwndTV: HWND; htiInsert: HTREEITEM;
                                         fAfter: BOOL ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndTV, CC.TVM_SETINSERTMARK, SYSTEM.CAST(WPARAM, fAfter),
                                  SYSTEM.CAST(LPARAM, htiInsert)));
END TreeView_SetInsertMark;

PROCEDURE [DLS] TreeView_SetInsertMarkColor ( hwndTV: HWND; clrInsertMark: COLORREF ): COLORREF;
BEGIN
  RETURN SYSTEM.CAST(COLORREF, SNDMSG(hwndTV, CC.TVM_SETINSERTMARKCOLOR, 0,
                                      SYSTEM.CAST(LPARAM, clrInsertMark)));
END TreeView_SetInsertMarkColor;

PROCEDURE [DLS] TreeView_SetItemA ( hwndTV: HWND; item-: TVITEMA (*!*) ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageA(hwndTV, CC.TVM_SETITEMA, 0,
                                        SYSTEM.CAST(LPARAM, SYSTEM.ADR(item))));
END TreeView_SetItemA;

PROCEDURE [DLS] TreeView_SetItemW ( hwndTV: HWND; item-: TVITEMW (*!*) ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SendMessageW(hwndTV, CC.TVM_SETITEMW, 0,
                                        SYSTEM.CAST(LPARAM, SYSTEM.ADR(item))));
END TreeView_SetItemW;

PROCEDURE [DLS] TreeView_SetItemHeight ( hwndTV: HWND; cyItem: INTEGER ): INTEGER;
BEGIN
  RETURN SYSTEM.CAST(INTEGER, SNDMSG(hwndTV, CC.TVM_SETITEMHEIGHT, cyItem, 0));
END TreeView_SetItemHeight;

PROCEDURE [DLS] TreeView_SetScrollTime ( hwndTV: HWND; uMaxScrollTime: UINT ): UINT;
BEGIN
  RETURN SYSTEM.CAST(UINT, SNDMSG(hwndTV, CC.TVM_SETSCROLLTIME, uMaxScrollTime, 0));
END TreeView_SetScrollTime;

PROCEDURE [DLS] TreeView_SetTextColor ( hwndTV: HWND; clrText: COLORREF ): COLORREF;
BEGIN
  RETURN SYSTEM.CAST(COLORREF, SNDMSG(hwndTV, CC.TVM_SETTEXTCOLOR, 0, SYSTEM.CAST(LPARAM, clrText)));
END TreeView_SetTextColor;

PROCEDURE [DLS] TreeView_SetToolTips ( hwndTV: HWND; hwndTooltip: HWND ): HWND;
BEGIN
  RETURN SYSTEM.CAST(HWND, SNDMSG(hwndTV, CC.TVM_SETTOOLTIPS, SYSTEM.CAST(WPARAM, hwndTooltip), 0));
END TreeView_SetToolTips;

PROCEDURE [DLS] TreeView_SetUnicodeFormat ( hwndTV: HWND; fUnicode: BOOL ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndTV, CC.TVM_SETUNICODEFORMAT, SYSTEM.CAST(WPARAM, fUnicode), 0));
END TreeView_SetUnicodeFormat;

PROCEDURE [DLS] TreeView_SortChildren ( hwndTV: HWND; hitem: HTREEITEM;
                                        fRecurse: BOOL ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndTV, CC.TVM_SORTCHILDREN, SYSTEM.CAST(WPARAM, fRecurse),
                                  SYSTEM.CAST(LPARAM, hitem)));
END TreeView_SortChildren;

PROCEDURE [DLS] TreeView_SortChildrenCB ( hwndTV: HWND; sort-: TVSORTCB (*!*);
                                          fRecurse: BOOL ): BOOL;
BEGIN
  RETURN SYSTEM.CAST(BOOL, SNDMSG(hwndTV, CC.TVM_SORTCHILDRENCB, SYSTEM.CAST(WPARAM, fRecurse),
                                  SYSTEM.CAST(LPARAM, SYSTEM.ADR(sort))));
END TreeView_SortChildrenCB;


END CCtlRTL.
