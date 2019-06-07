(*  *FSA - Dec 1998 *)

IMPLEMENTATION MODULE OutMon;

<* +STORAGE   *>
<* -IOVERFLOW *>
<* -COVERFLOW *>


IMPORT SYSTEM, Strings, IOChan, IOLink, ChanConsts, StdChans;
IMPORT O := OS2;

FROM   Storage IMPORT ALLOCATE;
FROM   SYSTEM  IMPORT ADR, SET32, SET16, ADDRESS;

CONST
  MONCLASSNAME  = "STDOUT_MONITOR";
  ITEM_FONT     = 10;
  MM_START      = O.WM_USER+1;
  MM_CHANGEFONT = O.WM_USER+2;
  MM_POSTSEM    = O.WM_USER+3;

TYPE
<*  PUSH *>
<* +WOFF324 *>
  PROFILE_RECORD = RECORD
    fAttrs       : O.FATTRS;
    x, y, cx, cy : INTEGER;
  END;
<* POP *>

VAR
  bFirst       : BOOLEAN;
  pwpEdDefault : O.PFNWP;
  fAttrs       : O.FATTRS;
  hAB          : O.HAB;
  fcdata       : O.FRAMECDATA;
  hMsgQ        : O.HMQ;
  qMsg         : O.QMSG;
  hevReady     : O.HEV;
  win_tid      : O.TID;
(*=================== Catch out: ====================================================*)

TYPE
  pAr = POINTER TO ARRAY [0..0FFFFh] OF CHAR;

VAR
  ipt                   : O.IPT;
  buf                   : POINTER TO ARRAY OF CHAR;
  hevOutTxt             : O.HEV;

PROCEDURE app_txt(p : pAr; cb : CARDINAL);
VAR
  l  : CARDINAL;
  ul : O.ULONG;
BEGIN
  O.DosWaitEventSem(hevOutTxt, O.SEM_INDEFINITE_WAIT);
  O.DosResetEventSem(hevOutTxt, ul);
  IF (hEd = 0) OR (cb = 0)THEN RETURN; END;
  IF (buf # NIL) THEN DISPOSE(buf); END;
  NEW(buf,cb);
  FOR l:=0 TO cb-1 DO buf^[l] := p^[l]; END;
  O.WinPostMsg(hEd, O.MLM_SETIMPORTEXPORT, O.MPARAM(ADR(buf^[0])), O.MPARAM(cb));
  O.WinPostMsg(hEd, O.MLM_IMPORT,          O.MPARAM(ADR(ipt)),     O.MPARAM(cb));
  O.WinPostMsg(hEd, O.MLM_SETSEL,          O.MPARAM(ipt),          O.MPARAM(ipt));
  O.WinPostMsg(hEd, MM_POSTSEM, NIL, NIL);             -- hevOutTxt posted inside
END app_txt;


VAR
  did :IOLink.DeviceId;
  cid :IOChan.ChanId;

PROCEDURE ISOrawWrite ( pDT :IOLink.DeviceTablePtr; src :ADDRESS; len :CARDINAL );
BEGIN
  app_txt(pAr(src), len);
END ISOrawWrite;

VAR ch:CHAR;

PROCEDURE ISOtextWrite ( pDT :IOLink.DeviceTablePtr; src :ADDRESS; len :CARDINAL );
VAR
BEGIN
  app_txt(pAr(src), len);
END ISOtextWrite;



PROCEDURE ISOWrLn ( pDT :IOLink.DeviceTablePtr );
VAR
  lf : ARRAY [0..0] OF CHAR;
BEGIN
  lf[0] := CHAR(0ah);
  app_txt(ADR(lf), 1);
END ISOWrLn;



PROCEDURE CatchOut();
VAR
  pt  :IOLink.DeviceTablePtr;
BEGIN
  O.DosCreateEventSem(NIL, hevOutTxt, 0, TRUE);
  IOLink.AllocateDeviceId ( did );
  IOLink.MakeChan ( did, cid );
  IF ( cid = IOChan.InvalidChan() ) THEN RETURN END;

  pt := IOLink.DeviceTablePtrValue ( cid, did, IOChan.notAvailable, "" );

  pt^.flags       := ChanConsts.text + ChanConsts.raw + ChanConsts.write;
  pt^.doRawWrite  := ISOrawWrite;
  pt^.doTextWrite := ISOtextWrite;
  pt^.doLnWrite   := ISOWrLn;

  StdChans.SetOutChan ( cid );

END CatchOut;


PROCEDURE ReleaseOut();
BEGIN
--  IOLink.UnMakeChan (did,cid);
--  IOLink.AllocateDeviceId ( did );
END ReleaseOut;


(*==========================================================================*)



PROCEDURE wrProfile();
VAR
  prfRec : PROFILE_RECORD;
  swp    : O.SWP;
BEGIN
  IF (hEd # 0) THEN
    prfRec.fAttrs := fAttrs;
    O.WinQueryWindowPos(hFrame, swp);
    IF (SET32(swp.fl) * SET32(O.SWP_MINIMIZE) # SET32{}) THEN
      O.WinSetWindowPos  (hFrame,0,0,0,0,0,O.SWP_RESTORE);
      O.WinQueryWindowPos(hFrame, swp);
    END;

    IF (SET32(swp.fl) * SET32(O.SWP_MAXIMIZE) # SET32{}) THEN
      prfRec.x  := O.WinQueryWindowUShort(hFrame, O.QWS_XRESTORE);
      prfRec.y  := O.WinQueryWindowUShort(hFrame, O.QWS_YRESTORE);
      prfRec.cx := O.WinQueryWindowUShort(hFrame, O.QWS_CYRESTORE);
      prfRec.cy := O.WinQueryWindowUShort(hFrame, O.QWS_CXRESTORE);
    ELSE
      prfRec.x  := swp.x;
      prfRec.y  := swp.y;
      prfRec.cx := swp.cx;
      prfRec.cy := swp.cy;
    END;

    O.PrfWriteProfileData(O.HINI_USERPROFILE,"XDSDbg","STDMonitorPos",ADR(prfRec), SIZE(PROFILE_RECORD));
  END;
END wrProfile;


PROCEDURE rdProfile();
VAR
  prfRec : PROFILE_RECORD;
  l      : CARDINAL;
BEGIN
  WITH fAttrs DO
    usRecordLength  := SIZE(O.FATTRS);
    fsSelection     := 0;
    lMatch          := 82;
    Strings.Assign("System VIO",szFacename);
    idRegistry      := 0;
    usCodePage      := 0;
    lMaxBaselineExt := 14;
    lAveCharWidth   := 8;
    fsType          := 0;
    fsFontUse       := O.FATTR_FONTUSE_NOMIX;
  END;

  l := SIZE(PROFILE_RECORD);
  IF (O.PrfQueryProfileData(O.HINI_USERPROFILE,"XDSDbg","STDMonitorPos",ADR(prfRec),l)
     AND (l = SIZE(PROFILE_RECORD))) THEN
    fAttrs := prfRec.fAttrs;
  ELSE
    prfRec.x  := 100;
    prfRec.y  := 100;
    prfRec.cx := 400;
    prfRec.cy := 400;
  END;

  O.WinSetWindowPos(hFrame,0,prfRec.x,prfRec.y,prfRec.cx,prfRec.cy,O.SWP_MOVE+O.SWP_SIZE+O.SWP_SHOW+O.SWP_ACTIVATE);
  O.WinPostMsg(hEd,MM_CHANGEFONT,NIL,NIL);
END rdProfile;

PROCEDURE FontDlg():BOOLEAN;
-- cahnges global fAttrs
VAR
  fontDlg     : O.FONTDLG;
  szFamily    : ARRAY [0..100] OF CHAR;
  fontMetrics : O.FONTMETRICS;
  hpsEd       : O.HPS;
BEGIN
  SYSTEM.FILL (ADR(fontDlg), 0, SIZE(O.FONTDLG));

  O.WinSendMsg(hEd,O.MLM_QUERYFONT,O.MPARAM(ADR(fontDlg.fAttrs)),NIL);

  hpsEd := O.WinGetPS(hEd);
  O.GpiCreateLogFont(hpsEd, NIL, 1, fontDlg.fAttrs);
  O.GpiSetCharSet(hpsEd, 1);
  O.GpiQueryFontMetrics(hpsEd, SIZE(O.FONTMETRICS), fontMetrics);
  O.GpiSetCharSet(hpsEd, O.LCID_DEFAULT);
  O.GpiDeleteSetId(hpsEd, 1);
  O.WinReleasePS(hpsEd);

  Strings.Assign(fontMetrics.szFamilyname, szFamily);

  WITH fontDlg DO
    cbSize         := SIZE(O.FONTDLG);
    hpsScreen      := O.WinGetScreenPS(O.HWND_DESKTOP);
    pszFamilyname  := ADR(szFamily);
    usFamilyBufLen := SIZE(szFamily);
    fxPointSize    := 80000H;
    fl             := O.FNTS_CENTER+O.FNTS_INITFROMFATTRS+O.FNTS_BITMAPONLY;
    flType         := fontMetrics.fsType;
    clrFore        := O.CLR_BLACK;
    clrBack        := O.CLR_WHITE;
    sNominalPointSize := fontMetrics.sNominalPointSize;
    usWeight          := fontMetrics.usWeightClass;
    usWidth           := fontMetrics.usWidthClass;
  END;

  O.WinFontDlg(O.HWND_DESKTOP,hEd,fontDlg);
  O.WinReleasePS(fontDlg.hpsScreen);
  IF (fontDlg.lReturn = O.DID_OK) THEN
    fAttrs := fontDlg.fAttrs;
    RETURN TRUE;
  END;
  RETURN FALSE;
END FontDlg;


PROCEDURE [O.EXPENTRY] wpEd(hWnd :O.HWND; ulMsg  :CARDINAL; m1, m2 :O.MPARAM) :O.MRESULT;

CONST
  SZITEM = "~Font...";
VAR
  hMenu       : O.HWND;
  mi          : O.MENUITEM;

BEGIN

  CASE ulMsg OF

  |MM_START:
    O.WinSendMsg(hEd, O.MLM_SETTEXTLIMIT, O.MPARAM(65535), NIL);
    O.WinSendMsg(hEd, O.MLM_FORMAT,       O.MPARAM(O.MLFIE_NOTRANS), NIL);
    O.WinSendMsg(hEd, O.MLM_SETWRAP,      O.MPARAM(TRUE), NIL);

    rdProfile();

    mi.hwndSubMenu := 0;
    hMenu := O.WinWindowFromID(hFrame, O.FID_SYSMENU);
    O.WinSendMsg(hMenu,O.MM_QUERYITEM,O.MPARAM(O.SC_SYSMENU),O.MPARAM(ADR(mi)));
    hMenu := mi.hwndSubMenu;
    IF (hMenu # 0) THEN
      mi.iPosition   := 0;
      mi.afStyle     := O.MIS_TEXT;
      mi.afAttribute := 0;
      mi.id          := ITEM_FONT;
      mi.hwndSubMenu := 0;
      mi.hItem       := 0;
      O.WinSendMsg(hMenu, O.MM_INSERTITEM, O.MPARAM(ADR(mi)), O.MPARAM(ADR(SZITEM)));
    END;
    O.DosPostEventSem(hevReady);

  |O.WM_COMMAND:
    CASE O.ULONGFROMMP(m1) OF
    |ITEM_FONT:
      O.WinSendMsg(hWnd,MM_CHANGEFONT,O.MPARAM(1),NIL);
    ELSE
      RETURN pwpEdDefault(hWnd,ulMsg,m1,m2);
    END;

  |MM_CHANGEFONT:
    -- m1 = NIL => set fAttrs font.
    -- m1 # NIL => font dialog
    IF (m1 # NIL) THEN
      IF NOT FontDlg() THEN RETURN NIL; END;
    END;

    O.WinSendMsg(hEd, O.MLM_SETFONT, O.MPARAM(ADR(fAttrs)),NIL);

  | O.WM_CLOSE:
    wrProfile();
    O.WinDestroyWindow(hFrame);
    hFrame := 0;
    hEd    := 0;

    RETURN O.MRESULT(FALSE);

  | MM_POSTSEM:
    O.DosPostEventSem(hevOutTxt);

  ELSE
    RETURN pwpEdDefault(hWnd,ulMsg,m1,m2);
  END; (* CASE*)

  RETURN O.MRESULT(FALSE);
END wpEd;






PROCEDURE Close();
BEGIN
  IF (hFrame # 0) THEN
    wrProfile();
    O.WinDestroyWindow(hFrame);
  END;
  hFrame := 0;
  hEd    := 0;
END Close;


PROCEDURE ["SysCall"] WinThread(par: O.ULONG);

  PROCEDURE kill();
  BEGIN
    IF (hEd    # 0) THEN O.WinDestroyWindow    (hEd);    hEd    := 0; END;
    IF (hFrame # 0) THEN O.WinDestroyWindow    (hFrame); hFrame := 0; END;
    IF (hMsgQ  # 0) THEN O.WinDestroyMsgQueue  (hMsgQ);               END;
    IF (hAB    # 0) THEN O.WinTerminate        (hAB);                 END;
    O.DosPostEventSem(hevReady);
    O.DosExit(O.EXIT_THREAD, 0);
  END kill;

BEGIN
  hAB   := O.WinInitialize(0);
  hMsgQ := O.WinCreateMsgQueue(hAB,32);
  IF (hMsgQ = 0) THEN kill(); RETURN; END;

  fcdata.cb            := SIZE(O.FRAMECDATA);
  fcdata.flCreateFlags := O.FCF_TITLEBAR + O.FCF_SYSMENU + O.FCF_MINMAX +
                          O.FCF_SIZEBORDER + O.FCF_SHELLPOSITION + O.FCF_NOBYTEALIGN;
  fcdata.hmodResources := 0;
  fcdata.idResources   := 0;

  hFrame := O.WinCreateWindow(O.HWND_DESKTOP,      (* parent window          *)
                              O.WC_FRAME,          (* class name             *)
                              "StdOutChan",        (* window text            *)
                              0,                   (* window style           *)
                              0,0,                 (* position (x,y)         *)
                              0,0,                 (* size (width,height)    *)
                              0,                   (* owner window           *)
                              O.HWND_TOP,          (* sibling window         *)
                              0,                   (* window id              *)
                              ADR(fcdata),         (* control data           *)
                              NIL);                (* presentation parms     *)

  hEd := O.WinCreateWindow(     hFrame,              (* parent window          *)
                                O.WC_MLE,            (* class name             *)
                                NIL,                 (* window text            *)
                                O.MLS_HSCROLL + O.MLS_VSCROLL + O.WS_VISIBLE,
                                                     (* window style           *)
                                0, 0,                (* position (x,y)         *)
                                0, 0,                (* size (width,height)    *)
                                hFrame,              (* owner window           *)
                                O.HWND_TOP,          (* sibling window         *)
                                O.FID_CLIENT,        (* window id              *)
                                NIL,                 (* control data           *)
                                NIL);                (* presentation parms     *)

  IF (hEd # 0) THEN pwpEdDefault := O.WinSubclassWindow(hEd,wpEd); END;

  IF (hEd = 0) OR (pwpEdDefault = NIL) THEN
    kill();
    RETURN;
  END;

  O.WinPostMsg(hEd, MM_START, NIL, NIL);

  (* Main message loop *)
  WHILE O.WinGetMsg(hAB, qMsg, 0, 0, 0) DO
    O.WinDispatchMsg(hAB, qMsg);
  END;

  kill();

END WinThread;


BEGIN
  bFirst      := TRUE;
  hFrame      := 0;
  hEd         := 0;
  ipt         := 0;
  buf         := NIL;
  hevOutTxt   := 0;
  hevReady    := 0;
  O.DosCreateEventSem(NIL,   hevReady,  0, FALSE);
  O.DosCreateThread(win_tid, WinThread, 0, 0, 0FFFFH);
  O.DosWaitEventSem(hevReady,5000);
  CatchOut();
FINALLY
  ReleaseOut();
  IF (buf # NIL) THEN DISPOSE(buf); END;
END OutMon.

