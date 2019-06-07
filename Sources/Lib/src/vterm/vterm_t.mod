IMPLEMENTATION MODULE vterm_t;

<* +M2ADDTYPES   *>
<* +M2EXTENSIONS *>
<* +STORAGE *>


IMPORT W := Windows,
       Strings,
       Storage,
       SYSTEM,
       IOLink,
       IOChan,
       TextIO,
       IOConsts,
       StdChans,
       IOResult,
       SeqFile,
       FileSys,
       ChanConsts;

FROM Windows IMPORT max, min;

CONST
  MAINCLASSNAME   = "MainWnd";
  GRAPHCLASSNAME  = "GraphWnd";
  TEXTCLASSNAME   = "TextWnd";

VAR
  hwndText        : W.HWND;
  hFont           : W.HFONT;
  hdcText         : W.HDC;
  hsemTextReady   : W.HANDLE;
  res         : ChanConsts.OpenResults;
  resbool     : BOOLEAN;

(*------------------- Semaphores ---------------------*)

PROCEDURE wait_text_readyB();
BEGIN
  W.WaitForSingleObject ( hsemTextReady, W.INFINITE ); (* Auto-reset *)
END wait_text_readyB;

PROCEDURE set_text_ready();
BEGIN
  W.SetEvent  (hsemTextReady);
END set_text_ready;

(*----------------------------------------------------*)


(* ================== M O D U L E   T e x t  ==========================*)
(* ================== M O D U L E   T e x t  ==========================*)
(* ================== M O D U L E   T e x t  ==========================*)
MODULE Text;

IMPORT
    W, SYSTEM,
    CHAR_CR,
    CHAR_LF,
    hwndText,
    hdcText,
    min,
    max,
    wait_text_readyB,
    set_text_ready;

FROM Storage IMPORT
    ALLOCATE, DEALLOCATE;

EXPORT QUALIFIED
    AdjustWinSize,
    UpdateWindow,
    caret,
    wr_char,
    wr_lf,
    draw_at,
    set_pos,
    get_pos,
    get_winsize,
    text_changed,
    CARET_ACTION;

CONST
    DEF_FORECOLOR = 00FF80H;
    DEF_BACKCOLOR = 000000H;

TYPE
    CARET_ACTION  = (CA_SHOW,CA_HIDE,CA_DESTROY);
    TEXTLINE      = ARRAY OF CHAR;
    TEXTLINES     = ARRAY OF TEXTLINE;
    PTEXTLINES    = POINTER TO TEXTLINES;

VAR
    aachBuf          : PTEXTLINES;
    cxWin,  cyWin    : INTEGER;
    xPos,   yPos     : INTEGER;
    nRows,  nColumns : INTEGER;
    fontXd, fontYd   : INTEGER;
    caState          : CARET_ACTION;


(**** mark text changed *)
PROCEDURE text_changed();
BEGIN
  W.InvalidateRect(hwndText,NIL,FALSE);
END text_changed;


(****
 * Sets window size,
 * (re)allocates text buffer
 ****)
PROCEDURE AdjustWinSize(bInitCall: BOOLEAN);
VAR
  rect       : W.RECT;
  tMetrics   : W.TEXTMETRIC;
  col, row   : INTEGER;
  i,j        : INTEGER;
  aach       : PTEXTLINES;
BEGIN
  IF (NOT bInitCall) THEN wait_text_readyB(); END;

  W.GetTextMetrics(hdcText,tMetrics);
  WITH tMetrics DO
    fontXd      := max(tmMaxCharWidth,1);
    fontYd      := max(tmHeight,1);
  END;

  W.GetWindowRect(hwndText,rect);
  cxWin    := rect.right-rect.left;
  cyWin    := rect.bottom-rect.top;
  col      := max(cxWin DIV fontXd,1);
  row      := max(cyWin DIV fontYd,1)-2;
  IF (col # nColumns) OR (row # nRows) THEN
    NEW (aach,   row, col);
    FOR i:=0 TO row-1 DO
      FOR j:=0 TO col-1 DO
        IF (aachBuf # NIL)            AND
           (i - (row-nRows) >= 0)     AND
           (i - (row-nRows) <  nRows) AND
           (j < nColumns)
        THEN
          aach  ^[i][j] := aachBuf  ^[i - (row-nRows)][j];
        ELSE
          aach  ^[i][j] := ' ';
        END;
      END;
    END;
    IF (aachBuf # NIL) THEN
      DISPOSE(aachBuf);
    END;
    xPos         := min(xPos,col-1);
    yPos         := max(yPos + (row-nRows),0);
    nRows        := row;
    nColumns     := col;
    aachBuf      := aach;
  END;
  IF (NOT bInitCall) THEN set_text_ready()
  ELSE               xPos := 0; yPos := 0; END;
  text_changed();
END AdjustWinSize;


(**** change caret state *)
PROCEDURE caret(action: CARET_ACTION);
BEGIN
  CASE action OF
  | CA_SHOW:
    IF (W.GetFocus() = hwndText) THEN
      IF (caState = CA_DESTROY) THEN
        W.CreateCaret(hwndText,NIL,fontXd,1);
      END;
      W.SetCaretPos(xPos*fontXd,(yPos+1)*fontYd);
      IF (caState # CA_SHOW) THEN
        W.ShowCaret(hwndText);
      END;
      caState := CA_SHOW;
    END;
  | CA_HIDE:
    IF (caState = CA_SHOW) THEN
      W.HideCaret(hwndText);
      caState := CA_HIDE;
    END;
  | CA_DESTROY:
    IF (caState # CA_DESTROY) THEN
      W.DestroyCaret();
      caState := CA_DESTROY;
    END;
  ELSE;
  END;
END caret;


(**** Redraw text window *)
PROCEDURE UpdateWindow();
VAR
  i         : INTEGER;
  rect      : W.RECT;
  rgbF,rgbB : INTEGER;
BEGIN
  IF (aachBuf = NIL) THEN RETURN; END;
  wait_text_readyB();
  caret(CA_HIDE);

  rgbB := W.GetSysColor(W.COLOR_WINDOW);
  rgbF := W.GetSysColor(W.COLOR_WINDOWTEXT);
  W.SetTextColor(hdcText,rgbF);
  W.SetBkColor  (hdcText,rgbB);
  W.SetBkMode   (hdcText,W.OPAQUE);

  rect.left   := 0;
  rect.right  := cxWin;
  rect.top    := 0;
  rect.bottom := fontYd;
  FOR i:=0 TO nRows-1 DO
    W.DrawText(hdcText,aachBuf^[i], nColumns, rect,
               W.DT_LEFT+W.DT_SINGLELINE+W.DT_VCENTER+W.DT_NOCLIP);
    rect.top    := rect.bottom;
    rect.bottom := rect.bottom + (fontYd);
  END;

  rect.bottom := cyWin;
  W.FillRect(hdcText, rect, W.HBRUSH(INTEGER(W.COLOR_WINDOW)+1));
  rect.left   := fontXd*nColumns;
  rect.right  := cxWin;
  rect.top    := 0;
  W.FillRect(hdcText, rect, W.HBRUSH(INTEGER(W.COLOR_WINDOW)+1));

  caret(CA_DESTROY);
  caret(CA_SHOW);
  set_text_ready();
END UpdateWindow;


PROCEDURE wr_lf  ();
VAR i,j : INTEGER;
BEGIN
  wait_text_readyB();
  xPos := 0;
  IF (yPos<nRows-1) THEN
    yPos := yPos+1;
    caret(CA_SHOW);
  ELSE
    IF (aachBuf = NIL) THEN
      W.MessageBox (NIL, "Jopa (nil)!", NIL,
                    W.MB_OK + W.MB_SYSTEMMODAL + W.MB_ICONHAND);
      RETURN;
    END;
    IF (HIGH(aachBuf^) # CARDINAL(nRows-1)) OR (HIGH(aachBuf^[0]) # CARDINAL(nColumns-1))
    THEN
      W.MessageBox (NIL, "Jopa!", NIL,
                    W.MB_OK + W.MB_SYSTEMMODAL + W.MB_ICONHAND);
      RETURN;
    END;
    FOR i := 0 TO nRows-2 DO
      FOR j := 0 TO nColumns-1 DO
        aachBuf^[i][j] := aachBuf^[i+1][j];
      END;
    END;
    FOR j := 0 TO nColumns-1 DO
      aachBuf^[nRows-1][j] := ' ';
    END;
    yPos := nRows-1;
    text_changed();
  END;
  set_text_ready();
END wr_lf;


PROCEDURE wr_char(ch: CHAR);
BEGIN
  wait_text_readyB();
  aachBuf^[yPos][xPos] := ch;
--  text_changed();
  IF (xPos < nColumns-1) THEN
    xPos:= xPos+1;
  ELSE
    set_text_ready();
    wr_lf();
  END;
  set_text_ready();
END wr_char;


PROCEDURE draw_at(x0, y0, cxField: INTEGER; from : SYSTEM.ADDRESS; len : INTEGER);
TYPE
  PCHAR = POINTER TO CHAR;
VAR
  i  : INTEGER;
  ch : CHAR;
  p  : PCHAR;
BEGIN
  IF (x0>=nColumns) OR (y0>=nRows) OR (x0<0) OR (y0<0) THEN RETURN; END;
  IF (cxField <= 0) THEN cxField := nColumns - x0; END;
  cxField := min(cxField, nColumns - x0);

  wait_text_readyB();
  FOR i:=0 TO cxField-1 DO
    IF (i<=len) THEN
      p  := SYSTEM.CAST(PCHAR,(i+SYSTEM.INT32(from)));
      ch := p^;
      IF (ch = CHAR_CR) OR (ch = CHAR_LF) OR (ch = 0C) THEN ch := ' '; END;
    ELSE
      ch := ' ';
    END;
    aachBuf^[y0][x0+i] := ch;
  END;
  set_text_ready();
  text_changed();
END draw_at;

PROCEDURE set_pos(line, pos: INTEGER);
BEGIN
  wait_text_readyB();
  yPos := min(max(line,0),nRows   -1);
  xPos := min(max(pos, 0),nColumns-1);
  caret(CA_SHOW);
  set_text_ready();
END set_pos;


PROCEDURE get_pos(VAR line, pos: INTEGER);
BEGIN
  line := yPos;
  pos  := xPos;
END get_pos;

PROCEDURE get_winsize(VAR lines, columns: INTEGER);
BEGIN
  lines   := nRows;
  columns := nColumns;
END get_winsize;

BEGIN
  nColumns     := -1;
  aachBuf      := NIL;
  caState      := CA_DESTROY;
FINALLY
  IF (aachBuf # NIL) THEN
    DISPOSE(aachBuf);
  END;
END Text;




(* ================ M O D U L E   C h a n e l =========================*)
(* ================ M O D U L E   C h a n e l =========================*)
(* ================ M O D U L E   C h a n e l =========================*)

MODULE Chanel;

IMPORT
    W,
    SYSTEM,
    Strings,
    IOLink,
    IOChan,
    IOConsts,
    IOResult,
    StdChans,
    TextIO,
    SeqFile,
    ChanConsts,
    Text,
    CHAR_CR,
    CHAR_LF,
    CHAR_BS,
    CHAR_BEL,
    CHAR_EOF,
    KEYBUFSIZE,
    hwndText,
    TEMPSIFFILENAME,
    min,
    max;

FROM Storage IMPORT
    ALLOCATE, DEALLOCATE;

IMPORT readingFromFile,
       inputFile,
       outputFile,
       hsemCharReady;

EXPORT QUALIFIED
    Init,
    get_key,
    is_pressed,
    key_pressed;


TYPE
    PSTR = POINTER TO ARRAY OF CHAR;
VAR
    pstrInBuf     : PSTR;     -- line entered with get_line() ( + LF) or NIL
    nInSkip       : CARDINAL; -- chars read fron pstrInBuf
    dId           : IOLink.DeviceId;
    cId           : IOChan.ChanId;
    chanCreated   : BOOLEAN;
    achKeyBuf     : ARRAY [0..KEYBUFSIZE] OF CHAR; --
    nKeyTop       : INTEGER;  --    Keyboard       --
    nKeyBottom    : INTEGER;  --      buffer       --


PROCEDURE beep(); BEGIN W.MessageBeep(W.MB_SET(0FFFFFFFFH)); END beep;

PROCEDURE key_pressed(ch : CHAR);
BEGIN
  IF ((nKeyTop+1) MOD KEYBUFSIZE = nKeyBottom) THEN
    beep();
  ELSE
    achKeyBuf[nKeyTop] := ch;
    nKeyTop := (nKeyTop+1) MOD KEYBUFSIZE;
    W.SetEvent(hsemCharReady);
  END;
END key_pressed;

PROCEDURE is_pressed(): BOOLEAN;
BEGIN
   RETURN nKeyTop # nKeyBottom;
END is_pressed;


PROCEDURE get_key(bWait : BOOLEAN): CHAR;
VAR ch: CHAR;
    res : IOConsts.ReadResults;
    wasRead : CARDINAL;
BEGIN
  IF (hwndText = NIL) THEN RETURN 0C; END;
  IF (bWait) AND NOT readingFromFile THEN
    W.WaitForSingleObject(hsemCharReady, W.INFINITE); (* Auto-reset *)
  END;
  IF readingFromFile THEN 
    IF IOResult.ReadResult( inputFile ) = IOConsts.endOfInput THEN
       readingFromFile:=FALSE;
       SeqFile.Close( inputFile );
       RETURN CHAR_EOF;
    END;
    REPEAT
    TextIO.ReadChar (inputFile, ch );
    res := IOResult.ReadResult( inputFile );
        IF res = IOConsts.endOfInput THEN
          RETURN CHAR_EOF;
        END;
    IF res = IOConsts.endOfLine THEN
           TextIO.SkipLine( inputFile );
           ch:=15C;
        END;
    UNTIL (ch#12C) OR (res = IOConsts.endOfLine);
    IF res = IOConsts.endOfInput THEN
       RETURN CHAR_EOF;
    END;
  END;
  IF NOT readingFromFile THEN
      ch         := achKeyBuf[nKeyBottom];
    IF (nKeyTop = nKeyBottom) THEN RETURN 0C; END;
    nKeyBottom := (nKeyBottom+1) MOD KEYBUFSIZE;
    IF (nKeyTop # nKeyBottom) THEN
      W.SetEvent( hsemCharReady );
    END;
    IF ch = 15C THEN 
      res := IOConsts.endOfLine;
    ELSE
      res := IOConsts.allRight;
    END;
  END;

  IF res = IOConsts.endOfLine THEN
    TextIO.WriteLn( outputFile );
  ELSE
    TextIO.WriteChar( outputFile, ch );
  END;
  RETURN ch;
END get_key;


(* cxField:
 * > 0 - use field width up to value
 * <=0 - up to end of line
 *)
PROCEDURE get_line(cxField : INTEGER) : PSTR;
VAR
  res   : PSTR;
  ch    : CHAR;
  pos   : INTEGER;

  x0    : INTEGER; -- line
  y0    : INTEGER; --    position
  xSkip : INTEGER; -- scroll line left
  i,j   : INTEGER;

  PROCEDURE draw();
  BEGIN
    Text.set_pos(y0,x0+pos-xSkip);
    Text.draw_at(x0,y0,cxField,SYSTEM.ADR(res^[xSkip]),pos-xSkip);
  END draw;

  PROCEDURE app(ch : CHAR);
  VAR s : PSTR;
      i : INTEGER;
  BEGIN
    INC(pos);
    NEW(s,pos+1);
    FOR i:=0 TO pos-2 DO
      s^[i] := res^[i];
    END;
    s^[pos-1] := ch;
    s^[pos+0] := CHAR_LF;
    DISPOSE(res);
    res       := s;
    xSkip     := max(0,pos-cxField+1);
    draw();
  END app;

  PROCEDURE bs();
  VAR s : PSTR;
      i : INTEGER;
  BEGIN
    IF (pos>0) THEN
      DEC(pos);
      NEW(s,pos+1);
      FOR i:=0 TO pos-1 DO
        s^[i] := res^[i];
      END;
      s^[pos+0] := CHAR_LF;
      DISPOSE(res);
      res       := s;
      xSkip     := max(0,pos-cxField+1);
      draw();
    END;
  END bs;

BEGIN
  Text.get_pos    (y0,x0);
  Text.get_winsize(i, j);
  IF (cxField <= 0) THEN cxField := j-x0; END;
  cxField                        := min(cxField,j-x0);

  NEW(res,1);
  res^[0] := CHAR_LF;
  pos     := 0;
  LOOP
    ch := get_key(TRUE);
    CASE ch OF
      | CHAR_BS:
          bs();
      | CHAR_CR, 0C:
          Text.wr_lf();
          RETURN res;
      | CHAR_EOF:
          app(ch);
          RETURN res;
      | CHAR_LF:
      ELSE
          app(ch);
    END;
  END;
END get_line;

PROCEDURE peek_char(bDrop: BOOLEAN):CHAR;
VAR ch : CHAR;
BEGIN
  WHILE (pstrInBuf = NIL) OR (nInSkip>HIGH(pstrInBuf^)) DO
    IF (pstrInBuf # NIL) THEN DISPOSE(pstrInBuf); END;
    pstrInBuf := get_line(-1);
    nInSkip   := 0;
  END;
  ch := pstrInBuf^[nInSkip];
  IF (bDrop) THEN INC(nInSkip); END;
  RETURN ch;
END peek_char;

(*----- Channel functions --------*)

PROCEDURE doLook(pDT     : IOLink.DeviceTablePtr;
                 VAR ch  : CHAR;
                 VAR res : IOConsts.ReadResults);
BEGIN
  ch  := peek_char(FALSE);
  CASE ch OF
    | CHAR_EOF:
      res := IOConsts.endOfInput;
    | CHAR_LF:
      res := IOConsts.endOfLine;
  ELSE
    res := IOConsts.allRight;
  END;
  IOChan.SetReadResult (pDT^.cid, res);
END doLook;

PROCEDURE doSkip(pDT : IOLink.DeviceTablePtr);
VAR ch : CHAR;
BEGIN
  ch := peek_char(TRUE);
  IF (ch = CHAR_LF) THEN
    DISPOSE(pstrInBuf);
    pstrInBuf := NIL;
  END;
  IOChan.SetReadResult (pDT^.cid, IOConsts.allRight);
END doSkip;

PROCEDURE doSkipLook(pDT     : IOLink.DeviceTablePtr;
                     VAR ch  : CHAR;
                     VAR res : IOConsts.ReadResults);
BEGIN
  doSkip(pDT);
  doLook(pDT,ch,res);
END doSkipLook;

PROCEDURE doLnWrite(pDT : IOLink.DeviceTablePtr);
BEGIN
  Text.wr_lf();
END doLnWrite;

PROCEDURE doTextRead(pDT           : IOLink.DeviceTablePtr;
                     to            : SYSTEM.ADDRESS;
                     maxChars      : CARDINAL;
                     VAR charsRead : CARDINAL);
TYPE
  PCHAR = POINTER TO CHAR;
VAR
  ch  : CHAR;
  res : IOConsts.ReadResults;
  p   : PCHAR;
BEGIN
  IOChan.SetReadResult (pDT^.cid, IOConsts.allRight);
  charsRead := 0;
  WHILE (charsRead < maxChars) DO
    doLook(pDT,ch,res);
    IF (res = IOConsts.endOfLine) THEN
      IF (charsRead = 0) THEN
        IOChan.SetReadResult (pDT^.cid, IOConsts.endOfLine);
      END;
      RETURN;
    END;
-- aaaaa
    IF (res = IOConsts.endOfInput) THEN
      IF (charsRead = 0) THEN
        IOChan.SetReadResult (pDT^.cid, IOConsts.endOfInput);
      END;
      RETURN;
    END;
    doSkip(pDT);
    p  := SYSTEM.CAST(PCHAR,(charsRead+CARDINAL(to)));
    p^ := ch;
    INC(charsRead);
  END;
END doTextRead;

PROCEDURE doTextWrite(pDT          : IOLink.DeviceTablePtr;
                      from         : SYSTEM.ADDRESS;
                      charsToWrite : CARDINAL);
TYPE
  PCHAR = POINTER TO CHAR;
VAR
  i        : CARDINAL;
  p        : PCHAR;
  ch       : CHAR;
  line,pos : INTEGER;
BEGIN
  IF (charsToWrite > 0) THEN
    FOR i:=0 TO charsToWrite-1 DO
      p  := SYSTEM.CAST(PCHAR,(i+CARDINAL(from)));
      ch := p^;
      IF (ch<040C) THEN
        Text.get_pos(line,pos);
        CASE (ch) OF
        | CHAR_LF:
          Text.wr_lf();
        | CHAR_CR:
          Text.set_pos(line,0);
        | CHAR_BS:
          IF (pos>0) THEN
            Text.set_pos(line,pos-1);
            Text.wr_char(' ');
            Text.set_pos(line,pos-1);
          END;
        | CHAR_BEL:
          beep();
        ELSE;
        END;
      ELSE
        Text.wr_char(ch);
      END;
    END;
  END;
  Text.text_changed();
END doTextWrite;

PROCEDURE doGetName(pDT   : IOLink.DeviceTablePtr;
                    VAR s : ARRAY OF CHAR);
BEGIN
  Strings.Assign("VTerm",s);
END doGetName;

PROCEDURE doReset(pDT   : IOLink.DeviceTablePtr);
BEGIN
  IF (pstrInBuf # NIL) THEN
    DISPOSE(pstrInBuf);
    pstrInBuf := NIL;
  END;
END doReset;

PROCEDURE Init(): BOOLEAN;
VAR pDT: IOLink.DeviceTablePtr;
  VAR res: SeqFile.OpenResults;
BEGIN
  hsemCharReady := W.CreateEvent ( NIL, FALSE, FALSE, NIL ); (* Auto-reset *)
  IF (hsemCharReady = NIL) THEN
    W.MessageBox ( NIL, "Key event not created", "VTerm",
                   W.MB_OK + W.MB_SYSTEMMODAL + W.MB_ICONHAND );
    RETURN FALSE;

  END;

  IOLink.AllocateDeviceId(dId);
  IOLink.MakeChan        (dId,cId);
  IF (cId = IOChan.InvalidChan()) THEN
    W.MessageBox ( NIL, "Channel not created", "VTerm",
                   W.MB_OK + W.MB_SYSTEMMODAL + W.MB_ICONHAND );
    RETURN FALSE;
  ELSE
    chanCreated := TRUE;
  END;

  pDT := IOLink.DeviceTablePtrValue(cId, dId,IOChan.notAvailable,"VTerm");
  pDT^.flags         := pDT^.flags + ChanConsts.FlagSet
           {
             ChanConsts.readFlag,        (* input operations are requested/available *)
             ChanConsts.writeFlag,       (* output operations are requested/available *)
             ChanConsts.textFlag,        (* text operations are requested/available *)
             ChanConsts.interactiveFlag, (* interactive use is requested/applies *)
             ChanConsts.echoFlag         (* echoing by interactive device on removal
                                            of characters from input stream
                                            requested/applies *)
           };
  pDT^.doLook      := doLook;
  pDT^.doSkip      := doSkip;
  pDT^.doSkipLook  := doSkipLook;
  pDT^.doLnWrite   := doLnWrite;
  pDT^.doTextRead  := doTextRead;
  pDT^.doTextWrite := doTextWrite;
  pDT^.doGetName   := doGetName;
  pDT^.doReset     := doReset;

  StdChans.SetInChan(cId); 
  StdChans.SetOutChan(cId);
  StdChans.SetErrChan(cId);

  RETURN TRUE;
END Init;

BEGIN
  chanCreated   := FALSE;
  nKeyTop       := 0;
  nKeyBottom    := 0;
  pstrInBuf     := NIL;
  nInSkip       := 0;
  hsemCharReady := NIL;
FINALLY
  IF (chanCreated)         THEN IOLink.UnMakeChan(dId,cId);   END;
  IF (pstrInBuf # NIL)     THEN DISPOSE(pstrInBuf);           END;
  IF (hsemCharReady # NIL) THEN W.CloseHandle(hsemCharReady); END;
END Chanel;
(* ========= E N D  C h a n e l ==========*)




(**** Text window procedure
 *)
PROCEDURE [W.CALLBACK] TextWinProc ( hWnd   :W.HWND;
                                     msg    :W.UINT;
                                     wParam :W.WPARAM;
                                     lParam :W.LPARAM) : W.LRESULT;
TYPE SET32 = SET OF [0..31];
VAR
  dc   :W.HDC;
  ps   :W.PAINTSTRUCT;
BEGIN
  CASE msg OF
  | W.WM_CREATE:
    hwndText := hWnd;
    hdcText  := W.GetDC(hWnd);
    IF (hFont # NIL) THEN W.SelectObject(hdcText,hFont); END;
    Text. AdjustWinSize(TRUE);
    W.SetFocus(hWnd);
    IF (NOT Chanel.Init()) THEN RETURN -1; END;
    RETURN 0;
  | W.WM_SIZE:
    Text.AdjustWinSize(FALSE);
    RETURN 0;
  | W.WM_PAINT:
      dc := W.BeginPaint ( hWnd, ps );
      W.EndPaint (hWnd, ps);
      Text.UpdateWindow();
      RETURN 0;
  | W.WM_SETFOCUS:
      Text.caret(Text.CA_SHOW);
      RETURN 0;
  | W.WM_KILLFOCUS:
      Text.caret(Text.CA_DESTROY);
      RETURN 0;
  | W.WM_CHAR:
    IF ((NOT (24 IN SET32(lParam))) AND --* key is an extended key
        (NOT (29 IN SET32(lParam))) AND --* ALT
        (NOT (31 IN SET32(lParam))) AND --* Key up
        (wParam # 0))
    THEN
      Chanel.key_pressed(CHAR(wParam));
    END;
  | W.WM_DESTROY:
    hwndText := NIL;
    W.SetEvent(hsemCharReady);
  ELSE ;
  END;
  RETURN W.DefWindowProc (hWnd, msg, wParam, lParam);
END TextWinProc;

PROCEDURE get_key(): CHAR;
BEGIN
    RETURN Chanel.get_key(FALSE);
END get_key;

PROCEDURE is_pressed(): BOOLEAN;
BEGIN
    RETURN Chanel.is_pressed();
END is_pressed;

PROCEDURE pre_init(hInst :W.HINSTANCE):BOOLEAN;
VAR
  wc        : W.WNDCLASS;

BEGIN (* MainLoop *)
  hFont            := W.GetStockObject(W.OEM_FIXED_FONT);

  hsemTextReady    := W.CreateEvent ( NIL, FALSE,TRUE,  NIL ); (* Auto-reset *)
  IF (hsemTextReady = NIL) THEN
    W.MessageBox ( NIL, "Text event not created", "VTerm",
                   W.MB_OK + W.MB_SYSTEMMODAL + W.MB_ICONHAND );
    RETURN FALSE;
  END;

 (* Register TWCLASSNAME window class *)
  wc.style         := W.CS_BYTEALIGNWINDOW + W.CS_OWNDC;
  wc.lpfnWndProc   := TextWinProc;
  wc.cbClsExtra    := 0;
  wc.cbWndExtra    := 0;
  wc.hInstance     := hInst;
  wc.hIcon         := W.LoadIcon ( NIL, W.IDI_APPLICATION );
  wc.hCursor       := W.LoadCursor (NIL, W.RESOURCESTR(VAL (INTEGER, W.IDC_ARROW)) );
  wc.hbrBackground := W.CreateSolidBrush (W.RGB (255,255,255));
  wc.lpszMenuName  := NIL;
  wc.lpszClassName := SYSTEM.ADR (TWCLASSNAME);

  IF (W.RegisterClass (wc) = 0) THEN
    W.MessageBox (NIL, "Text window class was not registered", NIL,
                  W.MB_OK + W.MB_SYSTEMMODAL + W.MB_ICONHAND);
    RETURN FALSE;
  ELSE
    RETURN TRUE;
  END;
END pre_init;


BEGIN
  hwndText      := NIL;
  hdcText       := NIL;
  hsemTextReady := NIL;
  readingFromFile:=FALSE;
  SeqFile.OpenWrite ( outputFile, TEMPSIFFILENAME,
         ChanConsts.write+ChanConsts.old, res );
FINALLY
  SeqFile.Close( outputFile );
  FileSys.Remove( TEMPSIFFILENAME , resbool );
  IF (hdcText # NIL)       THEN W.DeleteDC(hdcText);          END;
  IF (hsemTextReady # NIL) THEN W.CloseHandle(hsemTextReady); END;
END vterm_t.
