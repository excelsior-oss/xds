(* Copyright (C) 1999 *FSA & XDS Ltd. *)

<*+M2ADDTYPES   *>
<*+M2EXTENSIONS *>

<*-IOVERFLOW    *>
<*-COVERFLOW    *>
<*-CHECKINDEX   *>
<*-CHECKRANGE   *>
<*-CHECKNIL     *>
<*+STORAGE      *>

IMPLEMENTATION MODULE xtsEvQue;


IMPORT xtsIGraph,
       StdChans,
       IOLink,
       IOChan,
       xlibOS,
       xDevData,
       ChanConsts,
       ioc:=IOConsts,
       SYSTEM,
       W := Windows;

<* IF multithread THEN *>
  IMPORT xmRTS;
<* END*>


TYPE
  CHARPAIR = RECORD
    LeadChar   : SYSTEM.CARD8;
    SecondChar : SYSTEM.CARD8;
  END;
  PCHARPAIR = POINTER TO CHARPAIR;

  KEYREC = RECORD
    LeadChar   : SYSTEM.CARD8;
    SecondChar : SYSTEM.CARD8;
    keyEvt     : W.KEY_EVENT_RECORD;
  END;
  PKEYREC = POINTER TO KEYREC;

  PMOUHNDLLIST = POINTER TO MOUHNDLLIST;
  MOUHNDLLIST = RECORD
    next  : PMOUHNDLLIST;
    f     : MOUHNDLFUNC;
  END;

  ENHKEYVALS = RECORD
    ScanCode   : W.WORD;
    RegChars   : CHARPAIR;
    ShiftChars : CHARPAIR;
    CtrlChars  : CHARPAIR;
    AltChars   : CHARPAIR;
  END;

  NORMKEYVALS = RECORD
    RegChars   : CHARPAIR;
    ShiftChars : CHARPAIR;
    CtrlChars  : CHARPAIR;
    AltChars   : CHARPAIR;
  END;

CONST
  NUM_EKA_ELTS = 12;
  NUM_NKA_ELTS = 89;
  KQMAX        = 32;

TYPE
  ENHANCEDKEYS = ARRAY [0..NUM_EKA_ELTS-1] OF ENHKEYVALS;
  NORMALKEYS   = ARRAY [0..NUM_NKA_ELTS-1] OF NORMKEYVALS;

CONST
  (*
   * Table of key values for enhanced keys
   *)
  EnhancedKeys = ENHANCEDKEYS{
        (*         -none-        Shift         Ctrl          Alt    *)
        { 28, {  13, 0e0h}, {  13, 0e0h}, {  10, 0e0h}, {   0, 166 } }, (* Gray Enter *)
        { 53, {  47, 0e0h}, {  63, 0e0h}, {   0, 149 }, {   0, 164 } }, (* Gray '/'   *)
        { 71, {   0,  71 }, {   0,  71 }, {   0, 119 }, {   0, 151 } }, (* Home  *)
        { 72, {   0,  72 }, {   0,  72 }, {   0, 141 }, {   0, 152 } }, (* Up    *)
        { 73, {   0,  73 }, {   0,  73 }, {   0, 132 }, {   0, 153 } }, (* PgUp  *)
        { 75, {   0,  75 }, {   0,  75 }, {   0, 115 }, {   0, 155 } }, (* Left  *)
        { 77, {   0,  77 }, {   0,  77 }, {   0, 116 }, {   0, 157 } }, (* Right *)
        { 79, {   0,  79 }, {   0,  79 }, {   0, 117 }, {   0, 159 } }, (* End   *)
        { 80, {   0,  80 }, {   0,  80 }, {   0, 145 }, {   0, 160 } }, (* Down  *)
        { 81, {   0,  81 }, {   0,  81 }, {   0, 118 }, {   0, 161 } }, (* PgDn  *)
        { 82, {   0,  82 }, {   0,  82 }, {   0, 146 }, {   0, 162 } }, (* Ins   *)
        { 83, {   0,  83 }, {   0,  83 }, {   0, 147 }, {   0, 163 } }  (* Del   *)
        };

(*
 * Table of key values for normal keys. Note that the table is padded so
 * that the key scan code serves as an index into the table.
 *)
NormalKeys = NORMALKEYS{
        (* -scan-       -none-        Shift         Ctrl          Alt    *)
        { (*  0 *) {   0,   0 }, {   0,   0 }, {   0,   0 }, {   0,   0 } },
        { (*  1 *) {  27,   0 }, {  27,   0 }, {  27,   0 }, {   0,   1 } },
        { (*  2 *) {  49,   0 }, {  33,   0 }, {   0,   0 }, {   0, 120 } },
        { (*  3 *) {  50,   0 }, {  64,   0 }, {   0,   3 }, {   0, 121 } },
        { (*  4 *) {  51,   0 }, {  35,   0 }, {   0,   0 }, {   0, 122 } },
        { (*  5 *) {  52,   0 }, {  36,   0 }, {   0,   0 }, {   0, 123 } },
        { (*  6 *) {  53,   0 }, {  37,   0 }, {   0,   0 }, {   0, 124 } },
        { (*  7 *) {  54,   0 }, {  94,   0 }, {  30,   0 }, {   0, 125 } },
        { (*  8 *) {  55,   0 }, {  38,   0 }, {   0,   0 }, {   0, 126 } },
        { (*  9 *) {  56,   0 }, {  42,   0 }, {   0,   0 }, {   0, 127 } },
        { (* 10 *) {  57,   0 }, {  40,   0 }, {   0,   0 }, {   0, 128 } },
        { (* 11 *) {  48,   0 }, {  41,   0 }, {   0,   0 }, {   0, 129 } },
        { (* 12 *) {  45,   0 }, {  95,   0 }, {  31,   0 }, {   0, 130 } },
        { (* 13 *) {  61,   0 }, {  43,   0 }, {   0,   0 }, {   0, 131 } },
        { (* 14 *) {   8,   0 }, {   8,   0 }, { 127,   0 }, {   0,  14 } },
        { (* 15 *) {   9,   0 }, {   0,  15 }, {   0, 148 }, {   0,  15 } },
        { (* 16 *) { 113,   0 }, {  81,   0 }, {  17,   0 }, {   0,  16 } },
        { (* 17 *) { 119,   0 }, {  87,   0 }, {  23,   0 }, {   0,  17 } },
        { (* 18 *) { 101,   0 }, {  69,   0 }, {   5,   0 }, {   0,  18 } },
        { (* 19 *) { 114,   0 }, {  82,   0 }, {  18,   0 }, {   0,  19 } },
        { (* 20 *) { 116,   0 }, {  84,   0 }, {  20,   0 }, {   0,  20 } },
        { (* 21 *) { 121,   0 }, {  89,   0 }, {  25,   0 }, {   0,  21 } },
        { (* 22 *) { 117,   0 }, {  85,   0 }, {  21,   0 }, {   0,  22 } },
        { (* 23 *) { 105,   0 }, {  73,   0 }, {   9,   0 }, {   0,  23 } },
        { (* 24 *) { 111,   0 }, {  79,   0 }, {  15,   0 }, {   0,  24 } },
        { (* 25 *) { 112,   0 }, {  80,   0 }, {  16,   0 }, {   0,  25 } },
        { (* 26 *) {  91,   0 }, { 123,   0 }, {  27,   0 }, {   0,  26 } },
        { (* 27 *) {  93,   0 }, { 125,   0 }, {  29,   0 }, {   0,  27 } },
        { (* 28 *) {  13,   0 }, {  13,   0 }, {  10,   0 }, {   0,  28 } },
        { (* 29 *) {   0,   0 }, {   0,   0 }, {   0,   0 }, {   0,   0 } },
        { (* 30 *) {  97,   0 }, {  65,   0 }, {   1,   0 }, {   0,  30 } },
        { (* 31 *) { 115,   0 }, {  83,   0 }, {  19,   0 }, {   0,  31 } },
        { (* 32 *) { 100,   0 }, {  68,   0 }, {   4,   0 }, {   0,  32 } },
        { (* 33 *) { 102,   0 }, {  70,   0 }, {   6,   0 }, {   0,  33 } },
        { (* 34 *) { 103,   0 }, {  71,   0 }, {   7,   0 }, {   0,  34 } },
        { (* 35 *) { 104,   0 }, {  72,   0 }, {   8,   0 }, {   0,  35 } },
        { (* 36 *) { 106,   0 }, {  74,   0 }, {  10,   0 }, {   0,  36 } },
        { (* 37 *) { 107,   0 }, {  75,   0 }, {  11,   0 }, {   0,  37 } },
        { (* 38 *) { 108,   0 }, {  76,   0 }, {  12,   0 }, {   0,  38 } },
        { (* 39 *) {  59,   0 }, {  58,   0 }, {   0,   0 }, {   0,  39 } },
        { (* 40 *) {  39,   0 }, {  34,   0 }, {   0,   0 }, {   0,  40 } },
        { (* 41 *) {  96,   0 }, { 126,   0 }, {   0,   0 }, {   0,  41 } },
        { (* 42 *) {    0,  0 }, {   0,   0 }, {   0,   0 }, {   0,   0 } },
        { (* 43 *) {  92,   0 }, { 124,   0 }, {  28,   0 }, {   0,  2bh} },
        { (* 44 *) { 122,   0 }, {  90,   0 }, {  26,   0 }, {   0,  44 } },
        { (* 45 *) { 120,   0 }, {  88,   0 }, {  24,   0 }, {   0,  45 } },
        { (* 46 *) {  99,   0 }, {  67,   0 }, {   3,   0 }, {   0,  46 } },
        { (* 47 *) { 118,   0 }, {  86,   0 }, {  22,   0 }, {   0,  47 } },
        { (* 48 *) {  98,   0 }, {  66,   0 }, {   2,   0 }, {   0,  48 } },
        { (* 49 *) { 110,   0 }, {  78,   0 }, {  14,   0 }, {   0,  49 } },
        { (* 50 *) { 109,   0 }, {  77,   0 }, {  13,   0 }, {   0,  50 } },
        { (* 51 *) {  44,   0 }, {  60,   0 }, {   0,   0 }, {   0,  51 } },
        { (* 52 *) {  46,   0 }, {  62,   0 }, {   0,   0 }, {   0,  52 } },
        { (* 53 *) {  47,   0 }, {  63,   0 }, {   0,   0 }, {   0,  53 } },
        { (* 54 *) {   0,   0 }, {   0,   0 }, {   0,   0 }, {   0,   0 } },
        { (* 55 *) {  42,   0 }, {  42,   0 }, {   0,  96h}, {   0,  37h} },
        { (* 56 *) {   0,   0 }, {   0,   0 }, {   0,   0 }, {   0,   0 } },
        { (* 57 *) {  32,   0 }, {  32,   0 }, {  32,   0 }, {  32,   0 } },
        { (* 58 *) {   0,   0 }, {   0,   0 }, {   0,   0 }, {   0,   0 } },
        { (* 59 *) {   0,  59 }, {   0,  84 }, {   0,  94 }, {   0, 104 } },
        { (* 60 *) {   0,  60 }, {   0,  85 }, {   0,  95 }, {   0, 105 } },
        { (* 61 *) {   0,  61 }, {   0,  86 }, {   0,  96 }, {   0, 106 } },
        { (* 62 *) {   0,  62 }, {   0,  87 }, {   0,  97 }, {   0, 107 } },
        { (* 63 *) {   0,  63 }, {   0,  88 }, {   0,  98 }, {   0, 108 } },
        { (* 64 *) {   0,  64 }, {   0,  89 }, {   0,  99 }, {   0, 109 } },
        { (* 65 *) {   0,  65 }, {   0,  90 }, {   0, 100 }, {   0, 110 } },
        { (* 66 *) {   0,  66 }, {   0,  91 }, {   0, 101 }, {   0, 111 } },
        { (* 67 *) {   0,  67 }, {   0,  92 }, {   0, 102 }, {   0, 112 } },
        { (* 68 *) {   0,  68 }, {   0,  93 }, {   0, 103 }, {   0, 113 } },
        { (* 69 *) {    0,  0 }, {   0,   0 }, {   0,   0 }, {   0,   0 } },
        { (* 70 *) {    0,  0 }, {   0,   0 }, {   0,   0 }, {   0,   0 } },
        { (* 71 *) {   0,  71 }, {  55,   0 }, {   0, 119 }, {   0,   0 } },
        { (* 72 *) {   0,  72 }, {  56,   0 }, {   0, 141 }, {   0,   0 } },
        { (* 73 *) {   0,  73 }, {  57,   0 }, {   0, 132 }, {   0,   0 } },
        { (* 74 *) { 2dh,   0 }, { 2dh,   0 }, {   0, 8eh }, {   0, 4ah } },
        { (* 75 *) {   0,  75 }, {  52,   0 }, {   0, 115 }, {   0,   0 } },
        { (* 76 *) {   0,   0 }, {  53,   0 }, {   0,   0 }, {   0,   0 } },
        { (* 77 *) {   0,  77 }, {  54,   0 }, {   0, 116 }, {   0,   0 } },
        { (* 78 *) { 2bh,   0 }, { 2bh,   0 }, {   0, 90h }, {   0, 4eh } },
        { (* 79 *) {   0,  79 }, {  49,   0 }, {   0, 117 }, {   0,   0 } },
        { (* 80 *) {   0,  80 }, {  50,   0 }, {   0, 145 }, {   0,   0 } },
        { (* 81 *) {   0,  81 }, {  51,   0 }, {   0, 118 }, {   0,   0 } },
        { (* 82 *) {   0,  82 }, {  48,   0 }, {   0, 146 }, {   0,   0 } },
        { (* 83 *) {   0,  83 }, {  46,   0 }, {   0, 147 }, {   0,   0 } },
        { (* 84 *) {   0,   0 }, {   0,   0 }, {   0,   0 }, {   0,   0 } },
        { (* 85 *) {   0,   0 }, {   0,   0 }, {   0,   0 }, {   0,   0 } },
        { (* 86 *) {   0,   0 }, {   0,   0 }, {   0,   0 }, {   0,   0 } },
        { (* 87 *) {   0, 133 }, {   0, 135 }, {   0, 137 }, {   0, 139 } },
        { (* 88 *) {   0, 134 }, {   0, 136 }, {   0, 138 }, {   0, 140 } }
        };

VAR
  KeyQue      : ARRAY [0..KQMAX] OF KEYREC;
  kqTop       : [0..KQMAX+1];
  hQueSem     : W.HANDLE;     -- set while queue is processed
  hKeySem     : W.HANDLE;     -- posted when queue is not empty
  hThr        : W.HANDLE;
  tidThr      : CARDINAL;
  KbdHndl     : W.HANDLE;
  conOldState : W.CONSOLE_MODE_SET;
  mouLst      : PMOUHNDLLIST;



VAR
  cpair   : CHARPAIR;

PROCEDURE getextendedkeycode (VAR KeyEv : W.KEY_EVENT_RECORD) : PCHARPAIR;

VAR
  CKS : W.CONTROLKEYSTATE_SET;
  pCP : PCHARPAIR;
  i   : INTEGER;
  c   : SYSTEM.CARD8;
BEGIN
  CKS := KeyEv.dwControlKeyState - W.NUMLOCK_ON - W.CAPSLOCK_ON - W.SCROLLLOCK_ON;
  pCP := NIL;

  (* Try to get actual ASCII value *)
  c := 0;
  IF (W.SHIFT_PRESSED = CKS) THEN
    IF (ORD(KeyEv.uChar.AsciiChar) # 0) AND (KeyEv.wVirtualScanCode # 42) THEN
      c := ORD(KeyEv.uChar.AsciiChar);
    END;
  ELSIF (W.ENHANCED_KEY # CKS)                      AND
        (W.ENHANCED_KEY+W.RIGHT_CTRL_PRESSED # CKS) AND
        (W.ENHANCED_KEY+W.LEFT_CTRL_PRESSED  # CKS) AND
        (W.RIGHT_CTRL_PRESSED                # CKS) AND
        (W.LEFT_CTRL_PRESSED                 # CKS) AND
        (W.ENHANCED_KEY+W.RIGHT_ALT_PRESSED  # CKS) AND
        (W.ENHANCED_KEY+W.LEFT_ALT_PRESSED   # CKS) AND
        (W.RIGHT_ALT_PRESSED                 # CKS) AND
        (W.LEFT_ALT_PRESSED                  # CKS)
  THEN
    c := ORD(KeyEv.uChar.AsciiChar);
  END;

  IF (c # 0) THEN
    cpair.LeadChar   := c;
    cpair.SecondChar := 0;
    pCP              := SYSTEM.ADR(cpair);
    RETURN pCP;
  END;

  (* Create value using scancode and Alt Ctrl & Shift atates *)
  IF( W.ENHANCED__KEY IN CKS) THEN
    (*
     * Find the appropriate entry in EnhancedKeys[]
     *)
    FOR i:=0 TO NUM_EKA_ELTS-1 DO
      IF ( EnhancedKeys[i].ScanCode = KeyEv.wVirtualScanCode ) THEN
        IF (W.LEFT__ALT_PRESSED IN CKS) OR (W.RIGHT__ALT_PRESSED IN CKS) THEN
          pCP := SYSTEM.ADR(EnhancedKeys[i].AltChars);
        ELSIF (W.LEFT__CTRL_PRESSED IN CKS) OR (W.RIGHT__CTRL_PRESSED IN CKS) THEN
          pCP := SYSTEM.ADR(EnhancedKeys[i].CtrlChars);
        ELSIF (W.SHIFT__PRESSED IN CKS) THEN
          pCP := SYSTEM.ADR(EnhancedKeys[i].ShiftChars);
        ELSE
          pCP := SYSTEM.ADR(EnhancedKeys[i].RegChars);
        END;
      END;
    END;
  ELSIF (KeyEv.wVirtualScanCode < NUM_NKA_ELTS) THEN
  (*
   * Regular key or a keyboard event which shouldn't be recognized.
   * Determine which by getting the proper field of the proper
   * entry in NormalKeys[], and examining the extended code.
   *)
    IF (W.LEFT__ALT_PRESSED IN CKS) OR (W.RIGHT__ALT_PRESSED IN CKS) THEN
       pCP := SYSTEM.ADR(NormalKeys[KeyEv.wVirtualScanCode].AltChars);
    ELSIF (W.LEFT__CTRL_PRESSED IN CKS) OR (W.RIGHT__CTRL_PRESSED IN CKS) THEN
       pCP := SYSTEM.ADR(NormalKeys[KeyEv.wVirtualScanCode].CtrlChars);
    ELSIF (W.SHIFT__PRESSED IN CKS) THEN
       pCP := SYSTEM.ADR(NormalKeys[KeyEv.wVirtualScanCode].ShiftChars);
    ELSE
       pCP := SYSTEM.ADR(NormalKeys[KeyEv.wVirtualScanCode].RegChars);
    END;

    IF (pCP^.SecondChar = 0) AND (pCP^.LeadChar = 0) THEN
      pCP := NIL;
    END;
  END;
  RETURN pCP;
END getextendedkeycode;


PROCEDURE waitQueB();
BEGIN
  W.WaitForSingleObject (hQueSem, W.INFINITE);
  W.ResetEvent (hQueSem);
END waitQueB;


PROCEDURE releaseQue();
BEGIN
  W.SetEvent (hQueSem);
END releaseQue;


PROCEDURE ["StdCall"] rdConsoleThread (p :W.PVOID) :W.DWORD;
VAR
  ConInpRec            : ARRAY [0..0] OF W.INPUT_RECORD;
  NumRead              : LONGCARD;
  pCP                  : PCHARPAIR;
  pMouHndl             : PMOUHNDLLIST;
  mouEvt               : MOUSE_EVT;
BEGIN

  LOOP
    WHILE (W.WaitForSingleObject(KbdHndl,W.INFINITE) # W.WAIT_OBJECT_0) DO
      W.Sleep(10);
    END;
    IF (NOT W.ReadConsoleInput( KbdHndl, ConInpRec, 1, NumRead )) OR (NumRead = 0) THEN
    ELSIF ((ConInpRec[0].EventType = W.KEY_EVENT) AND
       (ConInpRec[0].Event.KeyEvent.bKeyDown    ) ) THEN
      pCP := getextendedkeycode(ConInpRec[0].Event.KeyEvent);
      IF (pCP # NIL) THEN
        waitQueB();
        IF kqTop > KQMAX THEN
          releaseQue();
          W.Beep(300,100);
        ELSE
          KeyQue[kqTop].LeadChar   := pCP^.LeadChar;
          KeyQue[kqTop].SecondChar := pCP^.SecondChar;
          KeyQue[kqTop].keyEvt     := ConInpRec[0].Event.KeyEvent;
          INC(kqTop);
          IF (kqTop = 1) THEN W.SetEvent (hKeySem); END;
          releaseQue();
        END;
      END;
    ELSIF (ConInpRec[0].EventType = W.MOUSE_EVENT) THEN
      mouEvt.x  := ConInpRec[0].Event.MouseEvent.dwMousePosition.X;
      mouEvt.y  := ConInpRec[0].Event.MouseEvent.dwMousePosition.Y;
      mouEvt.fl := FLSET{};

      IF (W.FROM__LEFT_1ST_BUTTON_PRESSED IN ConInpRec[0].Event.MouseEvent.dwButtonState) THEN
        INCL(mouEvt.fl, L_PRESSED);
      END;
      IF (W.RIGHTMOST__BUTTON_PRESSED IN ConInpRec[0].Event.MouseEvent.dwButtonState) THEN
        INCL(mouEvt.fl, R_PRESSED);
      END;
      IF (W.FROM__LEFT_2ND_BUTTON_PRESSED IN ConInpRec[0].Event.MouseEvent.dwButtonState) THEN
        INCL(mouEvt.fl, M_PRESSED);
      END;

      pMouHndl := mouLst;
      LOOP
        IF pMouHndl # NIL THEN
          IF pMouHndl^.f( mouEvt ) THEN
            EXIT;
          ELSE
            pMouHndl := pMouHndl^.next;
          END;
        ELSE
          EXIT;
        END;
      END;
    END;
  END; -- LOOP
  RETURN 0;
END rdConsoleThread;


(*========================= I n t e r f a c e    f u n c t i o n s  =======================*)
(*========================= I n t e r f a c e    f u n c t i o n s  =======================*)
(*========================= I n t e r f a c e    f u n c t i o n s  =======================*)

PROCEDURE peekKey(VAR char, scan : SYSTEM.CARD8) : BOOLEAN;
BEGIN
  IF (kqTop = 0) THEN RETURN FALSE; END;
  waitQueB();

  char := KeyQue[0].LeadChar;
  IF (KeyQue[0].SecondChar # 0) THEN  scan := KeyQue[0].SecondChar;
  ELSE                                scan := SHORTCARD(KeyQue[0].keyEvt.wVirtualScanCode);
  END;

  releaseQue();
  RETURN TRUE;
END peekKey;


PROCEDURE getKey(VAR char, scan : SYSTEM.CARD8;
                     fWait      : BOOLEAN ) : BOOLEAN;
VAR
  i : CARDINAL;
BEGIN
  IF (kqTop = 0) AND NOT fWait THEN RETURN FALSE; END;
  W.WaitForSingleObject (hKeySem, W.INFINITE);
  ASSERT (kqTop # 0);
  waitQueB();

  char := KeyQue[0].LeadChar;
  IF (KeyQue[0].SecondChar # 0) THEN  scan := KeyQue[0].SecondChar;
  ELSE                                scan := SHORTCARD(KeyQue[0].keyEvt.wVirtualScanCode);
  END;

  FOR i := 1 TO kqTop-1 DO
    KeyQue[i-1] := KeyQue[i];
  END;
  DEC(kqTop);
  IF (kqTop = 0) THEN W.ResetEvent (hKeySem); END;

  releaseQue();
  RETURN TRUE;
END getKey;



PROCEDURE getKbf() : KBFlagSet;
(* RShift, LShift, Ctrl, Alt, Scroll, Num, Cap, Ins *)
VAR
  kbf      :KBFlagSet;
BEGIN
  kbf := KBFlagSet({});
  IF LONGCARD(W.GetAsyncKeyState(W.VK_LSHIFT))  >= 8000H THEN kbf := kbf + KBFlagSet{LShift}; END;
  IF LONGCARD(W.GetAsyncKeyState(W.VK_RSHIFT))  >= 8000H THEN kbf := kbf + KBFlagSet{RShift}; END;
  IF LONGCARD(W.GetAsyncKeyState(W.VK_CONTROL)) >= 8000H THEN kbf := kbf + KBFlagSet{Ctrl};   END;
  IF LONGCARD(W.GetAsyncKeyState(W.VK_MENU))    >= 8000H THEN kbf := kbf + KBFlagSet{Alt};    END;
  IF LONGCARD(W.GetAsyncKeyState(W.VK_SCROLL))  >= 8000H THEN kbf := kbf + KBFlagSet{Scroll}; END;
  IF LONGCARD(W.GetAsyncKeyState(W.VK_NUMLOCK)) >= 8000H THEN kbf := kbf + KBFlagSet{Num};    END;
  IF LONGCARD(W.GetAsyncKeyState(W.VK_CAPITAL)) >= 8000H THEN kbf := kbf + KBFlagSet{Cap};    END;
  IF LONGCARD(W.GetAsyncKeyState(W.VK_INSERT))  >= 8000H THEN kbf := kbf + KBFlagSet{Ins};    END;

  -- For Win95 API:
  IF (KBFlagSet{LShift,RShift}*kbf = KBFlagSet{})        AND
     (LONGCARD(W.GetAsyncKeyState(W.VK_SHIFT)) >= 8000H) THEN
    kbf := kbf + KBFlagSet{LShift,RShift};
  END;

  RETURN kbf;
END getKbf;



PROCEDURE AddMouseHandler(f : MOUHNDLFUNC);
VAR
  pNew : PMOUHNDLLIST;
BEGIN
  NEW(pNew);
  pNew^.f    := f;
  pNew^.next := mouLst;
  mouLst     := pNew;
END AddMouseHandler;


PROCEDURE RestartPipe(name: ARRAY OF CHAR);
BEGIN
   -- used in OS/2
END RestartPipe;

(*==================== Console input... =====================================================*)

CONST
  MaxRdLength = 256;
  CR          = CHR(0DH);
  LF          = CHR(0AH);
TYPE
  File = xDevData.DevData;

PROCEDURE GetConStr(VAR str : ARRAY OF CHAR) : LONGINT;
(* Returns str. length OR 0 *)
VAR
  pos  : LONGINT; -- cursor position in str
  hi   : LONGINT; -- max. char idx (-1..)
  hiini: LONGINT; -- characters entered (0..)
  winleft, winrow, winwidth, winskip : LONGINT;
  scrwidth                           : LONGINT;
  conInfo                            : W.CONSOLE_SCREEN_BUFFER_INFO;
  curpos                             : W.COORD;
  hCon                               : W.HANDLE;

CONST
  KEY_LEFT  = 4B00H;
  KEY_RIGHT = 4D00H;
  KEY_HOME  = 4700H;
  KEY_END   = 4F00H;
  KEY_DEL   = 5300H;
  KEY_BS    = 0008H;
  KEY_ENTER = 000DH;

  KEY_CHMIN = 0020H;
  KEY_CHMAX = 00FFH;

  PROCEDURE keyin():CARDINAL;
  VAR
    char,scan : SYSTEM.CARD8;
    r         : CARDINAL;
    b         : BOOLEAN;
  BEGIN
    LOOP
      b := getKey(char, scan, TRUE);
      IF (char = 0) THEN r := CARDINAL(scan) * 100H;
      ELSE               r := char;  END;
      IF (r>=KEY_CHMIN) AND (r<=KEY_CHMAX) THEN RETURN r; END;
      CASE r OF
        | KEY_LEFT, KEY_RIGHT, KEY_HOME, KEY_END, KEY_DEL, KEY_BS, KEY_ENTER:  RETURN r;
        | ELSE
      END;
    END;
  END keyin;

  PROCEDURE setcurpos();
  BEGIN
    curpos.X := winleft-winskip+pos;
    W.SetConsoleCursorPosition(hCon, curpos);   
  END setcurpos;

  PROCEDURE showstr();
  VAR
    i:    LONGINT;
    c:    ARRAY [0..0] OF CHAR;
    dw:   W.DWORD;
  BEGIN
    FOR i:=0 TO winwidth-1 DO
      IF (winskip+i < hiini) THEN
        c[0] := str[winskip+i];
      ELSE
        c[0] := ' ';
      END;
      curpos.X := winleft+i;
      W.WriteConsoleOutputCharacter(hCon, c, 1, curpos, dw);
    END;
    setcurpos();
  END showstr;

  PROCEDURE beep();
  BEGIN
    W.Beep(500,15);
  END beep;

  PROCEDURE del();
  VAR i:LONGINT;
  BEGIN
    IF (pos+1<hiini) THEN
      FOR i:=pos TO hiini-2 DO
        str[i] := str[i+1]
      END;
    END;
    DEC(hiini);
  END del;

VAR
  c    : CARDINAL;
  dw   : W.DWORD;
  CRLF : ARRAY [0..1] OF CHAR;
BEGIN
  pos      := 0;
  hi       := LONGINT(HIGH(str))-2;
  hiini    := 0;
  ASSERT(hi>=-1); (* CR + LF *)
  CRLF[0]  := CR;
  CRLF[1]  := LF;

  hCon := W.GetStdHandle(W.STD_OUTPUT_HANDLE);
  IF NOT W.GetConsoleScreenBufferInfo(hCon, conInfo) THEN
    scrwidth := 80;
    winleft  := 0;
    winrow   := 0;
    winwidth := 80;
    curpos.X := 0;
    curpos.Y := 0;
  ELSE
    scrwidth := conInfo.dwSize.X;
    winleft  := conInfo.dwCursorPosition.X;
    winrow   := conInfo.dwCursorPosition.Y;
    winwidth := scrwidth - winleft;
    curpos   := conInfo.dwCursorPosition;
  END;
  winskip  := 0;

  LOOP
    showstr();
    c := keyin();
    CASE c OF
    | KEY_LEFT:
      IF (pos>0) THEN
        DEC(pos);
        IF (pos<winskip) THEN
          winskip := pos;
          showstr();
        ELSE
          setcurpos();
        END;
      END;
    | KEY_RIGHT:
      IF (pos<hiini) THEN
        INC(pos);
        IF (pos-winskip >= winwidth) THEN
          INC(winskip);
          showstr();
        ELSE
          setcurpos();
        END;
      END;
    | KEY_HOME:
      pos     := 0;
      winskip := 0;
      showstr();
    | KEY_END:
      pos := hiini;
      IF (pos-winskip >= winwidth) THEN
        winskip := pos - winwidth +1;
      END;
      showstr();
    | KEY_BS:
      IF (pos>0) THEN
        DEC(pos);
        del();
        IF (pos<winskip) THEN  winskip := pos; END;
      END;
      showstr();
    | KEY_DEL:
      IF (pos<hiini) THEN
        del();
        showstr();
      END;
    | KEY_ENTER:
      str[hiini]   := CR;
      str[hiini+1] := LF;
      W.WriteConsole(hCon, SYSTEM.ADR(CRLF), 2, dw, NIL);
      RETURN hiini+2;
    ELSE
      IF (pos<=hi) THEN
        str[pos] := CHR(c);
        INC(pos);
        IF (pos>hiini) THEN INC(hiini); END;
        IF (pos-winskip >= winwidth) THEN INC(winskip); END;
        showstr();
      ELSE
        beep();
      END;
    END;
  END;
END GetConStr;

VAR
  Buf            : ARRAY [0..MaxRdLength] OF CHAR;
  begBuf, topBuf : LONGCARD;

(* =============== single file locks ============= *)
PROCEDURE lock(f: File);
BEGIN
  <* IF multithread THEN *> xmRTS.X2C_LockMutex(f^.lock); <* END*>
END lock;

PROCEDURE unlock(f: File);
BEGIN
  <* IF multithread THEN *> xmRTS.X2C_FreeMutex(f^.lock); <* END*>
END unlock;
(* =============== single file locks ============= *)

PROCEDURE update_buf();
BEGIN
  IF (begBuf >= topBuf) THEN
    begBuf := 0;
    topBuf := GetConStr(Buf);
  END;
END update_buf;

PROCEDURE RawRead(x: xDevData.Object; a: SYSTEM.ADDRESS; n: CARDINAL; VAR locs: CARDINAL);
  VAR f: File;
BEGIN
  IF n=0 THEN locs:=0; RETURN END;
  f:=SYSTEM.CAST(File,x^.cd);
  lock(f);
  update_buf();
  IF (begBuf >= topBuf) THEN
    x^.result:=ioc.endOfInput
  ELSE
    locs := topBuf-begBuf;
    IF (locs > n) THEN locs := n; END;
    SYSTEM.MOVE(SYSTEM.ADR(Buf), a, locs);
    x^.result:=ioc.allRight;
  END;
  unlock(f);
END RawRead;

PROCEDURE Look(x: xDevData.Object; VAR ch: CHAR; VAR res: ioc.ReadResults);
  VAR f: File;
BEGIN
  f:=SYSTEM.CAST(File,x^.cd);
  lock(f);
  update_buf();
  IF (begBuf >= topBuf) THEN
    res:=ioc.endOfInput;
  ELSE
    ch := Buf[begBuf];
    IF (ch=CR) OR (ch=LF)THEN res:=ioc.endOfLine;
    ELSE                      res:=ioc.allRight;
    END;
  END;
  x^.result:=res;
  unlock(f);
END Look;

PROCEDURE Skip(x: xDevData.Object);
  VAR f: File;
BEGIN
  f:=SYSTEM.CAST(File,x^.cd);
  lock(f);
  update_buf();
  IF (begBuf >= topBuf) THEN
    IOLink.RAISEdevException(x^.cid,x^.did,IOChan.skipAtEnd,"");
  ELSE
    IF (Buf[begBuf] = CR) OR (Buf[begBuf] = LF) THEN
      begBuf := topBuf;
    ELSE
      INC(begBuf);
    END;
  END;
  unlock(f);
  x^.result:=ioc.allRight;
END Skip;

PROCEDURE SkipLook(x: xDevData.Object; VAR ch: CHAR; VAR res: ioc.ReadResults);
BEGIN
  Skip(x); Look(x,ch,res);
END SkipLook;

PROCEDURE TextRead(x: xDevData.Object; a: SYSTEM.ADDRESS; n: CARDINAL; VAR locs: CARDINAL);
  VAR f: File;
    ch : CHAR;
    res: INTEGER;
BEGIN
  f:=SYSTEM.CAST(File,x^.cd);
  lock(f);
  update_buf();
  locs:=0;
  WHILE (locs<n) DO
    IF (begBuf >= topBuf) THEN
      x^.result:=ioc.endOfInput;
      unlock(f);
      RETURN
    END;
    ch := Buf[begBuf];
    IF (ch=CR) OR (ch=LF) THEN
      IF locs>0 THEN x^.result:=ioc.allRight
      ELSE x^.result:=ioc.endOfLine
      END;
      unlock(f);
      RETURN
    END;
    SYSTEM.PUT(a,ch);
    a:=SYSTEM.ADDADR(a,SIZE(CHAR));
    INC(begBuf);
    INC(locs);
  END;
  unlock(f);
  x^.result:=ioc.allRight;
END TextRead;

PROCEDURE mk_stdin_channel();
VAR
  cid     : StdChans.ChanId;
  did     : IOLink.DeviceId;
  x       : IOLink.DeviceTablePtr;
  f       : xlibOS.X2C_OSFHANDLE;
  fn      : xDevData.FileName;
  res     : ChanConsts.OpenResults;
BEGIN
  IOLink.AllocateDeviceId(did);
  IF (xlibOS.X2C_fGetStd(f,xlibOS.X2C_fStdIn) # 0) THEN  RETURN; END;
  xDevData.MakeName(fn,"",res);
  IOLink.MakeChan(did,cid);
  IF cid#IOChan.InvalidChan() THEN
    x := IOLink.DeviceTablePtrValue(cid,did,IOChan.notAvailable,"");
    IF xlibOS.X2C_IsMixAllowed()
    THEN xDevData.Open(x, f, fn, ChanConsts.read+ChanConsts.text+ChanConsts.raw, xDevData.bmLine,res);
    ELSE xDevData.Open(x, f, fn, ChanConsts.read+ChanConsts.text,                xDevData.bmLine,res);
    END;
    IF (res=ChanConsts.opened) THEN
      x^.doRawRead  := RawRead;
      x^.doTextRead := TextRead;
      x^.doSkip     := Skip;
      x^.doLook     := Look;
      x^.doSkipLook := SkipLook;
      StdChans.SetInChan(cid);
    END;
  END;
END mk_stdin_channel;
(*=========================================================================================*)



BEGIN
  kqTop   := 0;
  mouLst  := NIL;
  KbdHndl := xtsIGraph.hConsoleInput;

  IF W.GetConsoleMode(KbdHndl, conOldState) AND
     W.SetConsoleMode(KbdHndl, W.ENABLE_LINE_INPUT + W.ENABLE_ECHO_INPUT + W.ENABLE_PROCESSED_INPUT + W.ENABLE_MOUSE_INPUT)
  THEN
    mk_stdin_channel();
  END;

  hQueSem := W.CreateEvent ( NIL, TRUE, TRUE, NIL ); -- posted
  hKeySem := W.CreateEvent ( NIL, TRUE, FALSE,  NIL ); -- blocked
  IF (hQueSem = NIL) OR (hKeySem = NIL) THEN
    ASSERT(FALSE);
  ELSE
    hThr    := W.CreateThread(NIL, 7fffh, rdConsoleThread, NIL, W.CREATE_SET{}, tidThr);
    ASSERT(hThr # NIL);
  END;

FINALLY
  W.SetConsoleMode( KbdHndl, conOldState);
END xtsEvQue.
