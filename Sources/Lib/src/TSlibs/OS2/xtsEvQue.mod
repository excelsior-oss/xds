(* Copyright (C) 1999-2000 Excelsior. *)

<*+M2ADDTYPES   *>
<*+M2EXTENSIONS *>

<*-IOVERFLOW    *>
<*-COVERFLOW    *>
<*-CHECKINDEX   *>
<*-CHECKRANGE   *>
<*-CHECKNIL     *>
<*+STORAGE      *>

IMPLEMENTATION MODULE xtsEvQue;

IMPORT StdChans,
       IOLink,
       IOChan,
       Strings,
       xlibOS,
       xDevData,
       ChanConsts,
       ioc:=IOConsts,
       EXCEPTIONS,
       TERMINATION,
       SYSTEM,
       O := OS2;

<* IF multithread THEN *>
  IMPORT xmRTS;
<* END*>

FROM SYSTEM IMPORT CARD8, CARD16, CARD32, SET32, ADR;

VAR
  source :EXCEPTIONS.ExceptionSource;

PROCEDURE CheckRC (rc: O.APIRET; s-: ARRAY OF CHAR);
VAR m: ARRAY [0..79] OF CHAR;
BEGIN
  IF rc <> O.NO_ERROR THEN
    Strings.Concat("TS-like console I/O initialization error: ",s,m);
    EXCEPTIONS.RAISE(source,rc,m);
  END;
END CheckRC;


(*********************** Key code translation for WM_CHAR processing... *************************************)

CONST
  KBD_HI = 89;
  STDIN  = 0;

TYPE
  KBDKEY = RECORD
    scan : CARD8;
    none : CARD16;
    shift: CARD16;
    ctrl : CARD16;
    alt  : CARD16;
  END;

  KBDMAP = ARRAY [0..KBD_HI] OF KBDKEY;

  CHPIPE_MSG = RECORD
    cb    : CARD16;
    m1, m2: O.MPARAM;
  END;

CONST
  _nn_ = 0;      (* no code   *)
  _00  = 100H;   (* 00 prefix *)
  _nl  = 200H;   (* NumLock affected on this key      *)
  _ch  = 400H;   (* Better to use system usChar value *)

  KbdMap = KBDMAP {
    (*usScan,      none,        sh,        ctrl,        alt             key      *)
    (*---------------------------------------------------------------------------*)
    { 001h,        01Bh,       01Bh,       _nn_,       _nn_  },     (*  ESC      *)
    { 002h,    _ch+031h,   _ch+021h,       _nn_,   _00+078h  },     (*  1        *)
    { 003h,    _ch+032h,   _ch+040h,   _00+003h,   _00+079h  },     (*  2        *)
    { 004h,    _ch+033h,   _ch+023h,       _nn_,   _00+07ah  },     (*  3        *)
    { 005h,    _ch+034h,   _ch+024h,       _nn_,   _00+07bh  },     (*  4        *)
    { 006h,    _ch+035h,   _ch+025h,   _00+01eh,   _00+07ch  },     (*  5        *)
    { 007h,    _ch+036h,   _ch+05eh,       _nn_,   _00+07dh  },     (*  6        *)
    { 008h,    _ch+037h,   _ch+026h,       _nn_,   _00+07eh  },     (*  7        *)
    { 009h,    _ch+038h,   _ch+02ah,       _nn_,   _00+07fh  },     (*  8        *)
    { 00ah,    _ch+039h,   _ch+028h,       _nn_,   _00+080h  },     (*  9        *)
    { 00bh,    _ch+030h,   _ch+029h,       _nn_,   _00+081h  },     (*  0        *)
    { 00ch,    _ch+02dh,   _ch+05fh,   _00+01fh,   _00+082h  },     (*  -        *)
    { 00dh,    _ch+03dh,   _ch+02bh,       _nn_,   _00+083h  },     (*  =        *)
    { 00eh,        008h,       008h,   _00+07fh,   _00+00eh  },     (*  BS       *)
    { 00fh,        009h,   _00+00fh,   _00+094h,       _nn_  },     (*  TAB      *)
    { 010h,    _ch+071h,   _ch+051h,       011h,   _00+010h  },     (*  q        *)
    { 011h,    _ch+077h,   _ch+057h,       017h,   _00+011h  },     (*  w        *)
    { 012h,    _ch+065h,   _ch+045h,       005h,   _00+012h  },     (*  e        *)
    { 013h,    _ch+072h,   _ch+052h,       012h,   _00+013h  },     (*  r        *)
    { 014h,    _ch+074h,   _ch+054h,       014h,   _00+014h  },     (*  t        *)
    { 015h,    _ch+079h,   _ch+059h,       019h,   _00+015h  },     (*  y        *)
    { 016h,    _ch+075h,   _ch+055h,       015h,   _00+016h  },     (*  u        *)
    { 017h,    _ch+069h,   _ch+049h,       009h,   _00+017h  },     (*  i        *)
    { 018h,    _ch+06fh,   _ch+04fh,       00fh,   _00+018h  },     (*  o        *)
    { 019h,    _ch+070h,   _ch+050h,       010h,   _00+019h  },     (*  p        *)
    { 01ah,    _ch+05bh,   _ch+07bh,       01bh,   _00+01ah  },     (*  [        *)
    { 01bh,    _ch+03dh,   _ch+02bh,       _nn_,   _00+083h  },     (*  ]        *)
    { 01ch,        00dh,       00dh,       00ah,   _00+01ch  },     (*  Enter    *)
    { 01eh,    _ch+061h,   _ch+041h,       001h,   _00+01eh  },     (*  a        *)
    { 01fh,    _ch+073h,   _ch+053h,       013h,   _00+01fh  },     (*  s        *)
    { 020h,    _ch+064h,   _ch+044h,       004h,   _00+020h  },     (*  d        *)
    { 021h,    _ch+066h,   _ch+046h,       006h,   _00+021h  },     (*  f        *)
    { 022h,    _ch+067h,   _ch+047h,       007h,   _00+022h  },     (*  g        *)
    { 023h,    _ch+068h,   _ch+048h,       008h,   _00+023h  },     (*  h        *)
    { 024h,    _ch+06ah,   _ch+04ah,       00ah,   _00+024h  },     (*  j        *)
    { 025h,    _ch+06bh,   _ch+04bh,       00bh,   _00+025h  },     (*  k        *)
    { 026h,    _ch+06ch,   _ch+04ch,       00ch,   _00+026h  },     (*  l        *)
    { 027h,    _ch+03bh,   _ch+03ah,       _nn_,   _00+027h  },     (*  ;        *)
    { 028h,    _ch+027h,   _ch+022h,       _nn_,   _00+028h  },     (*  '        *)
    { 029h,    _ch+060h,   _ch+07eh,       _nn_,       _nn_  },     (*  ~        *)
    { 02bh,    _ch+05ch,   _ch+07ch,       01ch,       _nn_  },     (*  \        *)
    { 02ch,    _ch+07ah,   _ch+05ah,       01ah,   _00+02ch  },     (*  z        *)
    { 02dh,    _ch+078h,   _ch+058h,       018h,   _00+02dh  },     (*  x        *)
    { 02eh,    _ch+063h,   _ch+043h,       003h,   _00+02eh  },     (*  c        *)
    { 02fh,    _ch+076h,   _ch+056h,       016h,   _00+02fh  },     (*  v        *)
    { 030h,    _ch+062h,   _ch+042h,       002h,   _00+030h  },     (*  b        *)
    { 031h,    _ch+06eh,   _ch+04eh,       00eh,   _00+031h  },     (*  n        *)
    { 032h,    _ch+06dh,   _ch+04dh,       00dh,   _00+032h  },     (*  m        *)
    { 033h,    _ch+02ch,   _ch+03ch,       _nn_,   _00+033h  },     (*  ,        *)
    { 034h,    _ch+02eh,   _ch+03eh,       _nn_,   _00+034h  },     (*  .        *)
    { 035h,    _ch+02fh,   _ch+03fh,       _nn_,   _00+035h  },     (*  /        *)
    { 037h,        02ah,       02ah,   _00+096h,   _00+037h  },     (*  Gr. *    *)
    { 03bh,    _00+03bh,   _00+054h,   _00+05eh,   _00+068h  },     (*  F1       *)
    { 03ch,    _00+03ch,   _00+055h,   _00+05fh,   _00+069h  },     (*  F2       *)
    { 03dh,    _00+03dh,   _00+056h,   _00+060h,   _00+06ah  },     (*  F3       *)
    { 03eh,    _00+03eh,   _00+057h,   _00+061h,   _00+06bh  },     (*  F4       *)
    { 03fh,    _00+03fh,   _00+058h,   _00+062h,   _00+06ch  },     (*  F5       *)
    { 040h,    _00+040h,   _00+059h,   _00+063h,   _00+06dh  },     (*  F6       *)
    { 041h,    _00+041h,   _00+05ah,   _00+064h,   _00+06eh  },     (*  F7       *)
    { 042h,    _00+042h,   _00+05bh,   _00+065h,   _00+06fh  },     (*  F8       *)
    { 043h,    _00+043h,   _00+05ch,   _00+066h,   _00+070h  },     (*  F9       *)
    { 044h,    _00+044h,   _00+05dh,   _00+067h,   _00+071h  },     (*  F10      *)
    { 047h, _nl+_00+047h,  _nl+037h,   _00+077h,       _nn_  },     (*  Gr. 7    *)
    { 048h, _nl+_00+048h,  _nl+038h,   _00+08dh,       _nn_  },     (*  Gr. 8    *)
    { 049h, _nl+_00+049h,  _nl+039h,   _00+084h,       _nn_  },     (*  Gr. 9    *)
    { 04ah,        02dh,       02dh,   _00+08eh,   _00+04ah  },     (*  Gr. -    *)
    { 04bh, _nl+_00+04bh,  _nl+034h,   _00+073h,       _nn_  },     (*  Gr. 4    *)
    { 04ch, _nl+_00+04ch,  _nl+035h,   _00+08fh,       _nn_  },     (*  Gr. 5    *)
    { 04dh, _nl+_00+04dh,  _nl+036h,   _00+074h,       _nn_  },     (*  Gr. 6    *)
    { 04eh,        02bh,       02bh,   _00+090h,   _00+04eh  },     (*  Gr. +    *)
    { 04fh, _nl+_00+04fh,  _nl+031h,   _00+075h,       _nn_  },     (*  Gr. 1    *)
    { 050h, _nl+_00+050h,  _nl+032h,   _00+091h,       _nn_  },     (*  Gr. 2    *)
    { 051h, _nl+_00+051h,  _nl+033h,   _00+076h,       _nn_  },     (*  Gr. 3    *)
    { 052h,    _00+052h,       030h,   _00+092h,       _nn_  },     (*  Gr. Ins  *)
    { 053h,    _00+053h,       02eh,   _00+093h,       _nn_  },     (*  Gr. Del  *)
    { 057h,    _00+085h,   _00+087h,   _00+089h,   _00+08bh  },     (*  F11      *)
    { 058h,    _00+086h,   _00+088h,   _00+08ah,   _00+08ch  },     (*  F12      *)
    { 05ah,        00dh,       00dh,       00ah,   _00+0a6h  },     (*  Gr. Enter*)
    { 05ch,        02fh,       02fh,   _00+095h,   _00+0a4h  },     (*  Gr. /    *)
    { 05dh,        _nn_,       _nn_,   _00+072h,       _nn_  },     (*  PrtScr   *)
    { 060h,    _00+047h,   _00+047h,   _00+077h,   _00+097h  },     (*  Home     *)
    { 061h,    _00+048h,   _00+048h,   _00+08dh,   _00+098h  },     (*  Up       *)
    { 062h,    _00+049h,   _00+049h,   _00+084h,   _00+099h  },     (*  PgUp     *)
    { 063h,    _00+04bh,   _00+04bh,   _00+073h,   _00+09bh  },     (*  Left     *)
    { 064h,    _00+04dh,   _00+04dh,   _00+074h,   _00+09dh  },     (*  Right    *)
    { 065h,    _00+04fh,   _00+04fh,   _00+075h,   _00+09fh  },     (*  End      *)
    { 066h,    _00+050h,   _00+050h,   _00+091h,   _00+0a0h  },     (*  Down     *)
    { 067h,    _00+051h,   _00+051h,   _00+076h,   _00+0a1h  },     (*  PgDn     *)
    { 068h,    _00+052h,   _00+052h,   _00+092h,   _00+0a2h  },     (*  Ins      *)
    { 069h,    _00+053h,   _00+053h,   _00+093h,   _00+0a3h  }      (*  Del      *)
  }; (* KbdMap *)


PROCEDURE getextendedkeycode (m1,m2 : CARD32; VAR lastchar, lastscan : CARD8) : BOOLEAN;
VAR
  fsf    : SET32;
  usch   : CARD16;
  usscan : CARD8;
  i      : CARDINAL;
  val    : CARD16;

BEGIN
  usch   := CARD16(m2);
  usscan := CARD8(m1 DIV 1000000H);
  fsf    := SET32(m1 MOD 10000H);

  IF SET32(O.KC_KEYUP + O.KC_SCANCODE)*fsf # SET32(O.KC_SCANCODE) THEN
    RETURN FALSE;
  END;

  FOR i := 0 TO KBD_HI DO
    IF (KbdMap[i].scan = usscan) THEN
      IF    (SET32(O.KC_ALT)  *fsf # SET32{}) THEN val := KbdMap[i].alt;
      ELSIF (SET32(O.KC_CTRL) *fsf # SET32{}) THEN val := KbdMap[i].ctrl;
      ELSIF (SET32(O.KC_SHIFT)*fsf # SET32{}) THEN val := KbdMap[i].shift;
      ELSE                                         val := KbdMap[i].none;
      END;
      IF (SET32(val) * SET32(_nl + _ch) # SET32{}) THEN
        IF (usch < 100H) THEN val := usch;
        ELSE                  val := (usch DIV 100H) + _00;
        END;
      END;

      IF (val MOD 100H) = 0 THEN RETURN FALSE; END;

      IF (SET32(val) * SET32(_00) # SET32{}) THEN
        lastchar := 0;
        lastscan := val MOD 100H;
      ELSE
        lastchar := val MOD 100H;
        lastscan := 0;
      END;

      RETURN TRUE;

    END;
  END;
  RETURN FALSE;
END getextendedkeycode;


(************************************************************)


TYPE
  KEYREC = RECORD
    char : CARD8;
    scan : CARD8;
  END;
  PKEYREC = POINTER TO KEYREC;

CONST
  KQMAX        = 32;

VAR
  KeyQue      : ARRAY [0..KQMAX] OF KEYREC;
  kqTop       : [0..KQMAX+1];
  hmtxQue     : O.HMTX;       -- for queue protection
  hevChar     : O.HEV;        -- posted if queue is not empty
  tidCThr     : O.TID;
  tidPThr     : O.TID;
  hChPipe     : O.HPIPE;

PROCEDURE lockQue();
BEGIN
  O.DosRequestMutexSem(hmtxQue, O.SEM_INDEFINITE_WAIT);
END lockQue;

PROCEDURE trylockQue(): BOOLEAN;
BEGIN
  RETURN O.DosRequestMutexSem(hmtxQue, O.SEM_IMMEDIATE_RETURN) = O.NO_ERROR;
END trylockQue;

PROCEDURE unlockQue();
BEGIN
  O.DosReleaseMutexSem(hmtxQue);
END unlockQue;


PROCEDURE key2que(char, scan : CARD8);
BEGIN
  lockQue();
  IF kqTop > KQMAX THEN
    unlockQue();
    O.DosBeep(300,100);
  ELSE
    KeyQue[kqTop].char := char;
    KeyQue[kqTop].scan := scan;
    INC(kqTop);
    IF kqTop = 1 THEN O.DosPostEventSem(hevChar) END;
    unlockQue();
  END;
END key2que;

PROCEDURE mk_stdin_channel(); FORWARD;

PROCEDURE [O.EXPENTRY] rdConsoleThread (param :CARDINAL);
VAR
  CharData  : O.KBDKEYINFO;
  char,scan : CARD8;
BEGIN
  O.KbdFlushBuffer(0);
  LOOP
    CharData.bNlsShift := 0C;
    CharData.fbStatus  := 0C;
    IF (0 = O.KbdCharIn(CharData, O.IO_WAIT, 0)) AND (CharData.fbStatus # 0C) THEN
      WITH CharData DO
        scan := CARD8(chScan);
        IF (1 IN SET32(fbStatus)) THEN
          char := 0;
          key2que(char, scan);
        ELSIF chChar # 0C THEN
          char := CARD8(chChar);
          key2que(char, scan);
        END;
      END;
    END;
  END;
END rdConsoleThread;


PROCEDURE [O.EXPENTRY] rdPipeThread (param :CARDINAL);
VAR
  pipemsg   : CHPIPE_MSG;
  ul        : O.ULONG;
  char,scan : CARD8;
  rc        : O.APIRET;
BEGIN
  LOOP
    CheckRC(O.DosConnectNPipe(hChPipe),
            "Can't reset Graph pipe");
    LOOP
      CheckRC(O.DosRead(hChPipe,ADR(pipemsg),SIZE(pipemsg),ul), 
              "Can't read from Graph pipe");
      IF (ul = SIZE(pipemsg)) AND 
         getextendedkeycode (CARD32(pipemsg.m1), CARD32(pipemsg.m2),  char, scan) THEN
        key2que(char, scan);
      ELSIF ul = 0 THEN  (* Pipe has been disconnected *)
        IF TERMINATION.IsTerminating() THEN 
          RETURN
        ELSE
          EXIT
        END;
      END;
    END;
  END;
END rdPipeThread;



(*========================= I n t e r f a c e    f u n c t i o n s  =======================*)
(*========================= I n t e r f a c e    f u n c t i o n s  =======================*)
(*========================= I n t e r f a c e    f u n c t i o n s  =======================*)

PROCEDURE peekKey(VAR char, scan : SYSTEM.CARD8) : BOOLEAN;
BEGIN
  IF (kqTop = 0) THEN RETURN FALSE; END;
  lockQue();
  char := KeyQue[0].char;
  scan := KeyQue[0].scan;
  unlockQue();
  RETURN TRUE;
END peekKey;



PROCEDURE getKey(VAR char, scan : SYSTEM.CARD8;
                     fWait      : BOOLEAN ) : BOOLEAN;
VAR
  ul : O.ULONG;
  ulTimeOut : O.ULONG;
BEGIN
  IF fWait THEN 
    ulTimeOut := O.SEM_INDEFINITE_WAIT
  ELSE
    ulTimeOut := O.SEM_IMMEDIATE_RETURN
  END;
  LOOP
    IF O.DosWaitEventSem(hevChar, ulTimeOut) = O.ERROR_TIMEOUT THEN
      (* fWait = FALSE and there are no keys in the queue *)
      RETURN FALSE
    END;
    lockQue();
    IF kqTop # 0 THEN EXIT END;
    (* Oops, somebody has read all keys from the queue before we 
       had a chance to lock it *)
    unlockQue();
    IF NOT fWait THEN RETURN FALSE END;
  END;
  (* At this point, the queue is locked and not empty *)
  char := KeyQue[0].char;
  scan := KeyQue[0].scan;
  FOR ul := 1 TO kqTop-1 DO
    KeyQue[ul-1] := KeyQue[ul];
  END;
  DEC(kqTop);
  IF kqTop = 0 THEN O.DosResetEventSem(hevChar, ul) END;
  unlockQue();
  RETURN TRUE;
END getKey;



PROCEDURE getKbf() : KBFlagSet;
(* RShift, LShift, Ctrl, Alt, Scroll, Num, Cap, Ins *)
(* -------------------------------------------- *)
VAR
  kbf : KBFlagSet;
BEGIN
  kbf := KBFlagSet({});
  IF LONGCARD(O.WinGetPhysKeyState(O.HWND_DESKTOP, 2ah))       >= 8000H  THEN kbf := kbf + KBFlagSet{LShift}; END;
  IF LONGCARD(O.WinGetPhysKeyState(O.HWND_DESKTOP, 36h))       >= 8000H  THEN kbf := kbf + KBFlagSet{RShift}; END;
  IF LONGCARD(O.WinGetKeyState(O.HWND_DESKTOP, O.VK_CTRL))     >= 8000H  THEN kbf := kbf + KBFlagSet{Ctrl};   END;
  IF LONGCARD(O.WinGetKeyState(O.HWND_DESKTOP, O.VK_ALT))      >= 8000H  THEN kbf := kbf + KBFlagSet{Alt};    END;
  IF LONGCARD(O.WinGetKeyState(O.HWND_DESKTOP, O.VK_SCRLLOCK)) DIV 1 = 1 THEN kbf := kbf + KBFlagSet{Scroll}; END;
  IF LONGCARD(O.WinGetKeyState(O.HWND_DESKTOP, O.VK_NUMLOCK))  DIV 1 = 1 THEN kbf := kbf + KBFlagSet{Num};    END;
  IF LONGCARD(O.WinGetKeyState(O.HWND_DESKTOP, O.VK_CAPSLOCK)) DIV 1 = 1 THEN kbf := kbf + KBFlagSet{Cap};    END;
  IF LONGCARD(O.WinGetKeyState(O.HWND_DESKTOP, O.VK_INSERT))   DIV 1 = 1 THEN kbf := kbf + KBFlagSet{Ins};    END;

  RETURN kbf;
END getKbf;



PROCEDURE AddMouseHandler(f : MOUHNDLFUNC);
BEGIN
  (* Not used in OS/2 *)
END AddMouseHandler;

PROCEDURE RestartPipe(name: ARRAY OF CHAR);
VAR
  pszPipeName : ARRAY [0..63] OF CHAR;
BEGIN
  IF (hChPipe = O.NULLHANDLE) THEN
    CheckRC(O.DosCreateNPipe(name, hChPipe, O.NP_ACCESS_INBOUND,
                             O.NP_WAIT+O.NP_TYPE_MESSAGE+O.NP_READMODE_MESSAGE+1,
                             SIZE(CHPIPE_MSG)*32, SIZE(CHPIPE_MSG)*32, 0),
            "Can't create Graph pipe");
    CheckRC(O.DosCreateThread (tidPThr, rdPipeThread, 0, O.CREATE_READY, 7fffH),
            "Can't create Graph key event thread");
  ELSE
    O.DosDisConnectNPipe(hChPipe);
    (* DosRead shall return zero for pcbActual, resulting in rdPipeThread
       restarting the big loop.
    *) 
  END;
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
  row                                : O.USHORT;

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
    O.VioSetCurPos(row, winleft-winskip+pos, 0);
  END setcurpos;

  PROCEDURE showstr();
  VAR
    i:    LONGINT;
    c:    ARRAY [0..0] OF CHAR;
  BEGIN
    FOR i:=0 TO winwidth-1 DO
      IF (winskip+i < hiini) THEN
        c[0] := str[winskip+i];
      ELSE
        c[0] := ' ';
      END;
      O.VioWrtCharStr(c, 1, row, winleft+i, 0);
    END;
    setcurpos();
  END showstr;

  PROCEDURE beep();
  BEGIN
    O.DosBeep(500,15);
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
  c       : CARDINAL;
  vioMode : O.VIOMODEINFO;
  col     : O.USHORT;
BEGIN
  pos      := 0;
  hi       := LONGINT(HIGH(str))-2;
  hiini    := 0;
  ASSERT(hi>=-1); (* CR + LF *)

  vioMode.cb := SIZE(vioMode);
  IF O.VioGetMode(vioMode, 0)    # O.NO_ERROR THEN scrwidth := 80;
  ELSE                                             scrwidth := vioMode.col; END;
  IF O.VioGetCurPos(row, col, 0) # O.NO_ERROR THEN winleft  := 0;
  ELSE                                             winleft  := col; END;
  winwidth := scrwidth - winleft;
  winskip  := 0;

  showstr();
  LOOP
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
      IOChan.RawWrite(StdChans.StdOutChan(), SYSTEM.ADR(str[hiini]), 2);
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
  handType: O.ULONG;
  flagWord: O.ULONG;
BEGIN
  O.DosQueryHType(STDIN, handType, flagWord);
  IF (handType MOD 2) # 1 THEN RETURN; END; -- it is not keyboard

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
  EXCEPTIONS.AllocateSource(source);
  kqTop   := 0;
  hChPipe := O.NULLHANDLE;
  CheckRC(O.DosCreateMutexSem (NIL, hmtxQue, 0, FALSE),    -- unowned
          "Can't create mutex sem.");
  CheckRC(O.DosCreateEventSem (NIL, hevChar, 0, FALSE),    -- reset
          "Can't create event sem.");
  CheckRC(O.DosCreateThread (tidCThr, rdConsoleThread, 0, O.CREATE_READY, 7fffH),
          "Can't create console event thread.");
  mk_stdin_channel();
FINALLY
  IF hChPipe <> O.NULLHANDLE THEN
    O.DosDisConnectNPipe(hChPipe); (* The pipe thread shall end. *)
    O.DosClose(hChPipe);
  END;
END xtsEvQue.


