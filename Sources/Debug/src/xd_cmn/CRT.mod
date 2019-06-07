<* Storage+ *>
IMPLEMENTATION MODULE CRT;

IMPORT sys := SYSTEM;
IMPORT fmt := FormStr;
IMPORT io  := InOut;

IMPORT con := Console;

IMPORT key := Keys;
IMPORT eve := DlgEvent;
IMPORT win := Dlg_Win;
IMPORT act := Dlg_Acts;
IMPORT dlt := DlgTypes;

IMPORT xStr;
IMPORT pro := Protocol;
IMPORT opt := Options;

IMPORT xdt := XD_Title;

IMPORT kexe := KrnExec;

FROM SYSTEM IMPORT CARD16;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;

<* IF env_target = 'x86os2' THEN *>

  IMPORT os2 := OS2;
  IMPORT utl := Krn_Dbg;
  
<* ELSIF env_target = 'x86nt' THEN *>

  IMPORT WIN := Windows;
  IMPORT utl := Krn_Dbg;

<* END *>


PROCEDURE Beep;
BEGIN
  IF opt.WarningBell THEN con.Beep; END;
END Beep;

TYPE

  SCREEN = con.LOGICAL_SCREEN;

  MAP    = POINTER TO ARRAY OF HWND;


VAR
  X,Y  : CARDINAL;
  BufferSize: CARDINAL;  (* Размер экранного буфера в байтах *)

  Scr : SCREEN;          (* Хранение текущего состояния экрана       *)
  Map : MAP;             (* Основная карта текущего состояния экрана *)

  ChangingWindow: SCREEN;(* Экран, и карта, в которых есть окно      *)
  ChangingMap   :    MAP;(* у которого меняется место/размер         *)

  StaticWindows: SCREEN; (* Экран, и карта, в которых окно           *)
  StaticMap:    MAP;     (* у которого меняется место/размер ЗАКРЫТО *)


PROCEDURE PutSymbol(x,y: CARDINAL; ch:CHAR; attr: ATTR);
BEGIN
  con.OutSymbol(x,y, ch, attr);
END PutSymbol;

PROCEDURE Min(a,b: CARDINAL): CARDINAL;
BEGIN
  IF a > b THEN RETURN b ELSE RETURN a END;
END Min;

PROCEDURE Attr(foreground, background: COLOR): ATTR;
BEGIN
  RETURN sys.CARD8(sys.CARD8(background)*16 + sys.CARD8(foreground));
END Attr;

PROCEDURE Fg(attr: ATTR): COLOR;
BEGIN
  RETURN VAL(COLOR, sys.CARD8(attr) MOD 16);
END Fg;

PROCEDURE Bg(attr: ATTR): COLOR;
BEGIN
  RETURN VAL(COLOR, sys.CARD8(attr) DIV 16);
END Bg;


PROCEDURE BringColor (c: COLOR): COLOR;
BEGIN
  CASE c OF
  | Black     : RETURN DarkGray;
  | Blue      : RETURN LightBlue;
  | Green     : RETURN LightGreen;
  | Cyan      : RETURN LightCyan;
  | Red       : RETURN LightRed;
  | Magenta   : RETURN LightMagenta;
  | Brown     : RETURN Yellow;
  | LightGray : RETURN White;
  | DarkGray  : RETURN LightGray;
  ELSE
    RETURN c;
  END;
END BringColor;


PROCEDURE FindColor(str: ARRAY OF CHAR): COLOR;
VAR
  i: COLOR;
BEGIN
  FOR i := MIN(COLOR) TO MAX(COLOR) DO
    IF Names[i] = str THEN
      RETURN i;
    END;
  END;
  ASSERT(FALSE);
END FindColor;


PROCEDURE FindWindow(x,y: CARDINAL): HWND;
BEGIN
  RETURN Map^[y*Xmax+x];
END FindWindow;


PROCEDURE FindWord (x,y: CARDINAL; VAR start, end: CARDINAL; VAR word: ARRAY OF CHAR): BOOLEAN;
TYPE
  CHARSET = SET OF CHAR;
CONST
  LegalChar = CHARSET{ '0'..'9', 'A'..'z'};
VAR
  i: CARDINAL;
BEGIN
  start := x;
  LOOP
    IF NOT (Scr^[y*Xmax + x].ch IN LegalChar) THEN
      EXIT
    END;
    INC(x);
    IF x = Xmax THEN
      EXIT
    END;
  END;
  end := x - 1;
  x := start;
  LOOP
    IF NOT (Scr^[y*Xmax + x].ch IN LegalChar) THEN
      EXIT
    END;
    IF x = 0 THEN
      EXIT
    END;
    DEC(x);
  END;
  ASSERT(x # 0);
  start := x + 1;
  IF start <= end THEN
    FOR i := start TO end DO
      word[i-start] := Scr^[y*Xmax + i].ch;
    END;
    word[end-start+1] := 0C;
    RETURN TRUE;
  ELSE
    COPY('', word);
    RETURN FALSE;
  END;
END FindWord;


PROCEDURE Union(r1, r2: SZ): SZ;

  PROCEDURE Min(a,b: CARDINAL): CARDINAL;
  BEGIN
    IF a < b THEN
      RETURN a;
    ELSE
      RETURN b;
    END;
  END Min;

  PROCEDURE Max(a,b: CARDINAL): CARDINAL;
  BEGIN
    IF a > b THEN
      RETURN a;
    ELSE
      RETURN b;
    END;
  END Max;

VAR
  r: SZ;
BEGIN
  r.x1 := Min(r1.x1, r2.x1);
  r.y1 := Min(r1.y1, r2.y1);
  r.x2 := Max(r1.x2, r2.x2);
  r.y2 := Max(r1.y2, r2.y2);
  RETURN r;
END Union;

PROCEDURE UpdateRect(size: SZ);
BEGIN
  WITH size DO
    con.OutRect(Scr, x1, y1, x2, y2);
  END;
END UpdateRect;

PROCEDURE Repaint(hwnd: HWND);
VAR
  size: SZ;
  x, y: CARDINAL;
  Msg : eve.MSG;
  hndl: win.WND_PROC;
BEGIN
  size := win.GetWindowSize(hwnd);
  WITH size DO
    FOR y:=y1 TO y2 DO
      FOR x:=x1 TO x2 DO
        Map^[y*Xmax+x] := hwnd;
      END;
    END;
  END;
  hndl := win.GetHandler(hwnd);
  Msg.hwnd := hwnd;
  Msg.ID   := eve.Paint;
  Msg.par  := 1;
  hndl(hwnd, Msg);
END Repaint;

PROCEDURE FillBackground;
VAR
  x, y, pos: CARDINAL;
(*  i  : CARDINAL;
  len: CARDINAL;
  str: ARRAY [0..80] OF CHAR;
  *)
BEGIN
  pos := 0;
  FOR y:=0 TO Ymax-1 DO
    FOR x:=0 TO Xmax-1 DO
      Map^[pos] := win.Invalid_H;
      Scr^[pos].ch   :=  '▒';
      Scr^[pos].attr := Attr(Blue,LightGray);
      INC(pos);
    END;
  END;
  (*
  fmt.print(str, "%s, Version %s", xdt.PRODUCT, xdt.VERSION);
  len := LENGTH(str) - 1;
  FOR i := 0 TO len DO
    Scr^[10*Xmax + i + (Xmax - len) DIV 2].AsciiChar := str[i];
  END;
  fmt.print(str, "(c) %s", xdt.COPYRIGHT);
  len := LENGTH(str) - 1;
  FOR i := 0 TO len DO
    Scr^[11*Xmax + i + (Xmax - len) DIV 2].AsciiChar := str[i];
  END;
  *)
END FillBackground;

PROCEDURE Refresh();
BEGIN
  FillBackground;
  win.IterateForVisible(Repaint);
END Refresh;

PROCEDURE Update;
BEGIN
  con.OutRect(Scr, 0, 0, Xmax - 1, Ymax - 1);
END Update;

PROCEDURE RiseWindow(hwnd: HWND);
VAR
  x,y, pos: CARDINAL;
  size: SZ;
BEGIN
  win.UnHide(hwnd);
  size := win.GetWindowSize(hwnd);
  WITH size DO
    FOR y:=y1 TO y2 DO
      pos := y*Xmax + x1;
      FOR x:=x1 TO x2 DO
        Map^[pos] := hwnd;
        INC(pos);
      END;
    END;
  END;
END RiseWindow;

PROCEDURE HideWindow(hwnd: HWND);
VAR
  size: SZ;
BEGIN
  win.Hide(hwnd);
  Refresh;
  size := win.GetWindowSize(hwnd);
  UpdateRect(size);
END HideWindow;

PROCEDURE Save(hwnd: HWND);
BEGIN
  sys.MOVE(sys.ADR(Scr^),  sys.ADR(ChangingWindow^), BufferSize);
  sys.MOVE(sys.ADR(Map^),  sys.ADR(ChangingMap^),    SIZE(Map^));
  win.Hide(hwnd);
  Refresh;
  sys.MOVE(sys.ADR(Scr^), sys.ADR(StaticWindows^), BufferSize);
  sys.MOVE(sys.ADR(Map^), sys.ADR(StaticMap^),     SIZE(Map^));
  win.UnHide(hwnd);
END Save;


<* PUSH *> <* CHECKNIL- *> <* CHECKDINDEX- *>
PROCEDURE RestoreMapFromMove;
BEGIN
   sys.MOVE(sys.ADR(StaticMap^), sys.ADR(Map^), SIZE(Map^));
END RestoreMapFromMove;

PROCEDURE RestoreStatic(size: SZ);
VAR
  y, Xlen, pos: CARDINAL;
BEGIN
  WITH size DO
    Xlen := (x2-x1+1);
    pos := y1*Xmax + x1;
    FOR y := y1 TO y2 DO
      sys.MOVE(sys.ADR(StaticWindows^[pos]), sys.ADR(Scr^[pos]), Xlen*SIZE(con.CELL));
      INC(pos, Xmax);
    END;
  END;
END RestoreStatic;

PROCEDURE RestoreForMove(new_size, old_size: SZ);
VAR
  y, Xlen, pos, new_pos: CARDINAL;
BEGIN
  WITH old_size DO
    Xlen := (x2-x1+1);
    pos := y1*Xmax + x1;
    new_pos := new_size.y1*Xmax + new_size.x1;
    FOR y := y1 TO y2 DO
      sys.MOVE(sys.ADR(ChangingWindow^[pos]), sys.ADR(Scr^[new_pos]), Xlen*SIZE(con.CELL));
      INC(pos, Xmax);
      INC(new_pos, Xmax);
    END;
  END;
END RestoreForMove;
<* POP *>

PROCEDURE FillWindowSize(hwnd: HWND; size: SZ; ch: CHAR; attr: ATTR );
VAR
  x, y, pos: CARDINAL;
BEGIN
<* PUSH *>
<* COVERFLOW- *>
<* CHECKINDEX- *>
<* CHECKRANGE- *>
  WITH size DO
    FOR y:=y1 TO y2 DO
      pos := y*Xmax+x1;
      FOR x:=x1 TO x2 DO
        IF (Map^[pos]=hwnd) OR (Map^[pos] = win.Invalid_H) THEN
          Scr^[pos].ch   := ch;
          Scr^[pos].attr := attr;
          Map^[pos] := hwnd;
        END;
        INC(pos);
      END;
    END;
  END;
<* POP *>
END FillWindowSize;

PROCEDURE FillWindow(hwnd: HWND; ch: CHAR; attr: ATTR );
VAR
  size: SZ;
BEGIN
  size := win.GetWindowSize(hwnd);
  FillWindowSize(hwnd, size, ch, attr);
END FillWindow;


PROCEDURE PutCh(hwnd: HWND; x,y: CARDINAL; ch: CHAR; attr: ATTR);
VAR
  pos: CARDINAL;
BEGIN
  pos := y*Xmax+x;
  IF (Map^[pos] = hwnd) OR (Map^[pos] = win.Invalid_H) THEN
    Scr^[pos].ch   := ch;
    Scr^[pos].attr := attr;
  END;
END PutCh;

PROCEDURE DrawFrame(hwnd: HWND; size: SZ; fr: FRAME; attr: ATTR);
VAR
  x, y: CARDINAL;
BEGIN
 WITH size DO
   PutCh(hwnd, x1, y1, Frames[fr][UpperLeft],  attr);
   PutCh(hwnd, x2, y1, Frames[fr][UpperRight], attr);
   PutCh(hwnd, x1, y2, Frames[fr][LowerLeft],  attr);
   PutCh(hwnd, x2, y2, Frames[fr][LowerRight], attr);
   FOR x := x1+1 TO x2-1 DO
     PutCh(hwnd, x, y1, Frames[fr][UpperCenter], attr);
     PutCh(hwnd, x, y2, Frames[fr][LowerCenter], attr);
   END;
   FOR y := y1+1 TO y2-1 DO
     PutCh(hwnd, x1, y, Frames[fr][MiddleLeft],  attr);
     PutCh(hwnd, x2, y, Frames[fr][MiddleRight], attr);
   END;
 END;
END DrawFrame;


TYPE
  WND_CTRL_ICON = ARRAY [0..3] OF CHAR;

  WND_CTRLS = ARRAY dlt.WND_CTRL OF WND_CTRL_ICON;

CONST
  WndCtrls = WND_CTRLS { ""    -- WinCtrl_None
                       , "[■]" -- WinCtrl_System
                       , "[r]" -- WinCtrl_Restore
                       , "[m]" -- WinCtrl_Move
                       , "[s]" -- WinCtrl_Size
                       , "[]" -- WinCtrl_Minimize
                       , "[]" -- WinCtrl_Maximize
                       , "[x]" -- WinCtrl_Close
                       };


PROCEDURE DrawControls (hwnd: HWND; size: SZ; attr: ATTR; ctrls: dlt.WND_CTRL_SET);
VAR
  c: dlt.WND_CTRL;
  l: CARDINAL;
BEGIN
  WITH size DO
    l := 3;
    FOR c := MAX(dlt.WND_CTRL) TO MIN(dlt.WND_CTRL) BY -1 DO
      IF c IN ctrls THEN
        IF x2-x1 < l THEN RETURN; END;
        SetPos(x2-x1-l, 0);
        WrStr(hwnd, WndCtrls[c], attr);
        INC(l, LENGTH(WndCtrls[c]));
      END;
    END;
  END;
END DrawControls;


PROCEDURE GetControl (size: SZ; x, y: CARDINAL; ctrls: dlt.WND_CTRL_SET): dlt.WND_CTRL;
VAR
  c: dlt.WND_CTRL;
  l: CARDINAL;
BEGIN
  IF y = 0 THEN
    WITH size DO
      l := 3;
      FOR c := MAX(dlt.WND_CTRL) TO MIN(dlt.WND_CTRL) BY -1 DO
        IF c IN ctrls THEN
          IF x = x2-x1-l+1 THEN
            RETURN c;
          END;
          INC(l, LENGTH(WndCtrls[c]));
        END;
      END;
    END;
  END;
  RETURN dlt.WinCtrl_None;
END GetControl;


PROCEDURE DrawHeader (hwnd: HWND; size: SZ; attr: ATTR);
VAR
  header1: xStr.String;
  header2: xStr.String;
BEGIN
  win.GetHeader (hwnd, header1);
  WITH size DO
    IF x2-x1 < 8 THEN RETURN; END;
    xStr.Extract (header1, 0, x2-x1-8, header2);
    IF header2 # '' THEN
      xStr.Insert(' ', 0, header2);
      xStr.Append(' ', header2);
      SetPos(2+((x2-x1-5) - LENGTH(header2)) DIV 2, 0);
      WrStr(hwnd, header2, attr);
    END;
  END;
END DrawHeader;


PROCEDURE SetPos(x,y: CARDINAL);
BEGIN
  X := x; Y := y;
END SetPos;

PROCEDURE WrChar(hwnd: HWND; ch: CHAR; attr: ATTR);
VAR
  size: SZ;
BEGIN
  size := win.GetWindowSize(hwnd);
  WITH size DO
    PutCh(hwnd, X + x1 ,Y + y1 , ch ,attr);
  END;
END WrChar;

PROCEDURE WrNChar(hwnd: HWND; N: CARDINAL; ch: CHAR; attr: ATTR);
VAR
  size: SZ;
  i : CARDINAL;
BEGIN
  size := win.GetWindowSize(hwnd);
  WITH size DO
    IF x2-x1 < X THEN RETURN; END;
    FOR i := 1 TO Min (N, x2-x1-X)+1 DO
      PutCh(hwnd, X+x1+i-1, Y+y1 , ch ,attr);
    END;
  END;
END WrNChar;

PROCEDURE WrStr(hwnd: HWND; str-: ARRAY OF CHAR; attr: ATTR);
VAR
  i, l, len, x, y : CARDINAL;
  size: SZ;
BEGIN
  size := win.GetWindowSize(hwnd);
  WITH size DO
    x := X + x1; y := Y + y1;
    IF (x > x2) OR (y > y2) THEN RETURN END;
    i := 0;
    l := 0;
    len := LENGTH(str);
    LOOP
      IF (i >= len) OR (X+l >= x2-x1+1) THEN EXIT END;
      CASE str[i] OF
      | 0C : EXIT;
      | 11C: INC(l, 8);
      ELSE
        PutCh(hwnd, x+l, y, str[i], attr);
        INC(l);
      END;
      INC(i);
    END;
  END;
END WrStr;

PROCEDURE WrStrFromPos(hwnd: HWND; str-: ARRAY OF CHAR; attr: ATTR; pos: CARDINAL);
VAR
  i, len, x, y : CARDINAL;
  size: SZ;
BEGIN
  len := LENGTH(str);
  IF len <= pos THEN RETURN END;
  DEC(len, pos);
  size := win.GetWindowSize(hwnd);
  WITH size DO
    x := X + x1; y := Y + y1;
    IF (x > x2) OR (y > y2) THEN RETURN END;
    FOR i := 0 TO Min(len-1,x2-x) DO
      PutCh(hwnd, x+i, y, str[i+pos], attr);
    END;
  END;
END WrStrFromPos;


PROCEDURE WrNStr(hwnd: HWND; N: CARDINAL; str-: ARRAY OF CHAR; attr: ATTR);
VAR
  i : CARDINAL;
  size: SZ;
BEGIN
  IF LENGTH(str) = 0 THEN RETURN END;
  size := win.GetWindowSize(hwnd);
  WITH size DO
    IF ((X+x1) > x2) OR ((Y+y1) > y2) THEN RETURN END;
    FOR i := 1 TO Min(N,Min(LENGTH(str),x2-(x1+X)+1)) DO
      PutCh(hwnd, x1+X+i-1,y1+Y,str[i-1],attr);
    END;
  END;
END WrNStr;

PROCEDURE WrNCharAbs(ch: CHAR; N: CARDINAL; attr: ATTR);
VAR
  i: CARDINAL;
BEGIN
  FOR i := 1 TO N DO
    PutSymbol(X+i-1,Y,ch,attr);
  END;
END WrNCharAbs;

PROCEDURE WrStrAbs(str-: ARRAY OF CHAR; attr: ATTR);
VAR
  i : CARDINAL;
BEGIN
  IF LENGTH(str) = 0 THEN RETURN END;
  FOR i := 0 TO LENGTH(str)-1 DO
    PutSymbol(X+i,Y,str[i],attr);
  END;
END WrStrAbs;

PROCEDURE WrStrNAbs(str-: ARRAY OF CHAR; N: CARDINAL; attr: ATTR);
VAR
  i : CARDINAL;
BEGIN
  IF LENGTH(str) = 0 THEN RETURN END;
  FOR i := 1 TO Min(N,LENGTH(str)) DO
    PutSymbol(X+i-1,Y,str[i-1],attr);
  END;
END WrStrNAbs;

PROCEDURE Hilite(hwnd: HWND; i, pos : CARDINAL; color: COLOR);
VAR
  size: SZ;
  x   : CARDINAL;
BEGIN
  size := win.GetWindowSize(hwnd);
  WITH size DO
    IF y1+i > y2 THEN RETURN; END;
    FOR x := x1+pos TO x2 DO
      IF (Map^[(y1+i)*Xmax + x] = hwnd) THEN
        Scr^[(y1+i)*Xmax + x].attr := VAL(sys.CARD8, (sys.CARD8(Scr^[(y1+i)*Xmax + x].attr) MOD 16) + ORD(color)*16);
      END;
    END;
  END;
END Hilite;


PROCEDURE HilitePart(hwnd: HWND; i, pos1, pos2 : CARDINAL; color: COLOR);
VAR
  size: SZ;
  x   : CARDINAL;
BEGIN
  size := win.GetWindowSize(hwnd);
  WITH size DO
    IF (y1+i > y2) OR (x1+pos2 > x2) THEN RETURN; END;
    FOR x := x1+pos1 TO x1+pos2-1 DO
      IF (Map^[(y1+i)*Xmax + x] = hwnd) THEN
        Scr^[(y1+i)*Xmax + x].attr := VAL(sys.CARD8, (sys.CARD8(Scr^[(y1+i)*Xmax + x].attr) MOD 16) + ORD(color)*16);
      END;
    END;
  END;
END HilitePart;


PROCEDURE Lite (hwnd: HWND; i, pos : CARDINAL; attr: ATTR);
VAR
  size: SZ;
  x   : CARDINAL;
BEGIN
  size := win.GetWindowSize(hwnd);
  WITH size DO
    IF y1+i > y2 THEN RETURN; END;
    FOR x := x1+pos TO x2 DO
      IF (Map^[(y1+i)*Xmax + x] = hwnd) THEN
        Scr^[(y1+i)*Xmax + x].attr := attr;
      END;
    END;
  END;
END Lite;


PROCEDURE LitePart(hwnd: HWND; i, pos1, pos2 : CARDINAL; attr: ATTR);
VAR
  size: SZ;
  x   : CARDINAL;
BEGIN
  size := win.GetWindowSize(hwnd);
  WITH size DO
    IF (y1+i > y2) OR (x1+pos2 > x2) THEN RETURN; END;
    FOR x := x1+pos1 TO x1+pos2-1 DO
      IF (Map^[(y1+i)*Xmax + x] = hwnd) THEN
        Scr^[(y1+i)*Xmax + x].attr := attr;
      END;
    END;
  END;
END LitePart;

PROCEDURE Toggle;
BEGIN
    con.ToggleConsoles(Scr);;
END Toggle;

PROCEDURE ShowPackScr (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  k : CARDINAL;
BEGIN
  ASSERT(action = act.Show_scr);
  IF mode # act.mode_check THEN
    Toggle;
  <* PUSH *>
  <* WOFF903+ *>
    k := key.GetKey();
  <* POP *>
    Toggle;
  END;
  RETURN TRUE;
END ShowPackScr;

<* IF DEST_XDS THEN *>

  <* IF env_target = "x86os2" THEN *>

<* PUSH *>
<* WOFF301+ *>
PROCEDURE ["SysCall"] SwitchBack(par: os2.ULONG);
<* POP *>
BEGIN
  os2.DosSleep(4000);
  utl.SwitchToDebugger;
  os2.DosExit(0, 0);
END SwitchBack;

PROCEDURE ShowProcessWindow (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  Tid: os2.TID;
BEGIN
  ASSERT(action = act.Show_wnd);
  IF NOT kexe.ProgramContextOk OR NOT kexe.Loaded THEN RETURN FALSE; END;
  IF mode # act.mode_check THEN
    utl.SwitchToDebuggee;
    os2.DosCreateThread(Tid, SwitchBack, 0, 0, 32000);
  END;
  RETURN TRUE;
END ShowProcessWindow;

  <* ELSE *>

<* PUSH *>
<* WOFF301+ *>
PROCEDURE ["StdCall"] SwitchBack(par: WIN.PVOID): WIN.DWORD;
<* POP *>
BEGIN
  WIN.Sleep(4000);
  utl.SwitchToDebugger;
  RETURN 0;
END SwitchBack;

PROCEDURE ShowProcessWindow (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  Tid: CARDINAL;
BEGIN
  ASSERT(action = act.Show_wnd);
  IF NOT kexe.ProgramContextOk OR NOT kexe.Loaded THEN RETURN FALSE; END;
  IF mode # act.mode_check THEN
    utl.SwitchToDebuggee;
    WIN.CreateThread(NIL, 32000, SwitchBack, NIL, WIN.CREATE_SET {}, Tid);
  END;
  RETURN TRUE;
END ShowProcessWindow;

  <* END  *>
<* END  *>


VAR
  pre, post : PROC;

PROCEDURE CursorOff;
BEGIN
  con.CursorOff;
END CursorOff;

PROCEDURE CursorOn;
BEGIN
  con.CursorOn;
END CursorOn;

PROCEDURE SetCurPos(x, y: CARDINAL);
BEGIN
  con.SetCurPos(x, y);
END SetCurPos;

PROCEDURE IsCursorVisible(): BOOLEAN;
BEGIN
  RETURN con.CursorState();
END IsCursorVisible;

PROCEDURE Ini;
VAR
  Xtmp, Ytmp: CARDINAL;
BEGIN
  act.IniAction(act.Show_scr, ShowPackScr);

<* IF TARGET_x86 THEN *>
  act.IniAction(act.Show_wnd, ShowProcessWindow);
<* END *>

  Xtmp := opt.X;
  Ytmp := opt.Y;
  CASE con.IniConsole(Xtmp, Ytmp) OF
  | con.Ok:
    opt.SetXY (Xtmp, Ytmp);
    Xmax := opt.X; Ymax := opt.Y;
  | con.WrongParams:
    io.WriteString('XD does not support such video mode!');
    io.WriteLn;
    HALT (3);
  | con.Error:
    io.WriteString('Error while initializing console!');
    io.WriteLn;
    HALT (4);
  ELSE
    io.WriteString('Unknow error!');
    io.WriteLn;
    HALT (5);
  END;

  BufferSize := Xmax * Ymax * SIZE(con.CELL);

  NEW(Scr, Ymax*Xmax);
  NEW(Map, Ymax*Xmax);

  NEW(ChangingWindow, Ymax*Xmax);
  NEW(StaticWindows,  Ymax*Xmax);

  NEW(ChangingMap, Ymax*Xmax);
  NEW(StaticMap,   Ymax*Xmax);

  pre := pro.pre_msg;
  post := pro.post_msg;
  pro.pre_msg := Toggle;
  pro.post_msg := Toggle;
  CursorOff;

  FillBackground;
  Update;
END Ini;

PROCEDURE Exi;
BEGIN
  pro.pre_msg := pre;
  pro.post_msg := post;

  Scr         := NIL;
  DISPOSE(Scr);

  DISPOSE(Map);

  DISPOSE(ChangingWindow);
  DISPOSE(ChangingMap);

  DISPOSE(StaticWindows);
  DISPOSE(StaticMap);

  con.CloseConsole;

  CursorOn;
END Exi;

BEGIN
  act.IniAction(act.Show_scr, ShowPackScr);

<* IF TARGET_x86 THEN *>
  act.IniAction(act.Show_wnd, ShowProcessWindow);
<* END *>


  Frames := ALL_FRAMES{ FRAME_IMAGE{'┌',   '─',  '┐',  '│', 260C,  '│',  '└',  '─',  '┘', 0C}
                      , FRAME_IMAGE{'╔',   '═',  '╗',  '║', 260C,  '║',  '╚',  '═',  '╝', 0C}
                      , FRAME_IMAGE{333C, 337C, 333C, 333C, 260C, 333C, 333C, 334C, 333C, 0C} };

  X := 0;
  Y := 0;
  Pulldown[Pull_Background]    := Attr(White,     Blue);
  Pulldown[Pull_Button]        := Attr(LightGray, Blue);
  Pulldown[Pull_CurrentButton] := Attr(White,     Black);
  Pulldown[Pull_Lite]          := Attr(White,     Blue);
  Pulldown[Pull_CurrentLite]   := Attr(Yellow,    Black);

  Menu[Menu_Background]    := Attr(White,     Blue);
  Menu[Menu_Frame]         := Attr(DarkGray,  Blue);
  Menu[Menu_Button]        := Attr(LightGray, Blue);
  Menu[Menu_CurrentButton] := Attr(White,     Black);
  Menu[Menu_Lite]          := Attr(White,     Blue);
  Menu[Menu_CurrentLite]   := Attr(Yellow,    Black);
  Menu[Menu_Inactive]      := Attr(DarkGray,  Blue);
  Menu[Menu_CurrInactive]  := Attr(DarkGray,  Black);

  Src[Src_Background]    := Attr(White,      Black);
  Src[Src_Header]        := Attr(Yellow,     Black);
  Src[Src_ActiveHeader]  := Attr(Black,      White);
  Src[Src_Code]          := Attr(LightGreen, Black);
  Src[Src_NoCode]        := Attr(DarkGray,   Black);
  Src[Src_Disasm]        := Attr(LightGray,  Black);
  Src[Src_Break]         := Attr(Yellow,     Black);
  Src[Src_BreakDisabled] := Attr(LightGray,  Black);
  Src[Src_UserCursor]    := Attr(White,      Blue);
  Src[Src_ExecCursor]    := Attr(White,      Red);
  Src[Src_CallCursor]    := Attr(White,      Green);

<* IF TARGET_VAX THEN *>

  Model[IO_Background]    := Attr(White,     Blue);
  Model[IO_Frame]         := Attr(LightGray, Blue);
  Model[IO_Header]        := Attr(White,     Black);
  Model[IO_Button]        := Attr(Black,     LightGray);
  Model[IO_CurrentButton] := Attr(White,     Black);
  Model[IO_Msg]           := Attr(White,     Blue);
  Model[IO_Byte]          := Attr(White,     Blue);
  Model[IO_CurrentByte]   := Attr(White,     Black);
  Model[IO_Info]          := Attr(Yellow,    Blue);
  Model[IO_Comment]       := Attr(DarkGray,  Blue);

<* END *>

  Help_Background  := Attr(White,    Blue);
  Modules_Active   := Attr(Black,    LightGray);
  Modules_Inactive := Attr(DarkGray, LightGray);


  List[List_Background]   := Attr(White, Black);
  List[List_Frame]        := Attr(White, Black);
  List[List_Header]       := Attr(White, Black);
  List[List_ActiveHeader] := Attr(Black, White);
  List[List_Line]         := Attr(White, Black);
  List[List_CurrentLine]  := Attr(White, Blue);

  Reg[Reg_Background]   := Attr(White,   Black);
  Reg[Reg_Frame]        := Attr(White,   Black);
  Reg[Reg_Header]       := Attr(White,   Black);
  Reg[Reg_ActiveHeader] := Attr(Black,   White);
  Reg[Reg_Normal]       := Attr(White,   Black);
  Reg[Reg_Current]      := Attr(White,   Blue);
  Reg[Reg_Written]      := Attr(LightRed,Black);

<* IF TARGET_VAX THEN *>

  Reg[Reg_Readed]       := Attr(Black, Green);
  Reg[Reg_Both]         := Attr(Red,   Green);

<* END *>

  Dump[Dump_Background]   := Attr(Black,     Black);
  Dump[Dump_Frame]        := Attr(LightGray, Black);
  Dump[Dump_Header]       := Attr(White,     Black);
  Dump[Dump_ActiveHeader] := Attr(Black,     White);
  Dump[Dump_Address]      := Attr(Green,     Black);
  Dump[Dump_Mem]          := Attr(White,     Black);
  Dump[Dump_CurMem]       := Attr(Black,     White);

  Stack := Dump;

  Msg[Msg_Background] := Attr(White, Blue);
  Msg[Msg_Frame]      := Attr(White, Blue);
  Msg[Msg_Header]     := Attr(White, Blue);
  Msg[Msg_Message]    := Attr(White, Blue);
  Msg[Msg_Button]     := Attr(Black, LightGray);

  Error[Msg_Background] := Attr(White, Red);
  Error[Msg_Frame]      := Attr(White, Red);
  Error[Msg_Header]     := Attr(White, Red);
  Error[Msg_Message]    := Attr(White, Red);
  Error[Msg_Button]     := Attr(Black, LightGray);

  Info[Msg_Background] := Attr (White, Cyan);
  Info[Msg_Frame]      := Attr (White, Cyan);
  Info[Msg_Header]     := Attr (White, Cyan);
  Info[Msg_Message]    := Attr (White, Cyan);
  Info[Msg_Button]     := Attr (Black, LightGray);

  PopupWindow [ Popup_Background   ] := Attr(White,     Blue);
  PopupWindow [ Popup_Frame        ] := Attr(DarkGray,  Blue);
  PopupWindow [ Popup_Line         ] := Attr(LightGray, Blue);
  PopupWindow [ Popup_CurrentLine  ] := Attr(White,     Black);
  PopupWindow [ Popup_Inactive     ] := Attr(DarkGray,  Blue);
  PopupWindow [ Popup_CurrInactive ] := Attr(DarkGray,  Black);

  -- Атрибуты элементов цветовой схемы для окон
  -- структурных переменных по умолчанию
  StructAttr[ Struct_Background    ] := Attr(White, Black);
  StructAttr[ Struct_Frame         ] := Attr(White, Black);
  StructAttr[ Struct_Header        ] := Attr(White, Black);
  StructAttr[ Struct_ActiveHeader  ] := Attr(Black, White);
  StructAttr[ Struct_Line          ] := Attr(White, Black);
  StructAttr[ Struct_CurrentLine   ] := Attr(White, Blue);
  StructAttr[ Struct_AlternateLine ] := Attr(White, Black);

  -- Атрибуты элементов цветовой схемы для
  -- диалоговых окон по умолчанию
  DialogAttr[ Dialog_Background     ] := Attr(White,     Blue);
  DialogAttr[ Dialog_ActiveEditor   ] := Attr(White,     Black);
  DialogAttr[ Dialog_InactiveEditor ] := Attr(Black,     LightGray);
  DialogAttr[ Dialog_Message        ] := Attr(White,     Blue);
  DialogAttr[ Dialog_Button         ] := Attr(Black,     LightGray);
  DialogAttr[ Dialog_ActiveButton   ] := Attr(White,     Black);
  DialogAttr[ Dialog_RadioHeader    ] := Attr(White,     Blue);
  DialogAttr[ Dialog_RadioActive    ] := Attr(White,     Blue);
  DialogAttr[ Dialog_RadioInactive  ] := Attr(LightGray, Blue);
  DialogAttr[ Dialog_CheckHeader    ] := Attr(White,     Blue);
  DialogAttr[ Dialog_CheckActive    ] := Attr(White,     Blue);
  DialogAttr[ Dialog_CheckInactive  ] := Attr(LightGray, Blue);
  DialogAttr[ Dialog_Lite           ] := Attr(Yellow,    Blue);


<* IF DEST_K26 THEN *>

  NEW(Pallette, 14);

  Pallette^[00] := PALLETTE_ITEM{ 'Pulldown'         , 'Основное меню'         , ORD(HIGH(Pulldown))+1    , sys.ADR(Pulldown)    , sys.ADR(Pull_Names)   };
  Pallette^[01] := PALLETTE_ITEM{ 'Menus'            , 'Меню второго уровня'   , ORD(HIGH(Menu))+1        , sys.ADR(Menu)        , sys.ADR(Menu_Names)   };
  Pallette^[02] := PALLETTE_ITEM{ 'Source_Window'    , 'Исходный текст'        , ORD(HIGH(Src))+1         , sys.ADR(Src)         , sys.ADR(Src_Names)    };
  Pallette^[03] := PALLETTE_ITEM{ 'List_Window'      , 'Списки'                , ORD(HIGH(List))+1        , sys.ADR(List)        , sys.ADR(List_Names)   };
  Pallette^[04] := PALLETTE_ITEM{ 'Error_Window'     , 'Окно ошибок'           , ORD(HIGH(Error))+1       , sys.ADR(Error)       , sys.ADR(Msg_Names)    };
  Pallette^[05] := PALLETTE_ITEM{ 'Message_Window'   , 'Окно предупреждений'   , ORD(HIGH(Msg))+1         , sys.ADR(Msg)         , sys.ADR(Msg_Names)    };
  Pallette^[06] := PALLETTE_ITEM{ 'Info_Window'      , 'Окно сообщений'        , ORD(HIGH(Info))+1        , sys.ADR(Info)        , sys.ADR(Msg_Names)    };
  Pallette^[07] := PALLETTE_ITEM{ 'Register_Window'  , 'Окно регистров'        , ORD(HIGH(Reg))+1         , sys.ADR(Reg)         , sys.ADR(Reg_Names)    };
  Pallette^[08] := PALLETTE_ITEM{ 'Dump_Window'      , 'Окно дампа памяти'     , ORD(HIGH(Dump))+1        , sys.ADR(Dump)        , sys.ADR(Dump_Names)   };
  Pallette^[09] := PALLETTE_ITEM{ 'Stack_Window'     , 'Окно стека'            , ORD(HIGH(Stack))+1       , sys.ADR(Stack)       , sys.ADR(Dump_Names)   };
  Pallette^[10] := PALLETTE_ITEM{ 'Popup_Window'     , 'Действия'              , ORD(HIGH(PopupWindow))+1 , sys.ADR(PopupWindow) , sys.ADR(Popup_Names)  };
  Pallette^[11] := PALLETTE_ITEM{ 'Struct_Var_Window', 'Структурные переменные', ORD(HIGH(StructAttr))+1  , sys.ADR(StructAttr)  , sys.ADR(Struct_Names) };
  Pallette^[12] := PALLETTE_ITEM{ 'Dialog_Window'    , 'Диалоговые окна'       , ORD(HIGH(DialogAttr))+1  , sys.ADR(DialogAttr)  , sys.ADR(Dialog_Names) };
  Pallette^[13] := PALLETTE_ITEM{ 'IO_Model_Window'  , 'Окно моделирования'    , ORD(HIGH(Model))+1       , sys.ADR(Model)       , sys.ADR(IO_Names)     };

<* ELSE *>

  NEW(Pallette, 13);

  Pallette^[00] := PALLETTE_ITEM{ 'Pulldown'          , 'Pulldown'     , ORD(HIGH(Pulldown))+1    , sys.ADR(Pulldown)    , sys.ADR(Pull_Names)   };
  Pallette^[01] := PALLETTE_ITEM{ 'Menus'             , 'Menus'        , ORD(HIGH(Menu))+1        , sys.ADR(Menu)        , sys.ADR(Menu_Names)   };
  Pallette^[02] := PALLETTE_ITEM{ 'Source_Window'     , 'Source'       , ORD(HIGH(Src))+1         , sys.ADR(Src)         , sys.ADR(Src_Names)    };
  Pallette^[03] := PALLETTE_ITEM{ 'List_Window'       , 'Lists'        , ORD(HIGH(List))+1        , sys.ADR(List)        , sys.ADR(List_Names)   };
  Pallette^[04] := PALLETTE_ITEM{ 'Error_Window'      , 'Errors'       , ORD(HIGH(Error))+1       , sys.ADR(Error)       , sys.ADR(Msg_Names)    };
  Pallette^[05] := PALLETTE_ITEM{ 'Message_Window'    , 'Warning'      , ORD(HIGH(Msg))+1         , sys.ADR(Msg)         , sys.ADR(Msg_Names)    };
  Pallette^[06] := PALLETTE_ITEM{ 'Info_Window'       , 'Messages'     , ORD(HIGH(Info))+1        , sys.ADR(Info)        , sys.ADR(Msg_Names)    };
  Pallette^[07] := PALLETTE_ITEM{ 'Register_Window'   , 'Registers'    , ORD(HIGH(Reg))+1         , sys.ADR(Reg)         , sys.ADR(Reg_Names)    };
  Pallette^[08] := PALLETTE_ITEM{ 'Dump_Window'       , 'Dump'         , ORD(HIGH(Dump))+1        , sys.ADR(Dump)        , sys.ADR(Dump_Names)   };
  Pallette^[09] := PALLETTE_ITEM{ 'Stack_Window'      , 'Stack'        , ORD(HIGH(Stack))+1       , sys.ADR(Stack)       , sys.ADR(Dump_Names)   };
  Pallette^[10] := PALLETTE_ITEM{ 'Popup_Window'      , 'Popup window' , ORD(HIGH(PopupWindow))+1 , sys.ADR(PopupWindow) , sys.ADR(Popup_Names)  };
  Pallette^[11] := PALLETTE_ITEM{ 'Struct_Var_Window' , 'Structures'   , ORD(HIGH(StructAttr))+1  , sys.ADR(StructAttr)  , sys.ADR(Struct_Names) };
  Pallette^[12] := PALLETTE_ITEM{ 'Dialog_Window'     , 'Dialogs'      , ORD(HIGH(DialogAttr))+1  , sys.ADR(DialogAttr)  , sys.ADR(Dialog_Names) };

<* END *>

FINALLY
  DISPOSE(Pallette);
END CRT.
