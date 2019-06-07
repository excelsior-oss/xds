<* Storage+ *>
IMPLEMENTATION MODULE Dlg_Win;

IMPORT sys := SYSTEM;
IMPORT fmt := FormStr;
IMPORT str := Strings;

IMPORT crt := CRT;
IMPORT key := Keys;
IMPORT eve := DlgEvent;
IMPORT std := Dlg_Std;
IMPORT act := Dlg_Acts;
IMPORT dv  := Dlg_Vars;
IMPORT dlt := DlgTypes;
IMPORT dmn := Dlg_Menu;

IMPORT xStr;

IMPORT pro := Protocol;

FROM Storage IMPORT ALLOCATE;


PROCEDURE IsValid (hwnd: HWND): BOOLEAN;
BEGIN
  RETURN hwnd # Invalid_H;
END IsValid;


TYPE
  WIN_STATE = SET OF ( visible, modal, movable, resizableX, resizableY, switch, search, in_list );

  WINDOW = RECORD
             Header : xStr.String;
             state  : WIN_STATE;
             handler: WND_PROC;
             AM     : sys.ADDRESS; -- память ассоцииpуемая с конкpетным окном
             size   : crt.SZ;
             next, prev: HWND;  -- следующее и пpедыдущее окно в списке активных окон
             pair_hwnd : HWND;  (* окно, в которое можно переехать с нажатой клавишей мышки *)
           END;

  PA_WINDOW = POINTER TO ARRAY OF WINDOW;

  WINDOWLIST = RECORD
                 window : PA_WINDOW;
                 max    : CARDINAL;
               END;

VAR
  Tail: HWND;


VAR
  AllWindows: WINDOWLIST;

CONST
  EmptyWindowsList = WINDOWLIST{NIL,0};


PROCEDURE ActiveToTail;
VAR
  win: HWND;
BEGIN
  IF ActiveWindow = Tail THEN
    ASSERT(visible IN AllWindows.window^[ActiveWindow].state);
    RETURN
  END;
  REPEAT
    WITH AllWindows DO
      win := ActiveWindow;
      ActiveWindow := window^[ActiveWindow].next;
      window^[ActiveWindow].prev := Invalid_H;
      window^[win].prev  := Tail;
      window^[Tail].next := win;
      window^[win].next := Invalid_H;
      Tail := win;
    END;
  UNTIL visible IN AllWindows.window^[ActiveWindow].state;
END ActiveToTail;

PROCEDURE PopupLast;
VAR
  win: HWND;
BEGIN
  IF ActiveWindow = Tail THEN RETURN END;
  win := Tail;
  WHILE NOT Visible(win)  DO
    win := AllWindows.window^[win].prev;
  END;
  Rise(win);
END PopupLast;

PROCEDURE IterateForVisible(action: ITERATOR);
VAR
  i: CARDINAL;
BEGIN
  WITH AllWindows DO
    i := Tail;
    REPEAT
      IF visible IN window^[i].state THEN
        action(i);
      END;
      i := window^[i].prev;
    UNTIL (i = Invalid_H);
  END;
END IterateForVisible;


PROCEDURE Rise(win: HWND);
VAR
  t1, t2: HWND;
BEGIN
  IF (ActiveWindow = win) THEN RETURN END;
  WITH AllWindows DO
    IF (win = Tail) THEN
      window^[win].next := ActiveWindow;
      window^[ActiveWindow].prev := win;
      Tail := window^[win].prev;
      window^[window^[win].prev].next := Invalid_H;
      window^[win].prev := Invalid_H;
    ELSE
      t1 := window^[win].next;
      t2 := window^[win].prev;
      window^[t2].next := t1;
      window^[t1].prev := t2;
      window^[win].prev := Invalid_H;
      window^[win].next := ActiveWindow;
      window^[ActiveWindow].prev := win;
    END;
    ActiveWindow := win;
  END;
END Rise;

PROCEDURE RegisterWindow(hndl: WND_PROC; amem: CARDINAL): HWND;
VAR
  tmp : PA_WINDOW;
CONST
  Q_WND = 32;

BEGIN
  WITH AllWindows DO
    IF window = NIL THEN
      NEW(window,Q_WND);
      IF window = NIL THEN RETURN Invalid_H END;
      max  := 0;
    ELSIF max >= HIGH(window^)+1 THEN
      NEW(tmp,HIGH(window^)+1+Q_WND);
      IF window = NIL THEN RETURN Invalid_H END;
      sys.MOVE(sys.ADR(window^),sys.ADR(tmp^),(HIGH(window^)+1)*SIZE(WINDOW));
      DISPOSE(window);
      window := tmp;
    END;
    WITH window^[max] DO
      IF amem # 0 THEN
        ALLOCATE(AM,amem);
      ELSE
        AM := NIL;
      END;
      handler := hndl;
      state   := WIN_STATE{in_list};
      Header  := '';
      pair_hwnd := Invalid_H;
      IF ActiveWindow = Invalid_H THEN
        ActiveWindow := max;
        Tail         := max;
        next := Invalid_H;
        prev := Invalid_H;
      ELSE
        window^[Tail].next := max;
        next := Invalid_H;
        prev := Tail;
        Tail := max;
      END;
    END;
    INC(max);
    RETURN max-1;
  END;
END RegisterWindow;

PROCEDURE GetAMPtr(hwnd: HWND): sys.ADDRESS;
BEGIN
  ASSERT(hwnd <= AllWindows.max);
  RETURN AllWindows.window^[hwnd].AM;
END GetAMPtr;

PROCEDURE GetWindowSize(hwnd: HWND): crt.SZ;
BEGIN
  ASSERT(hwnd <= AllWindows.max);
  RETURN AllWindows.window^[hwnd].size;
END GetWindowSize;

PROCEDURE GetHandler(hwnd: HWND): WND_PROC;
BEGIN
  ASSERT(hwnd <= AllWindows.max);
  RETURN AllWindows.window^[hwnd].handler;
END GetHandler;


-- Получить заголовок окна (без пробелов впереди/позади)
PROCEDURE GetHeader (hwnd: HWND; VAR header: ARRAY OF CHAR);
BEGIN
  ASSERT(hwnd <= AllWindows.max);
  WITH AllWindows.window^[hwnd] DO
    IF Header # '' THEN
      COPY(Header, header);
      str.Delete(header, 0, 1);
      IF LENGTH(header) # 0 THEN header[LENGTH(header)-1] := ''; END;
    ELSE
      COPY('', header);
    END;
  END;
END GetHeader;


PROCEDURE GetPair(hwnd: HWND): HWND;
BEGIN
  ASSERT(hwnd <= AllWindows.max);
  RETURN AllWindows.window^[hwnd].pair_hwnd;
END GetPair;

PROCEDURE SetPair(hwnd: HWND; pair: HWND);
BEGIN
  ASSERT(hwnd <= AllWindows.max);
  AllWindows.window^[hwnd].pair_hwnd := pair;
END SetPair;


-- Установить заголовок окна (дополнить пробелами)
PROCEDURE SetHeaderByStr (hwnd: HWND; str-: ARRAY OF CHAR);
BEGIN
  ASSERT(hwnd <= AllWindows.max);
  WITH AllWindows.window^[hwnd] DO
    Header := '';
    IF str # '' THEN
      COPY(' ', Header);
      xStr.Append(str, Header);
      xStr.Append(' ', Header);
    END;
  END;
END SetHeaderByStr;


-- Установить по номеру сообщения заголовок окна
PROCEDURE SetHeader (hwnd: HWND; no: CARDINAL);
VAR
  str: xStr.txt_ptr;
BEGIN
  ASSERT(hwnd <= AllWindows.max);
  str := pro.Get(no);
  SetHeaderByStr (hwnd, str^);
END SetHeader;


PROCEDURE SetWindowSize(hwnd: HWND;size: crt.SZ);
BEGIN
  ASSERT(hwnd <= AllWindows.max);
  AllWindows.window^[hwnd].size := size;
END SetWindowSize;

PROCEDURE Visible(hwnd: HWND): BOOLEAN;
BEGIN
  ASSERT(hwnd <= AllWindows.max);
  RETURN visible IN AllWindows.window^[hwnd].state;
END Visible;



PROCEDURE Hide(hwnd: HWND);
BEGIN
  ASSERT(hwnd <= AllWindows.max);
  EXCL(AllWindows.window^[hwnd].state, visible);
END Hide;

PROCEDURE UnHide(hwnd: HWND);
BEGIN
  ASSERT(hwnd <= AllWindows.max);
  INCL(AllWindows.window^[hwnd].state, visible);
END UnHide;

PROCEDURE IsModal(hwnd: HWND): BOOLEAN;
BEGIN
  ASSERT(hwnd <= AllWindows.max);
  RETURN modal IN AllWindows.window^[hwnd].state;
END IsModal;

PROCEDURE SetModal(hwnd: HWND);
BEGIN
  ASSERT(hwnd <= AllWindows.max);
  INCL(AllWindows.window^[hwnd].state, modal);
END SetModal;


PROCEDURE ExcludeFromList (hwnd: HWND);
BEGIN
  ASSERT(hwnd <= AllWindows.max);
  EXCL(AllWindows.window^[hwnd].state, in_list);
END ExcludeFromList;


PROCEDURE IsMovable(hwnd: HWND): BOOLEAN;
BEGIN
  ASSERT(hwnd <= AllWindows.max);
  RETURN movable IN AllWindows.window^[hwnd].state;
END IsMovable;

PROCEDURE SetMovable(hwnd: HWND);
BEGIN
  ASSERT(hwnd <= AllWindows.max);
  INCL(AllWindows.window^[hwnd].state, movable);
END SetMovable;

PROCEDURE IsResizableX(hwnd: HWND): BOOLEAN;
BEGIN
  ASSERT(hwnd <= AllWindows.max);
  RETURN resizableX IN AllWindows.window^[hwnd].state;
END IsResizableX;

PROCEDURE IsResizableY(hwnd: HWND): BOOLEAN;
BEGIN
  ASSERT(hwnd <= AllWindows.max);
  RETURN resizableY IN AllWindows.window^[hwnd].state;
END IsResizableY;

PROCEDURE SetResizable(hwnd: HWND; X,Y: BOOLEAN);
BEGIN
  ASSERT(hwnd <= AllWindows.max);
  IF X THEN
    INCL(AllWindows.window^[hwnd].state, resizableX);
  END;
  IF Y THEN
    INCL(AllWindows.window^[hwnd].state, resizableY);
  END;
END SetResizable;

PROCEDURE IsSwitchable(hwnd: HWND): BOOLEAN;
BEGIN
  ASSERT(hwnd <= AllWindows.max);
  RETURN switch IN AllWindows.window^[hwnd].state;
END IsSwitchable;

PROCEDURE SetSwitchable(hwnd: HWND);
BEGIN
  ASSERT(hwnd <= AllWindows.max);
  INCL(AllWindows.window^[hwnd].state, switch);
END SetSwitchable;

PROCEDURE IsSearchable(hwnd: HWND): BOOLEAN;
BEGIN
  ASSERT(hwnd <= AllWindows.max);
  RETURN search IN AllWindows.window^[hwnd].state;
END IsSearchable;

PROCEDURE SetSearchable(hwnd: HWND);
BEGIN
  ASSERT(hwnd <= AllWindows.max);
  INCL(AllWindows.window^[hwnd].state, search);
END SetSearchable;

PROCEDURE GetPrevious (hwnd: HWND): HWND;
BEGIN
  RETURN AllWindows.window^[hwnd].next;
END GetPrevious;


PROCEDURE GetMouse (msg: eve.MSG; VAR x, y: CARDINAL): BOOLEAN;
BEGIN
  IF msg.ID IN eve.MouseEvents THEN
    x := msg.par DIV 10000H;
    y := msg.par MOD 10000H;
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END GetMouse;


PROCEDURE GetRelMouse (hwnd: HWND; msg: eve.MSG; VAR x, y: CARDINAL): BOOLEAN;
VAR
  size: crt.SZ;
BEGIN
  IF GetMouse (msg, x, y) THEN
    size := GetWindowSize(hwnd);
    DEC(x, size.x1);
    DEC(y, size.y1);
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END GetRelMouse;


PROCEDURE WinDispatchMsg(Msg: eve.MSG): BOOLEAN;
VAR
  i   : CARDINAL;
BEGIN
  IF Msg.ID <> eve.None THEN
    IF Msg.ID = eve.Quit THEN RETURN FALSE END;
    IF AllWindows.max > 0 THEN
      IF Msg.hwnd = eve.AllWindows THEN
        IF (Msg.ID = eve.Redraw) AND (Msg.par = 1) THEN
          crt.Refresh;
          crt.Update;
        ELSE
          FOR i:=0 TO AllWindows.max-1 DO
            IF visible IN AllWindows.window^[i].state THEN
              Msg.hwnd := i;
              AllWindows.window^[i].handler(i,Msg);
            END;
          END;
        END;
      ELSE
        IF ((visible IN AllWindows.window^[Msg.hwnd].state) OR (Msg.ID = eve.Rise)) THEN
          IF (Msg.ID = eve.KbHit) THEN
            IF (ActiveWindow = Msg.hwnd) THEN
              IF (Msg.par > 0FFFFH) THEN
                Msg.par := Msg.par DIV (256*256);
              END;
            ELSE
              IF (Msg.par > 0FFFFH) THEN
                Msg.par := Msg.par DIV (256*256);
              ELSE
                RETURN TRUE;
              END;
            END;
          END;
          AllWindows.window^[Msg.hwnd].handler(Msg.hwnd, Msg)
        END;
      END;
    END;
  END;
  RETURN TRUE;
END WinDispatchMsg;


PROCEDURE AllVisibleWnds(VAR max_len: CARDINAL) : CARDINAL;
VAR
  N  : CARDINAL;
  i  : CARDINAL;
  len: CARDINAL;
BEGIN
  N := 0;
  IF AllWindows.max > 0 THEN
    FOR i:=0 TO AllWindows.max-1 DO
      WITH AllWindows.window^[i] DO
        IF (visible IN state) AND NOT (modal IN state) AND
           (i # dmn.GetMenuHwnd(dlt.WindowsMenu)) AND
           (i # dv.MainPulldown) AND (Header # "")
        THEN
          INC(N);
          len := LENGTH(Header);
          IF len > max_len THEN max_len := len END;
        END;
      END;
    END;
  END;
  RETURN N;
END AllVisibleWnds;

PROCEDURE TakeN(N: CARDINAL) : CARDINAL;
VAR
  n: CARDINAL;
  w: CARDINAL;
BEGIN
  n := 0;
  w := ActiveWindow;
  REPEAT
    WITH AllWindows.window^[w] DO
      IF (WIN_STATE{visible, in_list} <= state) AND
        NOT (modal IN state) AND (Header # '')
      THEN
        IF N = n THEN
          RETURN w;
        END;
        INC(n);
      END;
      w := next;
    END;
  UNTIL (w = MAX(CARDINAL));
  RETURN MAX(CARDINAL);
END TakeN;



VAR
  W_Clr: crt.LIST_ATTR;

PROCEDURE ListWindow(hwnd: HWND; msg: eve.MSG);
VAR
  size   : crt.SZ;
  p      : std.PLIST;

  PROCEDURE write_line(num: CARDINAL);
  VAR
    tmp: xStr.String;
  BEGIN
    WITH p^ DO
      WITH AllWindows.window^[TakeN(num)] DO
        crt.SetPos(1, num - p^.frame + 1);
        COPY(Header, tmp);
        crt.WrStr(hwnd, tmp, Colors^[crt.List_Line]);
      END;
    END;
  END write_line;

VAR
  p2     : dlt.PPULLDOWN;
  y, len, last, i : CARDINAL;
  msg2: eve.MSG;
  hndl: WND_PROC;
BEGIN
  W_Clr[crt.List_Background]   := crt.Menu[crt.Menu_Background];
  W_Clr[crt.List_Frame]        := crt.Menu[crt.Menu_Frame];
  W_Clr[crt.List_Header]       := crt.Menu[crt.Menu_Frame];
  W_Clr[crt.List_ActiveHeader] := crt.Menu[crt.Menu_Frame];
  W_Clr[crt.List_Line]         := crt.Menu[crt.Menu_Button];
  W_Clr[crt.List_CurrentLine]  := crt.Menu[crt.Menu_CurrentButton];
 
  p := GetAMPtr(hwnd);
  CASE msg.ID OF
  | eve.Rise:
    SetPair(hwnd, dv.MainPulldown);
    SetPair(dv.MainPulldown, hwnd);
    p2 := GetAMPtr(dv.MainPulldown);
    WITH p2^ DO
      curr := dlt.WindowsMenu;
      opened := hwnd;
    END;
    IF ActiveWindow # dv.MainPulldown THEN
      eve.AddToTail(ActiveWindow, eve.Redraw,  0);
      eve.AddToTail(dv.MainPulldown, eve.Redraw, 2);
    ELSE
      eve.AddToTail(ActiveWindow, eve.Redraw,  2);
    END;
    Rise(hwnd);
    crt.RiseWindow(hwnd);
    eve.AddToTail(hwnd, eve.Redraw,  0);

  | eve.Hide:
    p2 := GetAMPtr(dv.MainPulldown);
    WITH p2^ DO
      opened := Invalid_H;
    END;
    crt.HideWindow(hwnd);
    ActiveToTail;
    IF ActiveWindow = dv.MainPulldown THEN
      ActiveToTail;
    END;
    eve.AddToTail(dv.MainPulldown, eve.Redraw, 0);
    eve.AddToTail(ActiveWindow, eve.Redraw, 0);

  | eve.Mouse_Released, eve.Mouse_Dbl, eve.Mouse_Moved, eve.Mouse_Pressed:
    size := GetWindowSize(hwnd);
    ASSERT(GetMouse(msg, i, y));
    IF NOT std.CheckFrame(size,i, y) THEN
      ASSERT(GetRelMouse(hwnd, msg, i, y));
      p := GetAMPtr(hwnd);
      WITH p^ DO
        IF frame + y-1 <= N THEN
          curr := frame + (y-1);
          eve.AddToTail(hwnd, eve.Redraw, 0);
          IF msg.ID = eve.Mouse_Released THEN
            eve.AddToTail(hwnd, eve.KbHit, key.Enter);
          END;
        END;
      END;
    END;

  | eve.Defocus:
    eve.AddToHead(hwnd, eve.Hide, 0);

  | eve.Redraw, eve.Paint:
    size := GetWindowSize(hwnd);
    IF ( msg.par # 3) AND (msg.par # 4) THEN
      crt.FillWindow(hwnd, ' ', p^.Colors^[crt.List_Background])
    END;
    WITH size DO len := y2 - y1 - 1; END;

    p2 := GetAMPtr(dv.MainPulldown);
    p2^.curr := dlt.WindowsMenu;
    eve.AddToTail(dv.MainPulldown, eve.Redraw, 2);

    WITH p^ DO
      N := AllVisibleWnds(len);
      IF N > 0 THEN
        CASE msg.par OF
        | 3:
          IF hwnd = ActiveWindow THEN
            write_line(curr);
            crt.Lite(hwnd, curr - frame + 1 , 1, Colors^[crt.List_CurrentLine]);
          END;
        | 4:
          IF hwnd = ActiveWindow THEN
            crt.Lite(hwnd, curr-frame+1 , 1, Colors^[crt.List_Background]);
            write_line(curr);
          END;
        ELSE
          last := std.Min(frame+len-1, N-1);
          FOR i:= frame TO last DO
            write_line(i)
          END;
          IF hwnd = ActiveWindow THEN
            crt.Lite(hwnd, curr - frame + 1, 1, Colors^[crt.List_CurrentLine]);
          END;
        END;
      END;
      crt.DrawFrame(hwnd, size, Frame, Colors^[crt.List_Frame]);
    END;
    IF msg.ID = eve.Redraw THEN crt.UpdateRect(size) END;

  | eve.KbHit:
    WITH p^ DO
      CASE msg.par OF
      | key.Left, key.Right:
        ActiveToTail;
        crt.HideWindow(hwnd);
        eve.AddToTail(dv.MainPulldown, eve.KbHit, msg.par*10000H);
        eve.AddToTail(dv.MainPulldown, eve.KbHit, key.Enter*10000H);

      | key.Enter:
        IF N = 0 THEN RETURN; END;
        eve.AddToTail(hwnd, eve.Hide, 0);
        eve.Flush;
        eve.AddToTail(TakeN(curr), eve.Rise, 0);

      | key.Del:
        IF N = 0 THEN RETURN; END;
        size := GetWindowSize(hwnd);
        crt.HideWindow(hwnd);
        msg2.hwnd := TakeN(curr);
        Rise(msg2.hwnd);
        hndl := GetHandler(msg2.hwnd);
        msg2.ID  := eve.KbHit;
        msg2.par := key.CtrlF4;
        hndl(msg2.hwnd, msg2);
        eve.Flush;
        len := 0;
        N := AllVisibleWnds(len);
        size.y2 := size.y1 + N + 1;
        IF curr = N THEN DEC(curr) END;
        SetWindowSize(hwnd, size);
        eve.AddToTail(hwnd, eve.Rise, 0);
        eve.Flush;
      | key.CtrlTab:
        IF N = 0 THEN RETURN; END;
        msg2.hwnd := TakeN(curr);
        IF std.QueryKey(msg2.hwnd, key.CtrlTab) THEN
          crt.HideWindow(hwnd);
          ActiveToTail;
          IF ActiveWindow = dv.MainPulldown THEN
            eve.AddToTail(dv.MainPulldown, eve.Redraw, 0);
          END;
          eve.AddToTail(TakeN(curr), eve.Rise, 0);
          eve.AddToTail(TakeN(curr), eve.KbHit, key.CtrlTab);
        END;

      | key.Esc:
        msg2.ID   := eve.KbHit;
        msg2.par  := key.CtrlF4;
        msg2.hwnd := hwnd;
        std.DefaultProc(hwnd, msg2);
      ELSE
        std.ListBox (hwnd, msg);
      END;
    END;

  | eve.QueryKbHit:
    IF p^.N = 0 THEN RETURN; END;
    msg2.hwnd := TakeN(p^.curr);
    CASE msg.par OF
    | key.Enter:
      act.ConfirmQuery;
    | key.CtrlTab:
      act.ConfirmQueryByCond(std.QueryKey(msg2.hwnd, key.CtrlTab));
    | key.CtrlW:
      act.ConfirmQueryByCond(std.QueryKey(msg2.hwnd, key.CtrlW));
    | key.Del:
      act.ConfirmQueryByCond(std.QueryKey(msg2.hwnd, key.CtrlF4));
    ELSE
    END;

  | eve.DoAction:
    eve.AddToHead(hwnd, eve.KbHit, msg.par DIV 100H);

  ELSE
    std.ListBox (hwnd, msg);
  END;
END ListWindow;


PROCEDURE FindClosed (hwnd: HWND; hndl: WND_PROC): CARDINAL;
VAR
  i, n: CARDINAL;
BEGIN
  IF AllWindows.max > 0 THEN
    IF hwnd = Invalid_H THEN
      n := 0;
    ELSE
      n := hwnd+1;
    END;
    FOR i := n TO AllWindows.max-1 DO
      IF NOT (visible IN AllWindows.window^[i].state) AND (AllWindows.window^[i].handler = hndl) THEN
        RETURN i
      END;
    END;
  END;
  RETURN Invalid_H;
END FindClosed;


PROCEDURE FindOpened (hwnd: HWND; hndl: WND_PROC): CARDINAL;
VAR
  i, n: CARDINAL;
BEGIN
  IF AllWindows.max > 0 THEN
    IF hwnd = Invalid_H THEN
      n := 0;
    ELSE
      n := hwnd+1;
    END;
    FOR i := n TO AllWindows.max-1 DO
      IF (visible IN AllWindows.window^[i].state) AND (AllWindows.window^[i].handler = hndl) THEN
        RETURN i;
      END;
    END;
  END;
  RETURN Invalid_H;
END FindOpened;


PROCEDURE InitWindowsList (offs: CARDINAL);
VAR
  hwnd  : HWND;
  p     : std.PLIST;
  size  : crt.SZ;
  len, N: CARDINAL;
BEGIN
  IF  AllVisibleWnds(len) = 0 THEN RETURN; END;
  hwnd := dmn.GetMenuHwnd(dlt.WindowsMenu);
  IF hwnd = Invalid_H THEN
    hwnd := RegisterWindow(ListWindow, SIZE(std.LIST));
    ASSERT(hwnd # Invalid_H);
    ExcludeFromList(hwnd);
    SetHeaderByStr(hwnd, '');
    dmn.UpdateMenuHwnd(dlt.WindowsMenu, hwnd);
    p := GetAMPtr(hwnd);
    p^ := std.EmptyList;
    WITH p^ DO
      Colors     := sys.ADR(W_Clr);
      Frame      := crt.Single;
      actions[0] := act.EMPTY_CONTEXT;
    END;
  ELSE
    p := GetAMPtr(hwnd);
  END;
  p^.curr  := 0;
  p^.frame := 0;
  len := 0;
  N := AllVisibleWnds(len);
  WITH size DO
    x1 := offs;
    x2 := offs+len+2;
    IF x2 >= crt.Xmax THEN x2 := crt.Xmax-1; END;
    y1 := 1;
    y2 := N+2;
    IF y2 >= crt.Ymax THEN y2 := crt.Ymax-1; END;
  END;
  SetWindowSize(hwnd, size);
  eve.AddToTail(hwnd, eve.Rise, 0);
END InitWindowsList;

BEGIN
  ActiveWindow := Invalid_H;
  Tail := Invalid_H;

  W_Clr[crt.List_Background]   := crt.Attr(crt.Black,     crt.Blue);
  W_Clr[crt.List_Frame]        := crt.Attr(crt.DarkGray,  crt.Blue);
  W_Clr[crt.List_Header]       := crt.Attr(crt.Black,     crt.LightGray);
  W_Clr[crt.List_ActiveHeader] := crt.Attr(crt.White,     crt.Black);
  W_Clr[crt.List_Line]         := crt.Attr(crt.LightGray, crt.Blue);
  W_Clr[crt.List_CurrentLine]  := crt.Attr(crt.White,     crt.Black);

  AllWindows := EmptyWindowsList;
END Dlg_Win.

