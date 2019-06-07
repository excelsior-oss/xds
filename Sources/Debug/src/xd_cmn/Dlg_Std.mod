<* Storage+ *>
IMPLEMENTATION MODULE Dlg_Std;

IMPORT sys := SYSTEM;
IMPORT fmt := FormStr;

IMPORT key := Keys;
IMPORT crt := CRT;
IMPORT win := Dlg_Win;
IMPORT eve := DlgEvent;
IMPORT act := Dlg_Acts;
IMPORT puw := DlgPopup;
IMPORT dv  := Dlg_Vars;
IMPORT dlt := DlgTypes;

IMPORT xs  := xStr;
IMPORT pro := Protocol;
IMPORT mes := MsgNo;
IMPORT opt := Options;

IMPORT lst := Lists;

<* IF DEST_XDS AND (TARGET_OS = "WINNT") THEN *>
IMPORT cbm := CB_Manag;
<* END *>


PROCEDURE Min(a,b: CARDINAL): CARDINAL;
BEGIN
  IF a > b THEN RETURN b ELSE RETURN a END;
END Min;


VAR
  WarningHwnd: crt.HWND;
  InformationHwnd: crt.HWND;


PROCEDURE Notify(str-: ARRAY OF CHAR);
BEGIN
  SetWarningMsg(str);
  eve.AddToTail(WarningHwnd, eve.Rise, 0);
  eve.Flush;
END Notify;

PROCEDURE Error(str-: ARRAY OF CHAR);
BEGIN
  SetErrorMsg(str);
  eve.AddToTail(ErrorMsg, eve.Rise, 0);
  eve.Flush;
END Error;

PROCEDURE NotifyNo(no: CARDINAL; SEQ arg: sys.BYTE);
VAR
  str, buf: xs.String;
BEGIN
  pro.GetMsg(no, str);
  fmt.print(buf, str, arg);
  Notify(buf);
END NotifyNo;

PROCEDURE ErrorNo(no: CARDINAL; SEQ arg: sys.BYTE);
VAR
  str, buf: xs.String;
BEGIN
  pro.GetMsg(no, str);
  fmt.print(buf, str, arg);
  Error(buf);
END ErrorNo;


PROCEDURE ErrorArg (str: ARRAY OF CHAR; SEQ arg: sys.BYTE);
VAR
  tmp: xs.String;
BEGIN
  fmt.print (tmp, str, arg);
  Error (tmp);
END ErrorArg;


PROCEDURE NotifyArg (str: ARRAY OF CHAR; SEQ arg: sys.BYTE);
VAR
  tmp: xs.String;
BEGIN
  fmt.print (tmp, str, arg);
  Notify (tmp);
END NotifyArg;


PROCEDURE SetErrorNo(no: CARDINAL);
VAR
  str: xs.String;
BEGIN
  pro.GetMsg(no, str);
  SetErrorMsg(str);
END SetErrorNo;


PROCEDURE MsgProc (hwnd: win.HWND; Msg: eve.MSG);
VAR
  p      : PPMSG;
  i      : CARDINAL;
  size   : crt.SZ;
  y,x: CARDINAL;
BEGIN
  CASE Msg.ID OF
  | eve.Mouse_Pressed, eve.Mouse_Moved:
    IF eve.InFlush THEN
      eve.WaitForEvent;
    END;

  | eve.Mouse_Released, eve.Mouse_Dbl:
    ASSERT(win.GetRelMouse(hwnd, Msg, x, y));
    size := win.GetWindowSize(hwnd);
    WITH size DO
      IF (x >= (x2-x1-8) DIV 2) & (x <= (x2-x1-8) DIV 2 + 8) & (y = (y2-y1-2)) THEN
        eve.AddToTail (hwnd, eve.Hide, 0);
      ELSE
        IF eve.InFlush THEN
          eve.WaitForEvent;
        END;
      END;
    END;

  | eve.Rise:
    IF win.ActiveWindow # hwnd THEN
      eve.AddToTail(win.ActiveWindow, eve.Redraw, 0);
    END;
    eve.ModalRedraw (hwnd);
    win.Rise(hwnd);
    crt.RiseWindow(hwnd);

  | eve.Redraw, eve.Paint:
    p := PPMSG(win.GetAMPtr(hwnd));
    WITH p^ DO
      crt.FillWindow(hwnd, ' ', Colors^[crt.Msg_Frame]);
      FOR i := 0 TO HIGH(p^.Msg^) DO
        WITH p^.Msg^[i] DO
          crt.SetPos(x,y);
          crt.WrStr(hwnd, message, Colors^[crt.Msg_Message]);
        END;
      END;
      size := win.GetWindowSize(hwnd);
      WITH size DO
        crt.SetPos((x2-x1-8) DIV 2, y2-y1-2);
      END;
      crt.WrStr(hwnd,'[  OK  ]', Colors^[crt.Msg_Button]);
      crt.DrawFrame (hwnd, size, crt.Double, Colors^[crt.Msg_Frame]);
      crt.DrawHeader (hwnd, size, Colors^[crt.Msg_Header]);
    END;
    IF Msg.ID = eve.Redraw THEN crt.UpdateRect(size); END;
    eve.CheckWaitForEvent (hwnd, Msg.par);

  | eve.KbHit :
    CASE Msg.par OF
    | key.Enter, key.Esc:
      eve.AddToTail (hwnd, eve.Hide, 0);
    ELSE
      IF eve.InFlush THEN
        eve.WaitForEvent;
      END;
    END;

  ELSE
    DefaultProc(hwnd,Msg);
  END;
END MsgProc;



PROCEDURE SetMessage (HWND: win.HWND; str-: ARRAY OF CHAR);
TYPE
  CHARSET = SET OF CHAR;
CONST
  BreakLineSymbols = CHARSET{ CHR(0DH), CHR(0AH), 0C };

VAR
  size   : crt.SZ;
  p      : PPMSG;
  str_buf: xs.String;
  str_len,
  str_pos,
  msg_no ,
  msg_pos,
  max_msg: CARDINAL;

BEGIN
  p := PPMSG(win.GetAMPtr(HWND));
  size := win.GetWindowSize(HWND);
  fmt.print(str_buf, str);
  str_len := LENGTH(str_buf);
  str_pos := 0;
  msg_no  := 0;
  max_msg := 0;
  REPEAT
    msg_pos := 0;
    WITH p^.Msg^[msg_no] DO
      WHILE (msg_pos<HIGH(message)) AND NOT (str_buf[str_pos] IN BreakLineSymbols) DO
        message[msg_pos] := str_buf[str_pos];
        INC(str_pos);
        INC(msg_pos);
      END;
      message[msg_pos] := 0C;
    END;
    IF max_msg < msg_pos THEN
      max_msg := msg_pos;
    END;
    IF (str_pos < str_len) AND (str_buf[str_pos] IN BreakLineSymbols) THEN
      INC(str_pos);
    END;
    INC(msg_no);
  UNTIL (str_pos = str_len) OR (msg_no > HIGH(p^.Msg^));
  WITH size DO
    IF max_msg > crt.Xmax-6 THEN
      x1 := 1;
      x2 := crt.Xmax-2;
    ELSIF max_msg >= 30 THEN
      x1 := (crt.Xmax-6-max_msg) DIV 2 + 1;
      x2 := x1 + max_msg + 3;
    ELSE
      x1 := crt.Xmax DIV 2 - 16;
      x2 := crt.Xmax DIV 2 + 16;
    END;
    y2 := y1 + 5 + msg_no;
    WHILE msg_no <= HIGH(p^.Msg^) DO
      p^.Msg^[msg_no].message[0] := 0C;
      INC(msg_no);
    END;
  END;
  win.SetWindowSize(HWND, size);
END SetMessage;


PROCEDURE InitMessage (HWND: win.HWND; messages: CARDINAL);
VAR
  p: PPMSG;
  i: CARDINAL;
BEGIN
  p := PPMSG (win.GetAMPtr (HWND));
  NEW (p^.Msg, messages);
  sys.FILL (sys.ADR(p^.Msg^), 0, SIZE(p^.Msg^));
  FOR i := 0 TO HIGH (p^.Msg^) DO
    WITH p^.Msg^[i] DO
      x := 2;
      y := i+2;
      COPY ("", message);
    END;
  END;
END InitMessage;


PROCEDURE InitMsgWindow (VAR hwnd: crt.HWND; VAR colors: crt.MSG_ATTR; header_no: CARDINAL);
VAR
  p: PPMSG;
BEGIN
  IF hwnd = win.Invalid_H THEN
    hwnd := win.RegisterWindow (MsgProc, SIZE(MSG_REC));
    ASSERT (hwnd # win.Invalid_H);
    win.SetWindowSize (hwnd, crt.SZ {20, 8, crt.Xmax-20, 16});
    win.SetMovable (hwnd);
    win.SetModal (hwnd);
    p := win.GetAMPtr (hwnd);
    p^.Colors := sys.ADR (colors);
    InitMessage (hwnd, 8);
    SetMessage (hwnd, "IF THIS MESSAGE DISPLAYED\nSOME INTERNAL DEBUGGER ERROR\nWAS DETECTED");
    win.SetHeader (hwnd, header_no);
  END;
END InitMsgWindow;


PROCEDURE SetCenterMessage (hwnd: crt.HWND; str-: ARRAY OF CHAR);
VAR
  p: PPMSG;
BEGIN
  SetMessage (hwnd, str);
  CenterWindow (hwnd);
  p := win.GetAMPtr (hwnd);
  CenterMessage (win.GetWindowSize (hwnd), p^.Msg);
END SetCenterMessage;


PROCEDURE InitErrorMsg;
BEGIN
  InitMsgWindow (ErrorMsg, crt.Error, mes.Error_header);
END InitErrorMsg;


PROCEDURE SetErrorMsg (str-: ARRAY OF CHAR; SEQ arg: sys.BYTE);
VAR
  str2: xs.String;
BEGIN
  InitErrorMsg ();
  fmt.print (str2, str, arg);
  SetCenterMessage (ErrorMsg, str2);
END SetErrorMsg;

PROCEDURE SetErrorMsgNo(no: CARDINAL; SEQ arg: sys.BYTE);
VAR
  msg: xs.String;
BEGIN
  InitMsgWindow (ErrorMsg, crt.Error, mes.Error_header);
  pro.GetMsg (no, msg);
  SetErrorMsg (msg, arg)
END SetErrorMsgNo;


PROCEDURE SetWarningMsg(str-: ARRAY OF CHAR);
BEGIN
  InitMsgWindow (WarningHwnd, crt.Msg, mes.Warning_header);
  SetCenterMessage (WarningHwnd, str);
END SetWarningMsg;


PROCEDURE Information (str: ARRAY OF CHAR);
BEGIN
  InitMsgWindow (InformationHwnd, crt.Info, mes.Information_header);
  SetCenterMessage (InformationHwnd, str);
  eve.AddToTail(InformationHwnd, eve.Rise, 0);
  eve.Flush;
END Information;


PROCEDURE InformationArg (str: ARRAY OF CHAR; SEQ arg: sys.BYTE);
VAR
  tmp: xs.String;
BEGIN
  fmt.print (tmp, str, arg);
  Information (tmp);
END InformationArg;


PROCEDURE InformationNo (no: CARDINAL; SEQ arg: sys.BYTE);
VAR
  str, buf: xs.String;
BEGIN
  pro.GetMsg (no, str);
  fmt.print (buf, str, arg);
  Information (buf);
END InformationNo;



--------------------------------------------------------------------------------

VAR
  OkCancelHwnd: win.HWND;

TYPE
  PCARDINAL = POINTER TO CARDINAL;

PROCEDURE OkCancelProc (hwnd: win.HWND; Msg: eve.MSG);
VAR
  p   : PPMSG;
  i   : CARDINAL;
  size: crt.SZ;
  pc  : PCARDINAL;
  attr: crt.ATTR;
BEGIN
  p := PPMSG(win.GetAMPtr(hwnd));
  pc := sys.ADDADR(p, SIZE(MSG_REC));
  CASE Msg.ID OF
  | eve.Rise:
    IF win.ActiveWindow # hwnd THEN
      eve.AddToTail(win.ActiveWindow, eve.Redraw, 0);
    END;
    eve.ModalRedraw (hwnd);
    win.Rise(hwnd);
    crt.RiseWindow(hwnd);
    pc^ := MAX(CARDINAL);
    
  | eve.Redraw, eve.Paint:
    WITH p^ DO
      crt.FillWindow(hwnd, ' ', Colors^[crt.Msg_Frame]);
      crt.DrawFrame(hwnd, size, crt.Double, Colors^[crt.Msg_Frame]);
      FOR i := 0 TO HIGH(p^.Msg^) DO
        WITH p^.Msg^[i] DO
          crt.SetPos(x,y);
          crt.WrStr(hwnd, message, Colors^[crt.Msg_Message]);
        END;
      END;
      size := win.GetWindowSize(hwnd);
      WITH size DO
        IF pc^ = MAX(CARDINAL) THEN
          attr := Colors^[crt.Msg_Button];
        ELSE
          attr := Colors^[crt.Msg_Frame];
        END;
        crt.SetPos((x2-x1-22) DIV 2, y2-y1-2);
        crt.WrStr(hwnd,'[  OK  ]', attr);
        IF pc^ # MAX(CARDINAL) THEN
          attr := Colors^[crt.Msg_Button];
        ELSE
          attr := Colors^[crt.Msg_Frame];
        END;
        crt.SetPos((x2-x1-22) DIV 2 + 12, y2-y1-2);
        crt.WrStr(hwnd,'[ Cancel ]', attr);
      END;
    END;
    IF Msg.ID = eve.Redraw THEN crt.UpdateRect(size); END;
    eve.CheckWaitForEvent (hwnd, Msg.par);

  | eve.KbHit :
    p := PPMSG(win.GetAMPtr(hwnd));
    pc := sys.ADDADR(p, SIZE(MSG_REC));
    CASE Msg.par OF
    | key.Enter:
      pc^ := MAX(CARDINAL);
      win.ActiveToTail;
      crt.HideWindow(hwnd);
    | key.Esc:
      pc^ := 0;
      win.ActiveToTail;
      crt.HideWindow(hwnd);
    | key.Tab:
      IF pc^ = MAX(CARDINAL) THEN pc^ := 0; ELSE pc^ := MAX(CARDINAL); END;
      eve.AddToTail(hwnd, eve.Redraw, 0);
    ELSE
      eve.AddToTail(hwnd, eve.Redraw, 0);
    END;

  ELSE
    DefaultProc(hwnd,Msg);
  END;
END OkCancelProc;


PROCEDURE InitOkCancel;
VAR
  size: crt.SZ;
  p   : PPMSG;
  msg : PMSG;
BEGIN
  IF OkCancelHwnd = win.Invalid_H THEN
    NEW(msg, 3);
    msg^[0] := MSG{ 2,2, 'Message Line 1'};
    msg^[1] := MSG{ 2,3, 'Message Line 2'};
    msg^[2] := MSG{ 2,4, 'Message Line 3'};
    OkCancelHwnd := win.RegisterWindow(OkCancelProc, SIZE(MSG_REC)+SIZE(CARDINAL));
    ASSERT(OkCancelHwnd # win.Invalid_H);
    WITH size DO x1 := 17; y1 := 8; x2 := 41; y2 := y1+8; END;
    win.SetWindowSize(OkCancelHwnd, size);
    win.SetModal(OkCancelHwnd);
    p := win.GetAMPtr(OkCancelHwnd);
    p^.Colors := sys.ADR(crt.Msg);
    p^.Msg := msg;
  END;
END InitOkCancel;


PROCEDURE OkCancel (message-: ARRAY OF CHAR): BOOLEAN;
VAR
  pc: PCARDINAL;
BEGIN
  InitOkCancel;
  SetMessage (OkCancelHwnd, message);
  CenterWindow (OkCancelHwnd);
  eve.AddToTail(OkCancelHwnd, eve.Rise, 0);
  eve.Flush;
  pc := sys.ADDADR(win.GetAMPtr(OkCancelHwnd), SIZE(MSG_REC));
  RETURN pc^ = MAX(CARDINAL);
END OkCancel;  

--------------------------------------------------------------------------------


PROCEDURE CheckFrame(size: crt.SZ; x,y: CARDINAL): BOOLEAN;
BEGIN
  WITH size DO
    RETURN (x1 = x) OR (x2 = x) OR (y1 = y) OR (y2 = y);
  END;
END CheckFrame;

VAR
   InputDialog  : win.HWND;

PROCEDURE Shift(k, N, len: CARDINAL; VAR curr, frame: CARDINAL): BOOLEAN;
VAR
  pos: CARDINAL;
  tmp: CARDINAL;
BEGIN
  IF N = 0 THEN RETURN TRUE; END;
  tmp := frame;
  DEC(N);
  CASE k OF
  | key.Down :
    IF curr < N THEN INC(curr); END;
    IF (frame + len <= N) AND (curr - frame > len - 1) THEN INC(frame); END;
  | key.Up :
    IF (frame > 0) OR (curr > 0) THEN
      DEC(curr);
      IF curr < frame THEN DEC(frame); END;
    END;
  | key.PgUp :
    pos := curr - frame;
    IF frame = 0 THEN
      curr := 0
    ELSE
      IF frame < len THEN
        frame := 0;
      ELSE
        DEC(frame,len)
      END;
      curr := frame + pos;
    END;
  | key.PgDn:
    pos := curr - frame;
    IF (N < len) OR (frame = N - len + 1) THEN
      curr := N;
    ELSE
      INC(frame,len);
      IF frame > N - len + 1 THEN
        frame := N - len + 1;
      END;
      curr := frame + pos;
    END;
  | key.CtrlHome : (* В начало страницы *)
    curr := frame;
  | key.CtrlPgUp : (* В начало текста *)
    curr  := 0;
    frame := 0;
  | key.CtrlEnd :
    curr := frame+len-1;
    IF curr > N THEN curr := N; END;
  | key.CtrlPgDn :
    curr := N;
    IF N < len THEN
      frame := 0
    ELSE
      frame := N - len + 1;
    END;
  ELSE
  END;
  RETURN frame = tmp;
END Shift;

PROCEDURE Normalize(size: crt.SZ; VAR curr, frame: CARDINAL; N: CARDINAL);
VAR
  len  : CARDINAL;
BEGIN
  IF curr > N THEN
    curr := N;
  END;
  IF frame > N THEN
    frame := N;
  END;
  IF N > 1 THEN
    DEC(N);
    len  := size.y2 - size.y1 - 1;
    IF curr > frame + len - 1 THEN
      frame := curr - len + 1;
    ELSIF curr < frame THEN
      frame := curr;
    ELSIF curr = N THEN
      IF N < len THEN
        frame := 0;
      ELSE
        frame := curr - len + 1;
      END;
    END;
  END;
END Normalize;

VAR
  _msg  : eve.MSG;

PROCEDURE Move_Resize_internal (window: win.HWND;
                                minX, minY: CARDINAL;
                                mouse: BOOLEAN;
                                mpX, mpY: CARDINAL);
VAR
  rect, saved_size, current_size, prev_size: crt.SZ;
  Xlen  : CARDINAL;
  Ylen  : CARDINAL;
  dX, dY: CARDINAL;
  pos   : CARDINAL;
  hndl: win.WND_PROC;
  ResizableX: BOOLEAN;
  ResizableY: BOOLEAN;
  WasResized: BOOLEAN;
  resize: BOOLEAN;
  left  : BOOLEAN;
  right : BOOLEAN;
  top   : BOOLEAN;
  bottom: BOOLEAN;

  is_move  : BOOLEAN;
  is_resize: BOOLEAN;


  MODULE FrameMove;
  IMPORT eve, win, window;
  BEGIN
  FINALLY
    eve.AddToTail(window, eve.Redraw, 1);
    win.SetPair(window, win.Invalid_H);
  END FrameMove;

BEGIN
  eve.Flush;
  IF crt.IsCursorVisible() THEN crt.CursorOff; END;

  IF minX < 5 THEN minX := 5; END;
  IF minY < 3 THEN minY := 3; END;

  IF NOT win.IsMovable(window) THEN RETURN; END;
  ResizableX := win.IsResizableX(window);
  ResizableY := win.IsResizableY(window);
  hndl := win.GetHandler(window);

  saved_size := win.GetWindowSize(window);
  crt.DrawFrame(window, saved_size, crt.Move, crt.Attr(crt.White, crt.Black));
  crt.UpdateRect(saved_size);
  crt.Save(window);
  WasResized := FALSE;

  current_size := saved_size;
  prev_size    := current_size;
  IF mouse THEN
    win.SetPair(window, eve.AllWindows);
  END;

  left   := FALSE;
  right  := FALSE;
  top    := FALSE;
  bottom := FALSE;
  resize := FALSE;

  WITH current_size DO
    Xlen := x2 - x1;
    Ylen := y2 - y1;
    IF mouse THEN
      dX := mpX - x1;
      dY := mpY - y1;
      left := x1 = mpX;
      right := x2 = mpX;
      top := (y1 = mpY) AND (left OR right);
      bottom := y2 = mpY;
      ResizableX := ResizableX AND (left OR right);
      ResizableY := ResizableY AND (top OR bottom);
      resize := ResizableX OR ResizableY;
    ELSE
      dX := 0;
      dY := 0;
    END;
  END;

  LOOP
    eve.WaitForEvent;
    LOOP
      is_move   := FALSE;
      is_resize := FALSE;
      _msg := eve.GetEvent();
      prev_size := current_size;
      CASE _msg.ID OF
      | eve.None: EXIT;
      | eve.DoAction:
      | eve.KbHit:
        IF NOT mouse THEN
          WITH current_size DO
            CASE _msg.par OF
            | key.CtrlW, key.Enter :
              crt.RestoreMapFromMove;
              crt.RiseWindow(window);
              crt.Update;
              RETURN;
            | key.Left:
              IF (x1 > 0) THEN
                DEC(x1);
                DEC(x2);
                is_move := TRUE;
              END;
            | key.Right:
              IF (x2 < crt.Xmax-1) THEN
                INC(x1);
                INC(x2);
                is_move := TRUE;
              END;
            | key.Up:
              IF (y1 > 1) THEN
                DEC(y1);
                DEC(y2);
                is_move := TRUE;
              END;
            | key.Down:
              IF (y2 < crt.Ymax-1) THEN
                INC(y1);
                INC(y2);
                is_move := TRUE;
              END;
            | key.CtrlLeft:
              IF ResizableX AND (x2-x1 >= minX) THEN
                DEC(x2);
                is_resize := TRUE;
              END;
            | key.CtrlRight:
              IF ResizableX AND (x2 < crt.Xmax-1) THEN
                INC(x2);
                is_resize := TRUE;
              END;
            | key.CtrlUp:
              IF ResizableY AND (y2-y1 >= minY) THEN
                DEC(y2);
                is_resize := TRUE;
              END;
            | key.CtrlDown:
              IF ResizableY AND (y2 < crt.Ymax-1) THEN
                INC(y2);
                is_resize := TRUE;
              END;
            ELSE
            END;
          END;
        END;
      ELSE
        IF (_msg.ID IN eve.MouseEvents) & mouse THEN
          CASE _msg.ID OF
          | eve.Mouse_Moved:
            WITH current_size DO
              IF resize THEN
                IF ResizableX THEN
                  IF left THEN
                    pos := _msg.par DIV 10000H;
                    IF pos+minX-1 <= x2 THEN
                      x1 := pos;
                    ELSIF x2+1 >= minX THEN
                      x1 := x2+1-minX;
                    END;
                  END;
                  IF right THEN
                    pos := _msg.par DIV 10000H;
                    IF x1+minX-1 <= pos THEN
                      x2 := pos;
                    ELSE
                      x2 := x1+minX-1;
                    END;
                  END;
                END;
                IF ResizableY THEN
                  IF top THEN
                    pos := _msg.par MOD 10000H;
                    IF pos # 0 THEN
                      IF pos+minY-1 <= y2 THEN
                        y1 := pos;
                      ELSIF y2+1 >= minY THEN
                        y1 := y2+1-minY;
                      END;
                    END;
                  END;
                  IF bottom THEN
                    pos := _msg.par MOD 10000H;
                    IF y1+minY-1 <= pos THEN
                      y2 := pos;
                    ELSE
                      y2 := y1+minY-1;
                    END;
                  END;
                END;
                is_resize := (prev_size.x1 # x1) OR (prev_size.x2 # x2)
                          OR (prev_size.y1 # y1) OR (prev_size.y2 # y2);
              ELSE
                y1 := _msg.par MOD 10000H;
                IF y1 < dY THEN
                  y1 := 0;
                ELSE
                  DEC(y1, dY);
                END;
                y2 := y1 + Ylen;
                IF y2 > crt.Ymax-1 THEN
                  y2 := crt.Ymax-1;
                  y1 := y2 - Ylen;
                ELSIF y1 = 0 THEN
                  INC(y1);
                  INC(y2);
                END;
                x1 := _msg.par DIV 10000H;
                IF x1 < dX THEN
                  x1 := 0;
                ELSE
                  DEC(x1, dX);
                END;
                x2 := x1 + Xlen;
                IF x2 > crt.Xmax-1 THEN
                  x2 := crt.Xmax-1;
                  x1 := x2 - Xlen;
                END;
                is_move := (prev_size.x1 # x1) OR (prev_size.x2 # x2)
                        OR (prev_size.y1 # y1) OR (prev_size.y2 # y2);
              END;
            END;
          ELSE
            crt.RestoreMapFromMove;
            crt.RiseWindow(window);
            crt.Update;
            RETURN;
          END;
        ELSE
        END;
      END;

      IF is_move THEN
        IF WasResized THEN
          WasResized := FALSE;
          crt.Save(window);
          saved_size := prev_size;
        END;
        ASSERT(window < eve.AllWindows);
        win.SetWindowSize(window, current_size);
        crt.RestoreStatic(prev_size);
        crt.RestoreForMove(current_size, saved_size);
        rect := crt.Union(prev_size, current_size);
        crt.UpdateRect(rect);
      ELSIF is_resize THEN
        WasResized := TRUE;
        crt.RestoreStatic(prev_size);
        win.SetWindowSize(window, current_size);
        crt.RiseWindow(window);
        hndl(window, eve.MSG{window, eve.Paint, 1});
        crt.DrawFrame(window, current_size, crt.Move, crt.Attr(crt.White, crt.Black));
        rect := crt.Union(prev_size, current_size);
        crt.UpdateRect(rect);
      END;
    END;
  END;
END Move_Resize_internal;


PROCEDURE Move_Resize_kbd (hwnd: win.HWND; minX, minY: CARDINAL);
BEGIN
  Move_Resize_internal(hwnd, minX, minY, FALSE, 0, 0);
END Move_Resize_kbd;

PROCEDURE Move_Resize_mou (hwnd: crt.HWND; minX, minY: CARDINAL; mpX, mpY: CARDINAL);
BEGIN
  Move_Resize_internal(hwnd, minX, minY, TRUE, mpX, mpY);
END Move_Resize_mou;




CONST
  PatternSearchRedraw = 10; (* Код Redraw для обработчика ListBox в режиме   *)
                            (* локатора, см. PatternSearch - шаблон локатора *)

VAR
  PatternSearch: xs.String; (* Строка для поиcка: "локатор", по шаблону *)



PROCEDURE ListBoxLocator (hwnd: win.HWND; msg: eve.MSG);
VAR
  p      : PLIST;
  size   : crt.SZ;
  header : ARRAY [0..32] OF CHAR;
  lheader: CARDINAL;
  pheader: CARDINAL;
  b, l, i: CARDINAL;
  name   : xs.String;
  tmp    : xs.String;
  up     : xs.String;
  ch     : CHAR;
  stop   : BOOLEAN;
  add    : BOOLEAN;
BEGIN
  p := win.GetAMPtr(hwnd);
  size := win.GetWindowSize(hwnd);
  CASE msg.ID OF
  | eve.Paint, eve.Redraw:
    IF msg.par # PatternSearchRedraw THEN
      PatternSearch := '';
    END;
    IF win.ActiveWindow = hwnd THEN
      win.GetHeader(hwnd, header);
      WITH size DO
        (* Отрисовка локатора *)
        lheader := x2-x1-1;
        IF lheader > 1 THEN
          DEC(lheader);
        END;
        pheader := LENGTH(PatternSearch);
        IF pheader < lheader THEN
          xs.Extract (PatternSearch, 0, lheader, tmp);
        ELSE
          xs.Extract (PatternSearch, pheader-lheader, lheader, tmp);
          IF LENGTH(tmp) > 2 THEN
            tmp [0] := '.';
            tmp [1] := '.';
          END;
        END;
        crt.SetPos (2, y2-y1);
        crt.WrStr (hwnd, tmp, p^.Colors^[crt.List_ActiveHeader]);
      END;
    END;
  | eve.KbHit:
    CASE msg.par OF
    | key.Esc:
      eve.AddToTail(hwnd, eve.Redraw,  0);
      RETURN;
    | key.Tab:
      WITH p^ DO
        IF (locator # NIL) AND (N > 0) THEN
          b := LENGTH(PatternSearch);
          IF PatternSearch = "" THEN
            locator (hwnd, curr, PatternSearch);  -- текущий элемент списка
          ELSE
            LOOP
              l := LENGTH(PatternSearch); -- длина шаблона
              COPY (PatternSearch, up);
              xs.Uppercase (up);
              locator (hwnd, curr, tmp);  -- текущий элемент списка
              ch := tmp[l]; -- очередной символ из текущего элемента списка
              IF ch = 0C THEN
                EXIT; -- т.е. шаблон полностью совпадает с текущим элементом списка
              END;
              -- просматриваем все от текущего элемента вниз по списку
              -- пытаемся найти элемент, у которого в позиции l не такой же символ
              stop := FALSE;
              i := curr;
              LOOP
                IF i = N-1 THEN
                  add := TRUE; -- просмотрели все, теперь в позицию l можно добавить символ
                  EXIT;
                END;
                INC(i);
                locator (hwnd, i, tmp);
                xs.Extract (tmp, 0, l, name);
                xs.Uppercase (name);
                IF name = up THEN
                  stop := CAP(ch) # CAP(tmp[l]);
                  IF stop THEN
                    -- оказалось, что очередные символы в позиции l различны
                    -- поэтому новый символ в шаблон добавлять нельзя
                    add := FALSE;
                    EXIT;
                  END;
                END;
              END;
              -- при просмотре всех элементов списка не нашлось ни одного такого,
              -- у которого бы в позиции l находился бы отличный от шаблона символ
              IF add THEN
                PatternSearch[l] := ch;
                PatternSearch[l+1] := 0C;
              END;
              IF stop THEN
                EXIT;
              END;
            END;
          END;
          IF b # LENGTH(PatternSearch) THEN
            eve.AddToTail(hwnd, eve.Redraw, PatternSearchRedraw);
          END;
          RETURN;
        END;
      END;
    | key.CtrlDown:
      WITH p^ DO
        IF (locator # NIL) AND (N > 0) THEN
          locator (hwnd, curr, tmp);
          l := LENGTH(PatternSearch);
          i := l;
          IF curr < N-1 THEN
            -- найти совпадающие части со следующим
            locator (hwnd, curr+1, name);
            i := 0;
            LOOP
              ch := CAP(name[i]);
              IF (ch = 0C) OR (ch # CAP(tmp[i])) THEN EXIT; END;
              INC(i);
            END;
          END;
          IF curr < N-1 THEN
            IF PatternSearch = "" THEN
              IF i = 0 THEN
                COPY(tmp, PatternSearch);
              ELSE
                xs.Extract (tmp, 0, i, PatternSearch);
              END;
            ELSIF l < i THEN
              xs.Extract (tmp, 0, i, PatternSearch);
            ELSIF l > i THEN
              crt.Beep;
            ELSE
              INC(curr);
              Normalize(size, curr, frame, N);
            END;
          ELSE
            IF tmp = PatternSearch THEN
              crt.Beep;
            ELSE
              COPY(tmp, PatternSearch);
            END;
          END;
        END;
      END;
      eve.AddToTail(hwnd, eve.Redraw, PatternSearchRedraw);
      RETURN;
    ELSE
      WITH p^ DO
        IF (locator # NIL) AND (N > 0) THEN
         <* PUSH *>
         <* WOFF312+ *>
          l := LENGTH(PatternSearch);
          COPY(PatternSearch, tmp);
          LOOP
         <* POP *>
            IF PatternSearch = '' THEN
              b := 0;
            ELSE
              b := curr;
            END;
            IF msg.par = key.BackSpace THEN
              IF l > 0 THEN
                DEC(l);
                b := 0;
              END;
            ELSIF (msg.par < 255) AND (l < HIGH(PatternSearch)) THEN
              ch := xs.UpChar(CHR(msg.par));
              IF ch IN xs.CHARSET{'A'..'Z', '0'..'9', '_', '.', '$', '[', ']', ':', '/', '&', '@', '`'} THEN
                PatternSearch[l] := ch;
                INC(l);
              ELSE
                EXIT;
              END;
            ELSE
              EXIT;
            END;
            PatternSearch[l] := 0C;
            xs.Uppercase(PatternSearch);
            FOR i := b TO N-1 DO
              locator (hwnd, i, name);
              xs.Extract(name, 0, l, up);
              xs.Uppercase(up);
              IF (up = PatternSearch) THEN
                xs.Extract(name, 0, l, PatternSearch);
                curr := i;
                Normalize(size, curr, frame, N);
                eve.AddToTail (hwnd, eve.Redraw, PatternSearchRedraw);
--                IF msg.par # key.BackSpace THEN
                  -- продолжить шаблон на сколько возможно
--                  eve.AddToTail (hwnd, eve.KbHit, key.Tab);
--                END;
                RETURN;
              END;
            END;
            EXIT;
          END;
          COPY(tmp, PatternSearch);
        END;
      END;
    END;
    DefaultProc(hwnd, msg);
  ELSE
    COPY("", PatternSearch);
    DefaultProc(hwnd, msg);
  END;
END ListBoxLocator;


PROCEDURE ListBox (hwnd: win.HWND; msg: eve.MSG);
VAR
  size   : crt.SZ;
  len    : CARDINAL;
  p      : PLIST;
  x, y   : CARDINAL;
  attr   : crt.ATTR;

BEGIN
  CASE msg.ID OF
  | eve.Mouse_Pressed, eve.Mouse_Dbl, eve.Mouse_Moved:
    ASSERT(win.GetRelMouse(hwnd, msg, x, y));
    size := win.GetWindowSize(hwnd);
    CASE crt.GetControl (size, x, y, dlt.WND_CTRL_SET{dlt.WinCtrl_Close}) OF
    | dlt.WinCtrl_Close:
      IF msg.ID = eve.Mouse_Pressed THEN
        eve.AddToTail(hwnd, eve.Hide, 0);
      END;
    ELSE
      p := win.GetAMPtr(hwnd);
      IF NOT CheckFrame(size, x+size.x1, y+size.y1) THEN
        WITH p^ DO
          IF frame + y <= N THEN
            curr := frame + (y-1);
            eve.AddToTail(hwnd, eve.Redraw, 0);
            IF msg.ID = eve.Mouse_Dbl THEN
              eve.AddToTail(hwnd, eve.KbHit, key.Enter);
            ELSIF msg.ID = eve.Mouse_Moved THEN
              IF (frame # 0) AND (curr = frame) THEN
                eve.AddToTail(hwnd, eve.KbHit, key.Up);
              ELSIF (curr+1 # N) AND (curr+1 = frame+size.y2-size.y1-1) THEN
                eve.AddToTail(hwnd, eve.KbHit, key.Down);
              END;
            END;
          END;
        END;
      ELSE
        DefaultProc (hwnd, msg);
        size := win.GetWindowSize(hwnd);
        WITH p^ DO
          Normalize (size, curr, frame, N);
        END;
      END;
    END;

  | eve.R_Mouse:
    ASSERT(win.GetMouse(msg, x, y));
    size := win.GetWindowSize(hwnd);
    IF NOT CheckFrame(size, x, y) THEN
      eve.AddToTail(hwnd, eve.Mouse_Pressed, msg.par);
      eve.Flush;
      p := win.GetAMPtr(hwnd);
      puw.PopupWindow(x, y, hwnd, p^.actions);
    END;

  | eve.DoAction:
    IF VAL(act.ACTION, msg.par MOD 100H) = act.ContextMenu THEN
      p := win.GetAMPtr(hwnd);
      size := win.GetWindowSize(hwnd);
      WITH size DO
        puw.PopupWindow(x1+(x2-x1) DIV 2, y1+(y2-y1) DIV 2, hwnd, p^.actions);
      END;  
    ELSE
      DefaultProc(hwnd, msg);
    END;  

  | eve.QueryAction:
    IF VAL(act.ACTION, msg.par MOD 100H) = act.ContextMenu THEN
      act.ConfirmQuery;
    ELSE
      DefaultProc(hwnd, msg);
    END;

  | eve.Paint, eve.Redraw:
    p := win.GetAMPtr(hwnd);
    size := win.GetWindowSize(hwnd);
    WITH p^ DO
      attr := Colors^[crt.List_Frame];
      IF Frame = crt.Single THEN
        crt.DrawFrame(hwnd, size, Frame, attr);
      ELSE
        IF win.ActiveWindow = hwnd THEN
          crt.DrawFrame(hwnd, size, crt.Double, attr);
        ELSE
          crt.DrawFrame(hwnd, size, crt.Single, attr);
        END;
      END;
      crt.DrawControls (hwnd, size, attr, dlt.WND_CTRL_SET{dlt.WinCtrl_Close});
      IF win.ActiveWindow = hwnd THEN
        attr := Colors^[crt.List_ActiveHeader];
      END;
    END;
    crt.DrawHeader (hwnd, size, attr);
    ListBoxLocator (hwnd, msg);
    IF msg.ID = eve.Redraw THEN crt.UpdateRect(size) END;

  | eve.KbHit:
    p := win.GetAMPtr(hwnd);
    CASE msg.par OF
    | key.Up, key.Down, key.PgUp, key.PgDn, key.CtrlHome,
      key.CtrlEnd, key.CtrlPgUp, key.CtrlPgDn :
      size := win.GetWindowSize(hwnd);
      len := size.y2 - size.y1 - 1;
      WITH p^ DO
        IF N < 2 THEN RETURN END;
        eve.AddToTail(hwnd, eve.Paint, 4);
        eve.Flush;
        IF Shift(msg.par, N, len , curr, frame) THEN
          eve.AddToTail(hwnd, eve.Redraw, 3);
        ELSE
          eve.AddToTail(hwnd, eve.Redraw, 0);
        END;
      END;
    | key.Left:
      WITH p^ DO
        IF pos # 0 THEN
          DEC(pos);
          eve.AddToTail(hwnd, eve.Redraw, 0);
        END;
      END;
    | key.Right:
      INC(p^.pos);
      eve.AddToTail(hwnd, eve.Redraw, 0);
    | key.Home:
      p^.pos := 0;
      eve.AddToTail(hwnd, eve.Redraw, 0);
    | key.CtrlW:
      Move_Resize_kbd(hwnd, 0, 0);
      size := win.GetWindowSize(hwnd);
      WITH p^ DO
        Normalize(size, curr, frame, N);
      END;
      eve.AddToTail(hwnd, eve.Redraw,  0);
    ELSE
      ListBoxLocator(hwnd, msg);
    END;
  ELSE
    DefaultProc(hwnd,msg);
  END;
END ListBox;







(* ------- Template for list handler ---------------------

PROCEDURE Locator?name? (hwnd: crt.HWND; i: CARDINAL; VAR str: ARRAY OF CHAR);
VAR
  p: std.PLIST;
BEGIN
  p := win.GetAMPtr(hwnd);
  ?p^ -> line?
  COPY(line, str);
END Locator?name?;


PROCEDURE Handler?name? (hwnd: win.HWND; msg: eve.MSG);
VAR
  p   : std.PLIST;
  size: crt.SZ;

  PROCEDURE write_line (num: CARDINAL);
  VAR
    line: xs.String;
  BEGIN
    WITH p^ DO
      ?-> line?
      crt.SetPos(2, num-frame + 1);
      crt.WrStrFromPos (hwnd, line, Colors^[crt.List_Line], pos);
    END;
  END write_line;

VAR
  i, len: CARDINAL;
  last  : CARDINAL;
  x, y  : CARDINAL;

BEGIN
  p := win.GetAMPtr(hwnd);
  size := win.GetWindowSize(hwnd);
  CASE msg.ID OF
  | eve.Redraw, eve.Paint:
    IF (msg.par # 3) AND (msg.par # 4) THEN
      crt.FillWindow(hwnd, ' ', p^.Colors^[crt.List_Background])
    END;
    WITH size DO len := y2 - y1 - 1; END;
    WITH p^ DO
      IF N > 0 THEN
        CASE msg.par OF
        | 3:
          IF hwnd = win.ActiveWindow THEN
            write_line(curr);
            crt.Lite(hwnd, curr-frame+1 , 1, Colors^[crt.List_CurrentLine]);
          END;
        | 4:
          IF hwnd = win.ActiveWindow THEN
            crt.Lite(hwnd, curr-frame+1 , 1, Colors^[crt.List_Background]);
            write_line(curr);
          END;
        ELSE
          last := std.Min(frame+len-1, N-1);
          FOR i:= frame TO last DO
            write_line(i)
          END;
          IF hwnd = win.ActiveWindow THEN
            crt.Lite(hwnd, curr-frame+1 , 1, Colors^[crt.List_CurrentLine]);
          END;
        END;
      END;
    END;
    std.ListBox(hwnd, msg);

  | eve.QueryAction:
    CASE act.ACTION(msg.par MOD 100H) OF
    | act.?name?:
      act.ConfirmQueryByCond (???);
    ELSE
      std.ListBox (hwnd, msg);
    END;

  | eve.DoAction:
    CASE act.ACTION(msg.par MOD 100H) OF
    | act.?name?:
      ???;
    ELSE
      std.ListBox (hwnd, msg);
    END;

  | eve.R_Mouse:
    ASSERT(win.GetMouse(msg, x, y));
    size := win.GetWindowSize(hwnd);
    IF NOT std.CheckFrame(size, x, y) THEN
      eve.AddToTail(hwnd, eve.Mouse_Pressed, msg.par);
      eve.Flush;
      ???
      puw.PopupWindow(x, y, hwnd, p^.actions);
    END;

  | eve.KbHit:
    CASE msg.par OF
    ELSE
      std.ListBox(hwnd, msg);
    END;
  ELSE
    std.ListBox(hwnd, msg);
  END;
END Handler?name?;


VAR
  ?name?Hwnd: win.HWND;

PROCEDURE Init?name?Window (show: BOOLEAN);
VAR
  p   : std.PLIST;
  size: crt.SZ;
BEGIN
  IF ?hwnd? = win.Invalid_H THEN
    hwnd := win.RegisterWindow (Handler?name?, SIZE(std.LIST)+???);
    ASSERT(hwnd # win.Invalid_H);
    size <- ???
    win.SetWindowSize(hwnd, size);
    win.SetMovable(hwnd);
    win.SetResizable(hwnd, TRUE, TRUE);
    win.SetSwitchable(hwnd);
    win.SetHeaderByStr (act.ActionName[act.?name?]);
    p := win.GetAMPtr(hwnd);
    p^ := std.EmptyList;
    WITH p^ DO
      Colors     := sys.ADR(crt.List);
      Frame      := crt.Double;
      locator    := Locator?name?;
      actions[0] := act.EMPTY_CONTEXT;
      ext        := sys.ADDADR(sys.ADR(p^), SIZE(std.LIST));
    END;
  END;
  IF show THEN
    eve.AddToTail(hwnd, eve.Rise, 0);
  END;
END Init?name?Window;


PROCEDURE Init?name?List (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.?name?);
  Init?name?Window (NOT check);
  IF mode = act.mode_check THEN
    RETURN std.QueryAction(?hwnd?, act.?name?);
  ELSE
    RETURN TRUE;
  END;
END Init?name?List;


BEGIN -- module
  ?name?Hwnd := win.Invalid_H;
  act.IniAction (act.?name?, Init?name?List);


------- Template for list handler ---------------------*)









VAR
  LE_Str      : xs.String;
  LE_Str_pos  : CARDINAL;
  LE_Str_frame: CARDINAL;
  LE_Str_mode : BOOLEAN;
  LE_FirstWait: BOOLEAN;


PROCEDURE LineEditor (hwnd     : win.HWND;
                      Msg      : eve.MSG;
                      x, y     : CARDINAL; 
                      edit_line: PMESSAGE; 
                      edit_part: CARDINAL;
                      active   : BOOLEAN);
VAR
  size : crt.SZ;
  qqq  : PMESSAGE;
  end_s: CARDINAL;
  attr : crt.ATTR;
  light: crt.ATTR;
  i    : CARDINAL;
  xm   : CARDINAL;
  ym   : CARDINAL;

 <* IF DEST_XDS AND (TARGET_OS = "WINNT") THEN *>
  ch          : CHAR;
  ClipBoardMsg: xs.String;
 <* END *>

BEGIN
  CASE Msg.ID OF
  | eve.Rise:
    IF active THEN
      COPY(edit_line^, LE_Str);
      LE_Str_pos := LENGTH(LE_Str);
      IF LE_Str_pos >= edit_part THEN
        LE_Str_frame := LE_Str_pos-edit_part+1;
      ELSE
        LE_Str_frame := 0;
      END;
      LE_FirstWait := LE_Str_pos # 0;
    ELSE  
      LE_Str := '';
      LE_Str_pos := 0;
      LE_Str_frame := 0;
      LE_FirstWait := FALSE;
    END;
    LE_Str_mode := TRUE;
  
  | eve.Mouse_Pressed:
    IF active THEN
      ASSERT(win.GetRelMouse(hwnd, Msg, xm, ym));
      IF (x <= xm) AND (xm <= x+edit_part-1) AND (y = ym) THEN
        size := win.GetWindowSize(hwnd);
        LE_Str_pos := LE_Str_frame+xm-x;
        IF LE_Str_pos > LENGTH(LE_Str) THEN
          LE_Str_pos := LENGTH(LE_Str);
        END;
        LE_FirstWait := FALSE;
      END;
    END;

  | eve.Paint, eve.Redraw:
    size := win.GetWindowSize(hwnd);
    crt.SetPos(x,y);
    IF active THEN
      attr := crt.DialogAttr[crt.Dialog_ActiveEditor];
      IF LE_FirstWait THEN
        light := crt.Attr( crt.Bg(attr), crt.Fg(attr) );
      ELSE
        light := attr;  
      END;
      qqq := sys.ADR(LE_Str[LE_Str_frame]);
      crt.WrNStr(hwnd, edit_part, qqq^, light);
      IF edit_part > LENGTH(qqq^) THEN
        crt.SetPos(x+LENGTH(qqq^), y);
        crt.WrNChar(hwnd, edit_part-LENGTH(qqq^), ' ', attr);
      END;
      crt.SetCurPos(size.x1+x+LE_Str_pos-LE_Str_frame, size.y1+y)
    ELSE
      attr := crt.DialogAttr[crt.Dialog_InactiveEditor];
      crt.WrNChar(hwnd, edit_part, ' ', attr);
      crt.WrNStr(hwnd, edit_part, edit_line^, attr);
    END;
    
  | eve.KbHit:
    IF LE_FirstWait THEN
      CASE Msg.par OF
      | key.Esc:
      | key.BackSpace, key.Del:
        LE_Str := '';
        LE_Str_pos := 0;
        LE_Str_frame := 0;
        LE_FirstWait := FALSE;
      | key.Enter, key.Tab, key.Home, key.End, key.Left, key.Right:
        LE_FirstWait := FALSE;
      | key.Ins:
      ELSE
        IF (Msg.par < 256) & (VAL(CHAR, Msg.par) IN key.LegalChar) THEN
          LE_Str := '';
          LE_Str_pos := 0;
          LE_Str_frame := 0;
          LE_FirstWait := FALSE;
        END;
      END;  
    END;
    CASE Msg.par OF
   <* IF DEST_XDS AND (TARGET_OS = "WINNT") THEN *>
    | key.CtrlV:
      IF cbm.get(ClipBoardMsg) THEN
        i := 0;
        LOOP
          ch := ClipBoardMsg[i];
          IF NOT (ch IN key.LegalChar) THEN EXIT; END;
          Msg := eve.Make (hwnd, eve.KbHit, VAL(key.KEY, ch));
          LineEditor (hwnd, Msg, x, y, edit_line, edit_part, active);
          INC(i);
        END;
      ELSE
        crt.Beep;
      END;
    | key.CtrlIns:
      IF NOT cbm.put (LE_Str) THEN
        crt.Beep;
      END;
   <* END *>
    | key.Esc:
    | key.Enter, key.Tab :
      COPY(LE_Str, edit_line^);
    | key.BackSpace :
      IF LE_Str_pos > 0 THEN
        DEC(LE_Str_pos);
        IF LE_Str_frame > 0 THEN DEC(LE_Str_frame) END;
        IF LENGTH(LE_Str) = HIGH(LE_Str)+1 THEN
          end_s := LENGTH(LE_Str) - 2;
        ELSIF LENGTH(LE_Str) = HIGH(LE_Str) THEN
          end_s := LENGTH(LE_Str) - 1;
        ELSE
          end_s := LENGTH(LE_Str);
        END;
        FOR i := LE_Str_pos TO end_s DO
          LE_Str[i] := LE_Str[i+1];
        END;
        IF LENGTH(LE_Str) = HIGH(LE_Str)+1 THEN
          LE_Str[HIGH(LE_Str)] := '';
        END;
      END;
    | key.Home:
      LE_Str_pos := 0; LE_Str_frame := 0;
    | key.End:
      LE_Str_pos := LENGTH(LE_Str);
      IF LE_Str_pos > HIGH(LE_Str) THEN LE_Str_pos := HIGH(LE_Str); END;
      IF LE_Str_pos > edit_part-1 THEN
        LE_Str_frame := LE_Str_pos-edit_part+1;
      ELSE
        LE_Str_frame := 0;
      END;
    | key.Del:
      IF LE_Str_pos < LENGTH(LE_Str) THEN
        IF LENGTH(LE_Str) = HIGH(LE_Str)+1 THEN
          end_s := LENGTH(LE_Str) - 2;
        ELSIF LENGTH(LE_Str) = HIGH(LE_Str) THEN
          end_s := LENGTH(LE_Str) - 1;
        ELSE
          end_s := LENGTH(LE_Str);
        END;
        FOR i := LE_Str_pos TO end_s DO
          LE_Str[i] := LE_Str[i+1];
        END;
        IF LENGTH(LE_Str) = HIGH(LE_Str)+1 THEN
          LE_Str[HIGH(LE_Str)] := '';
        END;
      END;
    | key.Ins:
      LE_Str_mode := NOT LE_Str_mode;
    | key.Left:
      IF LE_Str_pos > LE_Str_frame THEN
        DEC(LE_Str_pos);
      ELSE
        IF LE_Str_frame > 0 THEN
          DEC(LE_Str_frame);
          LE_Str_pos := LE_Str_frame;
        END;
      END;
    | key.Right:
      IF LE_Str_pos < LENGTH(LE_Str) THEN
        IF LE_Str_pos < LE_Str_frame + edit_part - 1 THEN
          IF LE_Str_pos < HIGH(LE_Str) THEN INC(LE_Str_pos); END;
        ELSE
          IF (LE_Str_frame+edit_part-1 <= LENGTH(LE_Str)) AND (LE_Str_pos < HIGH(LE_Str)) THEN
            INC(LE_Str_pos);
            INC(LE_Str_frame);
          END;
        END;
      END;
    ELSE
      IF (Msg.par < 256) & (VAL(CHAR,Msg.par) IN key.LegalChar) THEN
        IF LE_Str_mode THEN
          IF LENGTH(LE_Str) < HIGH(LE_Str)+1 THEN
            IF LENGTH(LE_Str) = HIGH(LE_Str) THEN
              end_s := LENGTH(LE_Str) - 1;
            ELSE
              end_s := LENGTH(LE_Str);
            END;
            FOR i := end_s TO LE_Str_pos BY -1 DO
              LE_Str[i+1] := LE_Str[i];
            END;
          ELSE
            RETURN;
          END;
        END;
        IF LE_Str_pos <= HIGH(LE_Str) THEN
          LE_Str[LE_Str_pos] := VAL(CHAR, Msg.par);
        END;
        IF LE_Str_pos < LENGTH(LE_Str) THEN
          IF LE_Str_pos < LE_Str_frame + edit_part - 1 THEN
            IF LE_Str_pos < HIGH(LE_Str) THEN
              INC(LE_Str_pos);
             END;
          ELSE
            IF (LE_Str_frame+edit_part-1 <= LENGTH(LE_Str)) AND (LE_Str_pos < HIGH(LE_Str)) THEN
              INC(LE_Str_pos);
              INC(LE_Str_frame);
            END;
          END;
        END;
      END;  
    END;
  ELSE
  END;
END LineEditor;



PROCEDURE DialogProc(hwnd: win.HWND; Msg: eve.MSG);
VAR
  s   : PMESSAGE;
  p   : PDIALOG;
  i,j : CARDINAL;
  attr: crt.ATTR;
  size: crt.SZ;
  sz  : crt.SZ;
  temp: MESSAGE;
  xm  : CARDINAL;
  ym  : CARDINAL;
  cm  : CARDINAL;
  crad: CARDINAL;
  ok  : BOOLEAN;
  tmp : eve.MSG;
BEGIN
  CASE Msg.ID OF
  | eve.Mouse_Pressed, eve.Mouse_Released:
    size := win.GetWindowSize(hwnd);
    ASSERT(win.GetRelMouse(hwnd, Msg, xm, ym));
    CASE crt.GetControl (size, xm, ym, dlt.WND_CTRL_SET{dlt.WinCtrl_Close}) OF
    | dlt.WinCtrl_Close:
      IF Msg.ID = eve.Mouse_Pressed THEN
        eve.AddToTail(hwnd, eve.Hide, 0);
      END;
    ELSE
      IF NOT CheckFrame(size, xm+size.x1, ym+size.y1) THEN
        p := PDIALOG(win.GetAMPtr(hwnd));
        WITH p^ DO
          ok := FALSE;
          cm := MAX(CARDINAL);
          crad := MAX(CARDINAL);
          i := 0;
          LOOP
            WITH Lines^[i] DO
              IF state = d_enabled THEN
                CASE sort OF
                | edit_str:
                  ok := (x <= xm) AND (xm <= x+len-1) AND (y = ym);
                  IF ok THEN
                    cm := i;
                    EXIT;
                  END;

                | button:
                  fmt.print(temp, '[ %s ]', bname);
                  ok := (x <= xm) AND (xm <= x+LENGTH(temp)-1) AND (y = ym);
                  IF ok THEN
                    cm := i;
                    EXIT;
                  END;

                | radio:
                  j := 0;
                  LOOP
                    IF j = HIGH(pradio^)+1 THEN EXIT; END;
                    WITH pradio^[j] DO
                      fmt.print(temp, '( ) %s', name);
                      ok := (x <= xm) AND (xm <= x+LENGTH(temp)-1) AND (y = ym);
                      IF ok THEN
                        crad := j;
                        EXIT;
                      END;
                    END;
                    INC(j);
                  END;
                  IF ok THEN
                    cm := i;
                    EXIT;
                  END;

                | check:
                  fmt.print(temp, '[ ] %s', cname);
                  ok := (x <= xm) AND (xm <= x+LENGTH(temp)-1) AND (y = ym);
                  IF ok THEN
                    cm := i;
                    EXIT;
                  END;

                ELSE
                END;
              END;
            END;
            IF i = HIGH(Lines^) THEN EXIT; END;
            INC(i);
          END;

          IF ok THEN
            IF Msg.ID = eve.Mouse_Pressed THEN
              IF (curr # cm) AND (curr <= HIGH(Lines^)) THEN
                WITH Lines^[curr] DO
                  IF sort = edit_str THEN
                    tmp := eve.Make (hwnd, eve.KbHit, key.Enter);
                    LineEditor(hwnd, tmp, x, y, e_str, len, TRUE);
                  END;
                END;
                WITH Lines^[cm] DO
                  IF sort = edit_str THEN
                    tmp := eve.Make (hwnd, eve.Rise, 0);
                    LineEditor(hwnd, tmp, x, y, e_str, len, TRUE);
                  END;
                END;
              END;
              curr := cm;
              WITH Lines^[curr] DO
                CASE sort OF
                | edit_str:
                  tmp := eve.Make (hwnd, eve.Rise, 0);
                  LineEditor(hwnd, tmp, x, y, e_str, len, TRUE);
                  LineEditor(hwnd, Msg, x, y, e_str, len, TRUE);
                | radio:
                  ractive := crad;
                  rcurr := ractive;
                  IF raction # NIL THEN raction; END;
                | check:
                  cactive^ := NOT cactive^;
                  IF caction # NIL THEN caction; END;
                ELSE
                END;
              END;
              eve.ModalRedraw (hwnd);
            ELSIF Msg.ID = eve.Mouse_Released THEN
              IF curr = cm THEN
                IF Lines^[curr].sort = button THEN
                  eve.AddToTail(hwnd, eve.KbHit, key.Enter);
                END;
              END;
            END;
            RETURN;
          END;

          WITH size DO
            IF ((x2-x1-22) DIV 2 <= xm) AND ((x2-x1-22) DIV 2 + 8 > xm) AND (ym = y2-y1-2) THEN
              ok := TRUE;
              cm := HIGH(Lines^)+1;
            ELSIF ((x2-x1-24) DIV 2 + 12 <= xm) AND ((x2-x1-24) DIV 2 + 22 > xm) AND (ym = y2-y1-2) THEN
              ok := TRUE;
              cm := HIGH(Lines^)+2;
            ELSE
              ok := FALSE;
            END;
          END;
          IF ok THEN
            IF Msg.ID = eve.Mouse_Pressed THEN
              IF (curr # cm) AND (curr <= HIGH(Lines^)) THEN
                WITH Lines^[curr] DO
                  IF sort = edit_str THEN
                    tmp := eve.Make (hwnd, eve.KbHit, key.Enter);
                    LineEditor(hwnd, tmp, x, y, e_str, len, TRUE);
                  END;
                END;
              END;
              curr := cm;
              eve.AddToTail(hwnd, eve.Redraw, 0);
            ELSIF Msg.ID = eve.Mouse_Released THEN
              IF curr = cm THEN
                eve.AddToTail(hwnd, eve.KbHit, key.Enter);
              END;
            END;
            RETURN;
          END;
        END;
      END;
      DefaultProc (hwnd, Msg);
    END

  | eve.Rise:
    p := PDIALOG(win.GetAMPtr(hwnd));
    WITH p^ DO
      curr := 0;
      LOOP
        IF curr > HIGH(Lines^) THEN EXIT; END;
        IF (Lines^[curr].sort # msg) AND
           (Lines^[curr].sort # frame) AND
           (Lines^[curr].state # d_disabled)
        THEN
          EXIT;
        END;
        INC(curr);
      END;
      IF curr <= HIGH(Lines^) THEN
        WITH Lines^[curr] DO
          tmp := eve.Make (hwnd, eve.Rise, 0);
          IF sort = edit_str THEN
            LineEditor(hwnd, tmp, x, y, e_str, len, TRUE);
          ELSE
            LineEditor(hwnd, tmp, 0, 0, NIL, 0, FALSE);
          END;
        END;
      END;
    END;
    IF win.ActiveWindow # hwnd THEN
      eve.AddToTail(win.ActiveWindow, eve.Redraw, 0);
    END;
    win.Rise (hwnd);
    eve.ModalRedraw (hwnd);
    crt.RiseWindow (hwnd);


  | eve.Hide:
    DefaultProc(hwnd, Msg);
    crt.CursorOff;

  | eve.Paint, eve.Redraw:
    crt.FillWindow(hwnd, ' ', crt.DialogAttr[crt.Dialog_Background]);
    size := win.GetWindowSize(hwnd);
    IF win.ActiveWindow = hwnd THEN
      crt.DrawFrame(hwnd, size, crt.Double, crt.DialogAttr[crt.Dialog_Background]);
    ELSE
      crt.DrawFrame(hwnd, size, crt.Single, crt.DialogAttr[crt.Dialog_Background]);
    END;
    crt.DrawControls (hwnd, size, crt.DialogAttr[crt.Dialog_Background], dlt.WND_CTRL_SET{dlt.WinCtrl_Close});
    crt.DrawHeader(hwnd, size, crt.DialogAttr[crt.Dialog_Background]);
    p := PDIALOG(win.GetAMPtr(hwnd));
    WITH p^ DO
      IF (curr <= HIGH(Lines^)) AND (Lines^[curr].sort = edit_str)  AND (win.ActiveWindow = hwnd) THEN
        crt.CursorOn;
      ELSE
        crt.CursorOff;
      END;
      FOR i := 0 TO HIGH(Lines^) DO
        WITH Lines^[i] DO
          crt.SetPos(x,y);
          CASE sort OF
          | msg :
            attr := crt.DialogAttr[crt.Dialog_Message];
            s := sys.ADR(str);
            crt.WrStr(hwnd, s^, attr);
          
          | frame:
            size := win.GetWindowSize(hwnd);
            WITH sz DO
              x1 := x+size.x1; x2 := x1+frame_dx;
              y1 := y+size.y1; y2 := y1+frame_dy;
            END;
            attr := crt.DialogAttr[crt.Dialog_Message];
            crt.DrawFrame(hwnd, sz, frame_type, attr);
                      
          | edit_str:
            LineEditor(hwnd, Msg, x, y, e_str, len, i = p^.curr);
          
          | radio:
            size := win.GetWindowSize(hwnd);
            FOR j := 0 TO HIGH(pradio^) DO
              WITH pradio^[j] DO
                crt.SetPos(x,y);
                IF (state = d_enabled) THEN
                  attr := crt.DialogAttr[crt.Dialog_RadioActive];
                ELSE
                  attr := crt.DialogAttr[crt.Dialog_RadioInactive];
                END;
                crt.WrStr(hwnd, '( )', attr);
                IF (state = d_enabled) AND (j = ractive) THEN
                  crt.SetPos(x+1,y);
                  crt.WrChar(hwnd, CHR(7), attr);
                END;
                IF (state = d_enabled) AND (j = rcurr) AND (i = p^.curr) THEN
                  attr := crt.DialogAttr[crt.Dialog_ActiveButton];
                END;
                crt.SetPos(x + 4, y);
                crt.WrStr(hwnd, name, attr);
                IF (state = d_enabled) AND (hilite # 0) THEN
                  crt.LitePart (hwnd, y, x+3+hilite, x+4+hilite, crt.Attr(crt.Fg(crt.DialogAttr[crt.Dialog_Lite]), crt.Bg(attr)));
                END;
              END;
            END;
            IF (radio_dx # 0) AND (radio_dy # 0) THEN
              WITH sz DO
                x1 := x+size.x1; x2 := x1+radio_dx;
                y1 := y+size.y1; y2 := y1+radio_dy;
              END;
              crt.DrawFrame(hwnd, sz, crt.Single, crt.DialogAttr[crt.Dialog_RadioHeader]);
            END;
            attr := crt.DialogAttr[crt.Dialog_RadioHeader];
            crt.SetPos(x+2,y);
            s := sys.ADR(rname);
            crt.WrStr(hwnd, s^, attr);

          | button:
            IF i = p^.curr THEN
              attr := crt.DialogAttr[crt.Dialog_ActiveButton];
            ELSE
              attr := crt.DialogAttr[crt.Dialog_Button];
            END;
            fmt.print(temp, '[ %s ]', bname);
            crt.WrStr(hwnd, temp, attr);
            IF (state = d_enabled) AND (bhilite # 0) THEN
              crt.LitePart (hwnd, y, x+1+bhilite, x+2+bhilite, crt.Attr(crt.Fg(crt.DialogAttr[crt.Dialog_Lite]), crt.Bg(attr)));
            END;

          | check:
            IF (state = d_enabled) THEN
              attr := crt.DialogAttr[crt.Dialog_CheckActive];
            ELSE
              attr := crt.DialogAttr[crt.Dialog_CheckInactive];
            END;
            crt.WrStr(hwnd, '[ ]', attr);
            IF cactive^ THEN
              crt.SetPos(x+1,y);
              crt.WrChar(hwnd, 'x', attr);
            END;
            IF (state = d_enabled) AND (i = p^.curr) THEN
              attr := crt.DialogAttr[crt.Dialog_ActiveButton];
            END;
            crt.SetPos(x + 4, y);
            crt.WrStr(hwnd, cname, attr);
            IF (state = d_enabled) AND (chilite # 0) THEN
              crt.LitePart (hwnd, y, x+3+chilite, x+4+chilite, crt.Attr(crt.Fg(crt.DialogAttr[crt.Dialog_Lite]), crt.Bg(attr)));
            END;
          END;
        END;
      END;
    END;
    WITH size DO
      crt.SetPos((x2-x1-22) DIV 2, y2-y1-2);
    END;
    IF p^.curr = HIGH(p^.Lines^) + 1 THEN
      attr := crt.DialogAttr[crt.Dialog_ActiveButton];
    ELSE
      attr := crt.DialogAttr[crt.Dialog_Button];
    END;
    crt.WrStr(hwnd,'[  OK  ]', attr);
    WITH size DO
      crt.SetPos(((x2-x1-24) DIV 2) + 12, y2-y1-2);
    END;
    IF p^.curr = HIGH(p^.Lines^) + 2 THEN
      attr := crt.DialogAttr[crt.Dialog_ActiveButton];
    ELSE
      attr := crt.DialogAttr[crt.Dialog_Button];
    END;
    crt.WrStr(hwnd,'[ Cancel ]' , attr);
    IF Msg.ID = eve.Redraw THEN
      crt.UpdateRect(size);
    END;
    eve.CheckWaitForEvent (hwnd, Msg.par);

  | eve.KbHit:
    p := PDIALOG(win.GetAMPtr(hwnd));
    WITH p^ DO
      ok := FALSE;
      cm := MAX(CARDINAL);
      crad := MAX(CARDINAL);
      i := 0;
      LOOP
        WITH Lines^[i] DO
          IF state = d_enabled THEN
            CASE sort OF
            | button:
              IF Msg.par = bhotkey THEN
                ok := TRUE;
                cm := i;
                EXIT;
              END;
            | check:
              IF Msg.par = chotkey THEN
                ok := TRUE;
                cm := i;
                EXIT;
              END;
            | radio:
              j := 0;
              LOOP
                IF j = HIGH(pradio^)+1 THEN EXIT; END;
                IF Msg.par = pradio^[j].hotkey THEN
                  ok := TRUE;
                  cm := i;
                  crad := j;
                  EXIT;
                END;
                INC(j);
              END;
              IF ok THEN EXIT; END;
            ELSE
            END;
          END;
        END;
        IF i = HIGH(Lines^) THEN EXIT; END;
        INC(i);
      END;

      IF ok THEN
        IF (curr # cm) AND (curr <= HIGH(Lines^)) THEN
          WITH Lines^[curr] DO
            IF sort = edit_str THEN
              tmp := eve.Make (hwnd, eve.KbHit, key.Enter);
              LineEditor(hwnd, tmp, x, y, e_str, len, TRUE);
            END;
          END;
          WITH Lines^[cm] DO
            IF sort = edit_str THEN
              tmp := eve.Make (hwnd, eve.Rise, 0);
              LineEditor(hwnd, tmp, x, y, e_str, len, TRUE);
            END;
          END;
        END;
        curr := cm;
        WITH Lines^[curr] DO
          CASE sort OF
          | radio:
            ractive := crad;
            rcurr := ractive;
            eve.AddToTail(hwnd, eve.KbHit, key.Space);
          | button:
            eve.AddToTail(hwnd, eve.KbHit, key.Enter);
          | check:
            eve.AddToTail(hwnd, eve.KbHit, key.Space);
          ELSE
          END;
        END;
        RETURN;
      END;

      IF curr <= HIGH(Lines^) THEN
        WITH Lines^[curr] DO
          CASE sort OF
          | radio:
            CASE Msg.par OF
            | key.Up, key.Left:
              IF rcurr > 0 THEN DEC(rcurr); END;
            | key.Down, key.Right:
              IF rcurr < HIGH(pradio^) THEN INC(rcurr); END;
            | key.Space:
              ractive := rcurr;
              IF raction # NIL THEN raction; END;
            ELSE
            END;

          | edit_str:
            WITH Lines^[p^.curr] DO
              LineEditor(hwnd, Msg, x, y, e_str, len, TRUE);
            END;  
          
          | button:
            IF Msg.par = key.Enter THEN
              IF baction # NIL THEN
                baction ();
              END;
              RETURN;
            END;

          | check:
            IF Msg.par = key.Space THEN
              cactive^ := NOT cactive^;
              IF caction # NIL THEN
                caction ();
              END;
              RETURN;
            END;
          ELSE
          END;
        END;
      END;
    END;
    CASE Msg.par OF
    | key.Enter:
      crt.CursorOff;
      IF p^.curr = HIGH(p^.Lines^)+1 THEN
        IF NOT p^.action(hwnd) THEN
          eve.Flush;
          IF p^.on_error = win.Invalid_H THEN
            crt.Beep;
          ELSE
            eve.AddToTail(p^.on_error, eve.Rise, 0);
            eve.Flush;
          END;
        END;
      ELSIF p^.curr = HIGH(p^.Lines^)+2 THEN
        eve.AddToTail(hwnd, eve.Hide, 0);
      ELSE
        IF NOT p^.action(hwnd) THEN
          IF p^.on_error = win.Invalid_H THEN
            crt.Beep;
          ELSE
            eve.Flush;
            eve.AddToTail(p^.on_error, eve.Rise, 0);
            eve.Flush;
          END;
        END;
      END;

    | key.Esc:
      eve.AddToTail(hwnd, eve.Hide, 0);

    | key.Tab:
      IF p^.curr = HIGH(p^.Lines^)+1 THEN
        INC(p^.curr);
      ELSIF p^.curr = HIGH(p^.Lines^)+2 THEN
        WITH p^ DO
          curr := 0;
          LOOP
            IF curr = HIGH(Lines^) + 1 THEN EXIT; END;
            IF (Lines^[curr].sort # msg) AND 
               (Lines^[curr].sort # frame) AND 
               (Lines^[curr].state # d_disabled)
            THEN
              WITH Lines^[curr] DO
                IF sort = edit_str THEN
                  tmp := eve.Make (hwnd, eve.Rise, 0);
                  LineEditor(hwnd, tmp, 0, 0, e_str, len, TRUE);
                END;
              END;
              EXIT;
            END;
            INC(curr);
          END;
        END;
      ELSE
        WITH p^ DO
          LOOP
            INC(curr);
            IF curr = HIGH(Lines^) + 1 THEN EXIT; END;
            IF (Lines^[curr].sort # msg) AND
               (Lines^[curr].sort # frame) AND
               (Lines^[curr].state # d_disabled)
            THEN
              WITH Lines^[curr] DO
                IF sort = edit_str THEN
                  tmp := eve.Make (hwnd, eve.Rise, 0);
                  LineEditor(hwnd, tmp, 0, 0, e_str, len, TRUE);
                END;
              END;
              EXIT;
            END;
          END;
        END;
      END;

    | key.CtrlW:
      crt.CursorOff;
      Move_Resize_kbd(hwnd, 0, 0);

    ELSE
    END;
    eve.ModalRedraw (hwnd);
  ELSE
    DefaultProc(hwnd, Msg);
  END;
END DialogProc;


PROCEDURE ba_Ok;
BEGIN
  eve.AddToTail (win.ActiveWindow, eve.KbHit, key.Enter);
END ba_Ok;


PROCEDURE ba_Cancel;
BEGIN
  eve.AddToTail (win.ActiveWindow, eve.KbHit, key.Esc);
END ba_Cancel;



VAR
  UniversalDialog: win.HWND;

PROCEDURE OpenUniversalDialog(msg_no: CARDINAL; action : D_ACTION; VAR result: ARRAY OF CHAR);
VAR
  header: xs.String;
BEGIN
  pro.GetMsg (msg_no, header);
  OpenUniversalDialogByStr (header, action, result);
END OpenUniversalDialog;


PROCEDURE OpenUniversalDialogByStr (header-: ARRAY OF CHAR; action : D_ACTION; VAR result: ARRAY OF CHAR);
VAR
  Lines: PLINES;
  size : crt.SZ;
  p    : PDIALOG;
BEGIN
  IF UniversalDialog = win.Invalid_H THEN
    NEW(Lines, 6);

    Lines^[0] := LINE{ 2, 1, msg     , '┌── ──────────────────────────────────┐' , d_enabled};
    Lines^[1] := LINE{ 6, 1, msg     , ''                                        , d_enabled};
    Lines^[2] := LINE{ 2, 2, msg     , '│'                                       , d_enabled};
    Lines^[3] := LINE{ 4, 2, edit_str, sys.ADR(result), 35                       , d_enabled};
    Lines^[4] := LINE{40, 2, msg     ,                                       '│' , d_enabled};
    Lines^[5] := LINE{ 2, 3, msg     , '└─────────────────────────────────────┘' , d_enabled};

    UniversalDialog := win.RegisterWindow(DialogProc, SIZE(DIALOG));
    ASSERT(UniversalDialog # win.Invalid_H);
    win.SetModal(UniversalDialog);
    win.SetMovable(UniversalDialog);

    size.x1 := 18; size.y1 := 8;
    size.x2 := 60; size.y2 := 15;
    win.SetWindowSize(UniversalDialog,size);

    p := win.GetAMPtr(UniversalDialog);
    IF (ErrorMsg = win.Invalid_H) THEN InitErrorMsg; END;
    p^.Lines    := Lines;
    WITH p^ DO
      on_error := ErrorMsg;
    END;
  ELSE
    p := win.GetAMPtr(UniversalDialog);
  END;
  WITH p^ DO
    Lines^[3].e_str := sys.ADR(result);
    curr     := 3;
    COPY(header, Lines^[1].str);
    xs.Append(' ', Lines^[1].str);
  END;
  p^.action := action;
  eve.AddToTail (UniversalDialog, eve.Rise, 0);
END OpenUniversalDialogByStr;


PROCEDURE IniInputDialog(action: D_ACTION; preserve: BOOLEAN; VarName-: ARRAY OF CHAR);
VAR
  Lines: PLINES;
  p    : PDIALOG;
  size : crt.SZ;
  header: MESSAGE;
BEGIN
  IF InputDialog = win.Invalid_H THEN
    NEW(Lines,7);

    pro.GetMsg(mes.Header_SetNewValue, header);
    Lines^[0] := LINE{ 2, 0, msg     , '' , d_enabled};
    Lines^[1] := LINE{ 2, 1, msg     , '┌──────────────────────────────────┐' , d_enabled};
    Lines^[2] := LINE{ 5, 1, msg     , header , d_enabled};
    Lines^[3] := LINE{ 2, 2, msg     , '│' , d_enabled};
    Lines^[4] := LINE{ 4, 2, edit_str, sys.ADR(NewValue), 32                 , d_enabled};
    Lines^[5] := LINE{37, 2, msg     ,                                    '│' , d_enabled};
    Lines^[6] := LINE{ 2, 3, msg     , '└──────────────────────────────────┘' , d_enabled};

    InputDialog := win.RegisterWindow(DialogProc,SIZE(DIALOG));
    ASSERT(InputDialog # win.Invalid_H);
    win.SetModal(InputDialog);
    win.SetMovable(InputDialog);

    size.x1 := 18; size.y1 := 8;
    size.x2 := 60; size.y2 := 15;
    win.SetWindowSize(InputDialog,size);

    p := PDIALOG(win.GetAMPtr(InputDialog));
    p^.curr     := 0;
    IF (ErrorMsg = win.Invalid_H) THEN InitErrorMsg; END;
    p^.on_error := ErrorMsg;
    p^.Lines    := Lines;
  ELSE
    p := PDIALOG(win.GetAMPtr(InputDialog));
  END;
  p^.action   := action;
  WITH p^.Lines^[0] DO
    str := ' ';
    xs.Append(VarName, str);
    xs.Append(' ', str);
  END;
  IF NOT preserve THEN
    NewValue := '';
  END;
  eve.AddToTail (InputDialog, eve.Rise, 0);
END IniInputDialog;


PROCEDURE Switch(hwnd: win.HWND);
VAR
  tmp: win.HWND;
BEGIN
  tmp := win.ActiveWindow;
  IF NOT win.IsSwitchable(hwnd) THEN RETURN END;
  REPEAT
    win.ActiveToTail;
  UNTIL win.IsSwitchable(win.ActiveWindow);
  IF win.ActiveWindow # tmp THEN
    eve.AddToTail(eve.AllWindows,   eve.Redraw, 1);
    eve.AddToTail(win.ActiveWindow, eve.Rise,   0);
  END;
END Switch;


PROCEDURE Action (hwnd: win.HWND; action: act.ACTION; event: eve.EVENT): BOOLEAN;
VAR
  handler: win.WND_PROC;
  msg    : eve.MSG;
BEGIN
  handler := win.GetHandler(hwnd);
  msg.hwnd := hwnd;
  msg.ID := event;
  msg.par := ORD(action);
  act.ClearQuery;
  handler (hwnd, msg);
  RETURN act.CheckQuery();
END Action;

PROCEDURE QueryAction (hwnd: win.HWND; action: act.ACTION): BOOLEAN;
BEGIN
  RETURN Action (hwnd, action, eve.QueryAction);
END QueryAction;

PROCEDURE DoAction(hwnd: crt.HWND; action: act.ACTION): BOOLEAN;
BEGIN
  RETURN Action (hwnd, action, eve.DoAction);
END DoAction;



PROCEDURE QueryKey(hwnd: win.HWND; _key: key.KEY): BOOLEAN;
VAR
  handler: win.WND_PROC;
  msg    : eve.MSG;
BEGIN
  handler := win.GetHandler(hwnd);
  msg.hwnd := hwnd;
  msg.ID := eve.QueryKbHit;
  msg.par := _key;
  act.ClearQuery;
  handler(hwnd, msg);
  RETURN act.CheckQuery();
END QueryKey;


PROCEDURE QueryItem(hwnd: win.HWND; num: CARDINAL): BOOLEAN;
VAR
  handler: win.WND_PROC;
  msg    : eve.MSG;
BEGIN
  handler := win.GetHandler(hwnd);
  msg.hwnd := hwnd;
  msg.ID := eve.QueryItem;
  msg.par := num;
  act.ClearQuery;
  handler(hwnd, msg);
  RETURN act.CheckQuery();
END QueryItem;


PROCEDURE PutItem (main: BOOLEAN; item: CARDINAL): CARDINAL;
BEGIN
  IF main THEN
    RETURN item;
  ELSE
    RETURN 100H+item;
  END;
END PutItem;


PROCEDURE GetItem (par: CARDINAL; VAR main: BOOLEAN; VAR item: CARDINAL);
BEGIN
  main := par DIV 100H = 0;
  item := par MOD 100H;
END GetItem;


-- по параметру обработчика получить действие и кнопку

PROCEDURE GetAction (par: CARDINAL): act.ACTION;
BEGIN
  RETURN act.ACTION (par MOD 100H);
END GetAction;

PROCEDURE GetKey (par: CARDINAL): key.KEY;
BEGIN
  RETURN key.KEY (par DIV 100H);
END GetKey;






PROCEDURE DefaultProc (hwnd: win.HWND; msg: eve.MSG);
VAR
  size: crt.SZ;
  x, y: CARDINAL;
BEGIN
  CASE msg.ID OF
  | eve.Mouse_Pressed:
    ASSERT(win.GetMouse(msg, x, y));
    size := win.GetWindowSize(hwnd);
    CASE crt.GetControl (size, x, y, dlt.WND_CTRL_SET{dlt.WinCtrl_Close}) OF
    | dlt.WinCtrl_Close:
      eve.AddToTail(hwnd, eve.Hide, 0);
    ELSE
      IF CheckFrame(size, x, y) THEN
        Move_Resize_mou (hwnd, 0, 0, x, y);
        eve.ModalRedraw (hwnd);
      END;
    END;

  | eve.Rise:
    IF hwnd # win.ActiveWindow THEN
      eve.AddToTail (win.ActiveWindow, eve.Redraw, 0);
    END;
    win.Rise (hwnd);
    crt.RiseWindow (hwnd);
    eve.AddToTail (hwnd, eve.Redraw, 0);

  | eve.Hide:
    IF hwnd = win.ActiveWindow THEN
      win.ActiveToTail;
    END;
    crt.HideWindow (hwnd);
    eve.AddToTail (win.ActiveWindow, eve.Redraw, 0);

  | eve.Redraw:
    ASSERT(FALSE);

  | eve.DoAction:
    act.ExecuteAction (GetAction(msg.par), act.mode_loud);

  | eve.QueryAction:
    act.ConfirmQueryByCond (act.QueryAction (GetAction (msg.par)));

  | eve.KbHit:
    CASE msg.par OF
    | key.CtrlW:
      Move_Resize_kbd(hwnd, 0, 0);

    | key.CtrlTab:
      Switch(hwnd);

    | key.CtrlF4 :
      IF win.IsSwitchable(hwnd) THEN
        REPEAT
          win.ActiveToTail;
        UNTIL win.IsSwitchable(win.ActiveWindow);
        crt.HideWindow(hwnd);
        win.Hide(hwnd);
        IF hwnd # win.ActiveWindow THEN
          eve.AddToTail(win.ActiveWindow, eve.Rise, 0);
        ELSE
          eve.AddToTail(dv.MainPulldown, eve.Rise, 0);
        END;
      ELSE
        eve.AddToTail(hwnd, eve.Hide, 0);
      END;
    ELSE
      crt.Beep;
    END;

  | eve.QueryKbHit:
    CASE msg.par OF
    | key.CtrlW:
      act.ConfirmQuery;
    | key.CtrlTab:
      act.ConfirmQueryByCond(win.IsSwitchable(hwnd));
    | key.CtrlF4 :
      act.ConfirmQueryByCond(win.IsSwitchable(hwnd));
    ELSE
    END;

  ELSE
  END;
END DefaultProc;


PROCEDURE tglOption (VAR b: BOOLEAN; check: BOOLEAN): BOOLEAN;
BEGIN
  IF check THEN
    RETURN b;
  ELSE
    b := NOT b;
    RETURN TRUE;
  END;
END tglOption;


VAR
  SubmenuViewAs: act.CONTEXT_LIST;

PROCEDURE MakeSubmenuViewAs (VAR context: act.CONTEXT): BOOLEAN;
VAR
  i, n: CARDINAL;
  name: act.CONTEXT_NAME;
BEGIN
  n := lst.EquNamesNo();
  IF n = 0 THEN
    context := act.EMPTY_CONTEXT;
    RETURN FALSE;
  ELSE
    context := act.CONTEXT{ act.submenu, "View as", act.PCONTEXT_LIST(sys.ADR(SubmenuViewAs))};
    n := Min (n, HIGH(SubmenuViewAs));
    FOR i := 0 TO n-1 DO
      lst.GetName (i, name);
      SubmenuViewAs[i] := act.CONTEXT{ act.context_item, name };
    END;
    SubmenuViewAs[n] := act.EMPTY_CONTEXT;
    RETURN TRUE;
  END;
END MakeSubmenuViewAs;



PROCEDURE CenterWindowV (hwnd: crt.HWND);
VAR
  size: crt.SZ;
  y, w: CARDINAL;
BEGIN
  size := win.GetWindowSize (hwnd);
  w := size.y2-size.y1;
  y := (crt.Ymax-(w+1)) DIV 2;
  size.y1 := y;
  size.y2 := y+w;
  win.SetWindowSize (hwnd, size);
END CenterWindowV;


PROCEDURE CenterWindowH (hwnd: crt.HWND);
VAR
  size: crt.SZ;
  x, h: CARDINAL;
BEGIN
  size := win.GetWindowSize (hwnd);
  h := size.x2-size.x1;
  x := (crt.Xmax-(h+1)) DIV 2;
  size.x1 := x;
  size.x2 := x+h;
  win.SetWindowSize (hwnd, size);
END CenterWindowH;


PROCEDURE CenterWindow (hwnd: crt.HWND);
BEGIN
  CenterWindowH (hwnd);
  CenterWindowV (hwnd);
END CenterWindow;



PROCEDURE CenterMessage (size: crt.SZ; msg: PMSG);
VAR
  i, l1, l2, x: CARDINAL;
BEGIN
  l1 := size.x2-size.x1+1;
  IF msg # NIL THEN
    FOR i := 0 TO HIGH(msg^) DO
      l2 := LENGTH(msg^[i].message);
      IF l1 > l2 THEN
        x := (l1-l2) DIV 2;
      ELSE
        x := 0;
      END;
      msg^[i].x := x;
    END;
  END;
END CenterMessage;


PROCEDURE Init_Wnds;         -- SCHERN
BEGIN
   IF dv.F_Init_Wnds # 55 THEN
<* IF DEST_K26 THEN *>

  Wnds := W_NAMES{ WND{ win.Invalid_H, crt.SZ{ 0,  1, 79, 15}, NIL, TRUE,  'Source_Window'    },
                   WND{ win.Invalid_H, crt.SZ{50, 11, 79, 24}, NIL, TRUE,  'Registers_Window' },
                   WND{ win.Invalid_H, crt.SZ{ 0, 16, 50, 24}, NIL, FALSE, 'Breaks_Window'    },
                   WND{ win.Invalid_H, crt.SZ{26, 16, 49, 24}, NIL, FALSE, 'Modules_Window'   },
                   WND{ win.Invalid_H, crt.SZ{26, 16, 49, 24}, NIL, FALSE, 'Labels_Window'    },
                   WND{ win.Invalid_H, crt.SZ{25, 16, 50, 24}, NIL, FALSE, 'Global_Var_Window' },
                   WND{ win.Invalid_H, crt.SZ{25, 16, 50, 24}, NIL, TRUE,  'Module_Var_Window' },
                   WND{ win.Invalid_H, crt.SZ{ 0, 16, 25, 24}, NIL, TRUE,  'Local_Variables'  },
                   WND{ win.Invalid_H, crt.SZ{26, 16, 49, 24}, NIL, FALSE, 'Call_Window'      },
                   WND{ win.Invalid_H, crt.SZ{ 0, 16, 50, 24}, NIL, FALSE, 'Watch_Window'     },
                   WND{ win.Invalid_H, crt.SZ{50, 16, 79, 24}, NIL, FALSE, 'Components_Window'},
                   WND{ win.Invalid_H, crt.SZ{55,  1, 79, 24}, NIL, FALSE, 'Stack_Window'     },
                   WND{ win.Invalid_H, crt.SZ{0,  14, 15, 24}, NIL, FALSE, 'Devices__Window'  }
                };
<* ELSIF DEST_XDS THEN *>

  Wnds := W_NAMES{ WND{ win.Invalid_H, crt.SZ{ 0,  1, 79, 15}, NIL, TRUE,  'Source_Window'     },
                   WND{ win.Invalid_H, crt.SZ{50, 16, 79, 24}, NIL, TRUE,  'Registers_Window'  },
                  <* IF TARGET_OS = "WINNT" THEN *>
                   WND{ win.Invalid_H, crt.SZ{18, 16, 79, 24}, NIL, FALSE,  'FloatRegs_Window'  },
                  <* END *>
                   WND{ win.Invalid_H, crt.SZ{ 0, 16, 50, 24}, NIL, FALSE, 'Breaks_Window'     },
                   WND{ win.Invalid_H, crt.SZ{25, 16, 49, 24}, NIL, FALSE, 'Modules_Window'    },
                   WND{ win.Invalid_H, crt.SZ{25, 16, 49, 24}, NIL, FALSE, 'Labels_Window'     },
                   WND{ win.Invalid_H, crt.SZ{25, 16, 50, 24}, NIL, FALSE, 'Global_Var_Window' },
                   WND{ win.Invalid_H, crt.SZ{25, 16, 50, 24}, NIL, TRUE,  'Module_Var_Window' },
                   WND{ win.Invalid_H, crt.SZ{ 0, 16, 25, 24}, NIL, TRUE,  'Local_Variables'   },
                   WND{ win.Invalid_H, crt.SZ{25, 16, 49, 24}, NIL, FALSE, 'Call_Window'       },
                   WND{ win.Invalid_H, crt.SZ{ 0, 16, 50, 24}, NIL, FALSE, 'Watch_Window'      },
                   WND{ win.Invalid_H, crt.SZ{50, 16, 79, 24}, NIL, FALSE, 'Components_Window' },
                   WND{ win.Invalid_H, crt.SZ{54, 1,  79, 24}, NIL, FALSE, 'Stack_Window'      },
                   WND{ win.Invalid_H, crt.SZ{25, 16, 79, 24}, NIL, FALSE, 'Publics_Window'    },
                   WND{ win.Invalid_H, crt.SZ{0,  16, 49, 24}, NIL, FALSE, 'Threads_Window'    }
                };
<* END *>
      dv.F_Init_Wnds := 55;
   END;
END Init_Wnds;

BEGIN
  Init_Wnds;       --- SCHERN

  UniversalDialog := win.Invalid_H;
  PatternSearch := '';
  InputDialog   := win.Invalid_H;

  OkCancelHwnd := win.Invalid_H;

  ErrorMsg        := win.Invalid_H;
  WarningHwnd     := win.Invalid_H;
  InformationHwnd := win.Invalid_H;
END Dlg_Std.

