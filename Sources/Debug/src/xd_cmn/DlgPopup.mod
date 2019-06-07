IMPLEMENTATION MODULE DlgPopup;

IMPORT crt := CRT;
IMPORT key := Keys;
IMPORT eve := DlgEvent;
IMPORT win := Dlg_Win;
IMPORT act := Dlg_Acts;
IMPORT std := Dlg_Std;
IMPORT dt  := DI_Types;
IMPORT xs  := xStr;




PROCEDURE Max (a, b: CARDINAL): CARDINAL;
BEGIN
  IF a > b THEN RETURN a; ELSE RETURN b; END;
END Max;


TYPE
  STATES_CONTEXT = ARRAY [0..act.MAX_CONTEXTS-1] OF BOOLEAN;
  
  POPUP_WND_DATA = RECORD
                     HwndAct: crt.HWND;
                     actions: act.CONTEXT_LIST;
                     states : STATES_CONTEXT;
                     N      : CARDINAL;
                     frame  : CARDINAL;
                     curr   : CARDINAL;
                   END;

  PPOPUP_WND_DATA = POINTER TO POPUP_WND_DATA;


CONST
  TypesName = 'Type';



PROCEDURE PopupSubmenuWindow (x, y   : CARDINAL; 
                              Hwnd   : crt.HWND;
                              Actions: act.CONTEXT_LIST); FORWARD;

VAR
  TypesContext: act.CONTEXT_LIST;


PROCEDURE PopupWndHandler (hwnd: win.HWND; msg: eve.MSG);
VAR
  p   : PPOPUP_WND_DATA;
  size: crt.SZ;
  main: BOOLEAN;


  PROCEDURE write_action (num: CARDINAL);
  VAR
    pos : CARDINAL;
    name: xs.String;
    attr: crt.ATTR;
  BEGIN
    WITH p^ DO
      WITH actions[num] DO
        CASE context OF
        | act.do_action:
          pos := 2;
          COPY(act.ActionName[action], name);
          IF states[num] THEN
            attr := crt.PopupWindow[crt.Popup_Line];
          ELSE
            attr := crt.PopupWindow[crt.Popup_Inactive];
          END;

        | act.push_key:
          pos := 2;  
          COPY(keyname, name);
          IF states[num] THEN 
            attr := crt.PopupWindow[crt.Popup_Line];
          ELSE 
            attr := crt.PopupWindow[crt.Popup_Inactive];
          END;  

        | act.context_item:
          pos := 2;
          COPY(item_name, name);
          IF states[num] THEN
            attr := crt.PopupWindow[crt.Popup_Line];
          ELSE
            attr := crt.PopupWindow[crt.Popup_Inactive];
          END;

        | act.submenu:
          ASSERT(main);
          COPY(subname, name);
          pos := LENGTH(name);
          WHILE pos < size.x2-size.x1-2 DO name[pos] := ' '; INC(pos); END;
          name[pos] := '';
          name[pos-1] := CHR(16);
          pos := 2;  
          IF states[num] THEN
            attr := crt.PopupWindow[crt.Popup_Line];
          ELSE
            attr := crt.PopupWindow[crt.Popup_Inactive];
          END;

        | act.toggler:
          COPY(togglname, name);
          IF states[num] THEN 
            pos := LENGTH(name);
            name[pos+1] := '';
            REPEAT name[pos] := name[pos-1]; DEC(pos); UNTIL (pos=0);  
            IF togglproc(TRUE) THEN
              name[0] := '+';
            ELSE  
              name[0] := '-';
            END;
            pos := 1;
            attr := crt.PopupWindow[crt.Popup_Line];
          ELSE 
            pos := 2;
            attr := crt.PopupWindow[crt.Popup_Inactive];
          END;  

        | act.radio:
          ASSERT(main);
          COPY(radioname, name);
          pos := LENGTH(name);
          WHILE pos < size.x2-size.x1-2 DO name[pos] := ' '; INC(pos); END;
          name[pos] := '';
          name[pos-1] := CHR(16);
          pos := 2;  
          IF states[num] THEN 
            attr := crt.PopupWindow[crt.Popup_Line];
          ELSE  
            attr := crt.PopupWindow[crt.Popup_Inactive];
          END;  

        | act.types:
          COPY(TypesName, name);
          pos := LENGTH(name);
          WHILE pos < size.x2-size.x1-2 DO name[pos] := ' '; INC(pos); END;
          name[pos] := '';
          name[pos-1] := CHR(16);
          pos := 2;
          IF states[num] THEN
            attr := crt.PopupWindow[crt.Popup_Line];
          ELSE
            attr := crt.PopupWindow[crt.Popup_Inactive];
          END;

        | act.chngtypes:
          pos := 2;
          COPY(dt.Types[t_value].name, name);
          attr := crt.PopupWindow[crt.Popup_Line];

        | act.separate:
          FOR pos := 0 TO HIGH(name) DO name[pos] := CHR(196); END; 
          pos := 1;
          attr := crt.PopupWindow[crt.Popup_Frame];
        END;
      END;
      crt.SetPos(pos, num-frame+1);
      crt.WrStr(hwnd, name, attr);
    END;
  END write_action;


TYPE
  SUB_MENUS = SET OF act.CONTEXT_ACTION;

CONST
  HasSubMenu = SUB_MENUS{ act.submenu, act.radio, act.types };

VAR
  i, y: CARDINAL;
  len : CARDINAL;
  last: CARDINAL;
  attr: crt.ATTR;
  hide: BOOLEAN;
  type: dt.SYM_TYPE_ID;
  sub : BOOLEAN;
  old : CARDINAL;

BEGIN
  main := (hwnd = PopupMainHwnd);
  IF main AND win.IsValid(PopupGroupHwnd) AND win.Visible(PopupGroupHwnd) THEN
    eve.AddToTail(PopupGroupHwnd, eve.Hide, 0);
  END;
  p := win.GetAMPtr(hwnd);
  size := win.GetWindowSize(hwnd);
  CASE msg.ID OF
  | eve.Mouse_Pressed, eve.Mouse_Moved, eve.Mouse_Released:
    ASSERT(win.GetRelMouse(hwnd, msg, i, y));
    IF (y # 0) AND (y # size.y2-size.y1) THEN
      p := win.GetAMPtr(hwnd);
      WITH p^ DO
        old := curr;
        curr := frame + (y-1);
        sub := states[curr] AND (actions[curr].context IN HasSubMenu);
        IF old # curr THEN
          eve.AddToTail(hwnd, eve.Redraw, 0);
        END;
        IF (msg.ID = eve.Mouse_Released) OR ((hwnd = PopupMainHwnd) AND sub) THEN
          eve.AddToTail(hwnd, eve.KbHit, key.Enter);
        END;
      END;
    END;

  | eve.Rise:
    IF PopupMainHwnd # hwnd THEN
      win.SetPair(hwnd, PopupMainHwnd);
      win.SetPair(PopupMainHwnd, hwnd);
    END;
    WITH p^ DO
      FOR i := 0 TO N-1 DO 
        WITH actions[i] DO
          CASE context OF
          | act.do_action:
            states[i] := std.QueryAction(HwndAct, action);

          | act.push_key:
            states[i] := std.QueryKey(HwndAct, keycode);

          | act.context_item:
            states[i] := std.QueryItem(HwndAct, std.PutItem(main, i));

          | act.submenu:
            ASSERT(main); 
            states[i] := std.QueryItem(HwndAct, std.PutItem(main, i));

          | act.types:
            ASSERT(main);
            states[i] := std.QueryAction(HwndAct, act.ChangeTypes);
          
          | act.toggler:
            states[i] := togglproc # NIL;

          | act.radio:
            ASSERT(main); 
            states[i] := radioproc # NIL;
                    
          | act.chngtypes:
            states[i] := TRUE;

          | act.separate:
            states[i] := FALSE;
          END;
        END;
      END;
    END;
(*
    IF win.ActiveWindow # hwnd THEN
      eve.AddToTail(win.ActiveWindow, eve.Redraw, 0);
    END;
*)
    win.Rise(hwnd);
    crt.RiseWindow(hwnd);
    eve.AddToTail(hwnd, eve.Redraw,0);

  | eve.Hide:
    crt.HideWindow(hwnd);
    win.ActiveToTail;
    eve.AddToTail(win.ActiveWindow, eve.Redraw, 0);
      
  | eve.Paint, eve.Redraw:
    IF (msg.par # 3) AND (msg.par # 4) THEN
      crt.FillWindow(hwnd, ' ', crt.PopupWindow[crt.Popup_Background])
    END;
    WITH size DO len := y2-y1-1; END;
    WITH p^ DO
      IF N > 0 THEN
        IF states[curr] THEN 
          attr := crt.PopupWindow[crt.Popup_CurrentLine]
        ELSE 
          attr := crt.PopupWindow[crt.Popup_CurrInactive];
        END;  
        CASE msg.par OF
        | 3: write_action(curr);
             crt.Lite(hwnd, curr-frame+1 , 1, attr);
        | 4: crt.Lite(hwnd, curr-frame+1 , 1, crt.PopupWindow[crt.Popup_Background]);
             write_action(curr);
        ELSE
          last := std.Min(frame+len-1, N-1);
          FOR i:= frame TO last DO write_action(i); END;
          crt.Lite(hwnd, curr-frame+1 , 1, attr);
        END;
      END;
    END;
    crt.DrawFrame(hwnd, size, crt.Single, crt.PopupWindow[crt.Popup_Frame]);
    IF msg.ID = eve.Redraw THEN crt.UpdateRect(size) END;

  | eve.KbHit:
    CASE msg.par OF
    | key.Enter:
      WITH p^ DO
        IF states[curr] THEN
          hide := TRUE;
          WITH actions[curr] DO
            CASE context OF
            | act.do_action:
              eve.AddToTail(hwnd, eve.Hide, 0);
              eve.AddToTail(HwndAct, eve.DoAction, ORD(action));

            | act.push_key:
              eve.AddToTail(hwnd, eve.Hide, 0);
              eve.AddToTail(HwndAct, eve.KbHit, keycode);

            | act.context_item:
              eve.AddToTail(hwnd, eve.Hide, 0);
              eve.AddToTail(HwndAct, eve.ContextItem, std.PutItem(main, curr));

            | act.submenu:
              ASSERT(main);
              hide := FALSE;
              PopupSubmenuWindow(size.x2-1, size.y1+curr, HwndAct, pscontext^);

            | act.types:
              ASSERT(main);
              hide := FALSE;
              i := 0;
              FOR type := MIN(dt.SYM_TYPE_ID) TO MAX(dt.SYM_TYPE_ID) DO
                IF type IN tenable THEN
                  TypesContext[i] := act.CONTEXT{ act.chngtypes, type };
                  INC(i);
                END;
              END;
              ASSERT(i#0);
              TypesContext[i] := act.EMPTY_CONTEXT;
              PopupSubmenuWindow(size.x2-1, size.y1+curr, HwndAct, TypesContext);

            | act.chngtypes:
              act.ChangeTypesID := t_value;
              eve.AddToTail(hwnd, eve.Hide, 0);
              eve.AddToTail(HwndAct, eve.DoAction, ORD(act.ChangeTypes));

            | act.radio:
              ASSERT(main);
              hide := FALSE;
              PopupSubmenuWindow(size.x2-1, size.y1+curr, HwndAct, prcontext^);

            | act.toggler:
              eve.AddToTail(hwnd, eve.Hide, 0);
              ASSERT(togglproc(FALSE));
            END;
          END;
          IF hide AND NOT main THEN
            eve.AddToTail(PopupMainHwnd, eve.Hide, 0);
          END;  
        END;  
      END;

    | key.Up, key.Down, key.PgUp, key.PgDn
    , key.CtrlHome, key.CtrlEnd, key.CtrlPgUp, key.CtrlPgDn:
      len := size.y2-size.y1-1;
      WITH p^ DO
        IF N < 2 THEN RETURN; END;
        eve.AddToTail(hwnd, eve.Paint, 4);
        eve.Flush;
        IF std.Shift(msg.par, N, len, curr, frame) THEN
          eve.AddToTail(hwnd, eve.Redraw, 3);
        ELSE
          eve.AddToTail(hwnd, eve.Redraw, 0);
        END;
      END;
    ELSE
      eve.AddToTail(hwnd, eve.Hide, 0);
      IF NOT main THEN
        eve.AddToTail(PopupMainHwnd, eve.Hide, 0);
      END;  
    END;

  | eve.Defocus:
    eve.AddToHead(hwnd, eve.Hide, 0);
    IF NOT main THEN
      IF msg.par = PopupMainHwnd THEN
        eve.AddToHead(PopupMainHwnd, eve.Redraw, 0);
      ELSE
        eve.AddToHead(PopupMainHwnd, eve.Hide, 0);
      END;  
    END;
    IF p^.HwndAct # win.ActiveWindow THEN
      eve.AddToHead(p^.HwndAct, eve.Defocus, 0);
    END;

  ELSE
    eve.AddToTail(hwnd, eve.Hide, 0);
    IF NOT main THEN
      eve.AddToTail(PopupMainHwnd, eve.Hide, 0);
    END;  
  END;
END PopupWndHandler;



PROCEDURE InitPopupWindow (VAR curr_hwnd: crt.HWND;
                               main     : BOOLEAN; 
                               x,y      : CARDINAL;
                               Hwnd     : crt.HWND;
                               Actions- : act.CONTEXT_LIST);
VAR
  p   : PPOPUP_WND_DATA;
  size: crt.SZ;
  i   : CARDINAL;
  max : CARDINAL;

  PROCEDURE SetMaxLength (name-: ARRAY OF CHAR; ext: CARDINAL);
  BEGIN
    max := Max(max, std.Min(crt.Ymax-5, LENGTH(name)+ext));
  END SetMaxLength;

  PROCEDURE CorrectPositionWindow;
  VAR
    delta: CARDINAL;
    sz   : crt.SZ;
  BEGIN
    WITH size DO
      IF x2 >= crt.Xmax THEN
        IF main THEN
          delta := x2 - x1;
          x2 := x1-2;
          DEC(x1, delta+2);
        ELSE
          sz := win.GetWindowSize(PopupMainHwnd);
          delta := x2 - sz.x1;
          DEC(x1, delta);
          DEC(x2, delta);
        END;  
      END; 
      IF y2 >= crt.Ymax THEN
        delta := y2 - crt.Ymax + 1;
        DEC(y1, delta);
        DEC(y2, delta);
      END;
    END;
  END CorrectPositionWindow;


BEGIN
  IF curr_hwnd = win.Invalid_H THEN
    curr_hwnd := win.RegisterWindow(PopupWndHandler, SIZE(POPUP_WND_DATA));
    ASSERT(curr_hwnd # win.Invalid_H);
    win.ExcludeFromList (curr_hwnd);
  END;
  p := win.GetAMPtr(curr_hwnd);
  WITH p^ DO
    N := 0;
    max := 0;
    i := 0;
    LOOP
      CASE Actions[i].context OF
      | act.nothing: EXIT;

      | act.do_action:
        IF act.Action[Actions[i].action] # NIL THEN
          actions[N] := Actions[i];
          INC(N);
          SetMaxLength(act.ActionName[Actions[i].action], 0);
        END;

      | act.push_key:
        actions[N] := Actions[i];
        INC(N);
        SetMaxLength(Actions[i].keyname, 0);

      | act.context_item:
        actions[N] := Actions[i];
        INC(N);
        SetMaxLength(Actions[i].item_name, 0);

      | act.submenu:
        actions[N] := Actions[i];
        INC(N);
        SetMaxLength(Actions[i].subname, 1);

      | act.radio:
        actions[N] := Actions[i];
        INC(N);
        SetMaxLength(Actions[i].radioname, 1);

      | act.toggler:
        actions[N] := Actions[i];
        INC(N);
        SetMaxLength(Actions[i].togglname, 0);
      
      | act.types:
        actions[N] := Actions[i];
        INC(N);
        SetMaxLength(TypesName, 1);

      | act.chngtypes:
        actions[N] := Actions[i];
        INC(N);
        SetMaxLength(dt.Types[Actions[i].t_value].name, 0);

      | act.separate:
        actions[N] := Actions[i];
        INC(N);
      END;
      INC(i);
      IF i > HIGH(Actions) THEN EXIT; END;
    END;
    IF N # 0 THEN
      HwndAct := Hwnd;
      curr := 0;
      frame := 0;
      WITH size DO
        x1 := x+1;
        y1 := y+1;
        x2 := x1+max+3;
        y2 := y1+N+1;
      END;
      CorrectPositionWindow;
      win.SetWindowSize(curr_hwnd, size);
      eve.AddToTail(curr_hwnd, eve.Rise, 0);
    END;
  END;
END InitPopupWindow;



PROCEDURE PopupWindow (x,y: CARDINAL; Hwnd: crt.HWND; Actions-: act.CONTEXT_LIST);
BEGIN
  InitPopupWindow(PopupMainHwnd, TRUE, x, y, Hwnd, Actions);
END PopupWindow;  


PROCEDURE PopupSubmenuWindow (x,y: CARDINAL; Hwnd: crt.HWND; Actions-: act.CONTEXT_LIST);
BEGIN
  InitPopupWindow(PopupGroupHwnd, FALSE, x, y, Hwnd, Actions);
END PopupSubmenuWindow;  




BEGIN
  PopupMainHwnd  := win.Invalid_H;
  PopupGroupHwnd := win.Invalid_H;
END DlgPopup.
