<* Storage+ *>
IMPLEMENTATION MODULE Dlg_Menu;

IMPORT opt := Options;

IMPORT crt := CRT;
IMPORT key := Keys;

IMPORT eve := DlgEvent;
IMPORT win := Dlg_Win;
IMPORT act := Dlg_Acts;
IMPORT dv  := Dlg_Vars;
IMPORT std := Dlg_Std;

FROM DlgTypes IMPORT PullDownMenus, PULLDOWN, PPULLDOWN;


<* IF SCHERN_K26 THEN *>
(*                           CHERN                    *)
IMPORT
        prf := Asmprof,
        tim := Timeprof,
        wsv := Windsave,
        lpr := Lineprof,
        prv := Prf_Var,
        wbr := Windbrk,
        tpr := Time_prc;                      (* CHERN *)
CONST
  MainMenu_N = 8+1;                           (* CHERN +1*)

<* END *>


TYPE
  MAIN_MENU_REC = RECORD
                    hwnd : win.HWND;
                    x_pos: CARDINAL;
                    name : ARRAY [0..15] OF CHAR;
                    pos  : CARDINAL;
                    hot  : CARDINAL;
                    l_hot: CARDINAL;
                    action: act.ACTION;
                  END;

  MAIN_MENU     = ARRAY PullDownMenus OF MAIN_MENU_REC;

VAR
  MainMenu: MAIN_MENU;


PROCEDURE GetMenuHwnd (menu: PullDownMenus): win.HWND;
BEGIN
  RETURN MainMenu[menu].hwnd;
END GetMenuHwnd;


PROCEDURE UpdateMenuHwnd (menu: PullDownMenus; hwnd: win.HWND);
BEGIN
  MainMenu[menu].hwnd := hwnd;
END UpdateMenuHwnd;


<* IF SCHERN_K26 THEN *>
VAR
  ProfMenu           : win.HWND;                         (* CHERN *)
  ProfBreakMenu      : win.HWND;                         (* CHERN *)
  ProfBlokMenu       : win.HWND;                         (* CHERN *)
<* END *>


TYPE
  BSTATE = (enabled, disabled, separator);

  MBUTTON = RECORD
             CASE state: BSTATE OF
             | enabled, disabled:
               action: act.ACTION;
               hotkey : CHAR;     (* локальная горячая клавиша *)
               l_pos  : CARDINAL; (* позиция подсвечиваемого элемента в имени пункта *)
             ELSE
             END;
           END;

  MBUTTONS = POINTER TO ARRAY OF MBUTTON;

  MENU = RECORD
           maxname: CARDINAL;
           curr   : CARDINAL;
           pos    : CARDINAL;
           Buttons: MBUTTONS;
         END;
  PMENU = POINTER TO MENU;


VAR
  beforePulldown: win.HWND;

PROCEDURE MenuProc(hwnd: win.HWND; msg: eve.MSG);
VAR
  atr    : crt.ATTR;
  p      : PMENU;
  p2     : PPULLDOWN;
  i      : CARDINAL;
  save   :  CARDINAL;
  size   : crt.SZ;
  x, y   : CARDINAL;
  keyname: ARRAY [0..30] OF CHAR;
BEGIN
  CASE msg.ID OF
  | eve.Defocus:
    eve.AddToHead(hwnd, eve.Hide, 0);

  | eve.Mouse_Dbl, eve.Mouse_Moved, eve.Mouse_Pressed, eve.Mouse_Released:
    ASSERT(win.GetMouse(msg, x, y));
    size := win.GetWindowSize(hwnd);
    IF NOT std.CheckFrame(size, x, y) THEN
      ASSERT(win.GetRelMouse(hwnd, msg, x, y));
      p := PMENU(win.GetAMPtr(hwnd));
      WITH p^ DO
        curr := y-1;
        eve.AddToTail(hwnd, eve.Redraw, 0);
        IF msg.ID = eve.Mouse_Released THEN
          eve.AddToTail(hwnd, eve.KbHit, key.Enter);
        END;
      END;
    END;

  | eve.Rise:
    win.SetPair(hwnd, dv.MainPulldown);
    win.SetPair(dv.MainPulldown, hwnd);
    p := PMENU(win.GetAMPtr(hwnd));
    WITH p^ DO
      FOR i := 0 TO HIGH(p^.Buttons^) DO
        WITH p^.Buttons^[i] DO
          CASE state OF
          | enabled, disabled:
            ASSERT( action # act.None);
            IF std.QueryAction(SkipMenus(), action) THEN
              state := enabled;
            ELSE
              state := disabled;
            END;
          | separator:
          END;
        END;
      END;
      curr := 0;
      WHILE Buttons^[curr].state = separator DO INC(curr) END;
    END;
    p2 := win.GetAMPtr(dv.MainPulldown);
    WITH p2^ DO
      curr   := VAL(PullDownMenus, p^.pos);
      opened := hwnd;
    END;
    IF win.ActiveWindow # dv.MainPulldown THEN
      eve.AddToTail(win.ActiveWindow, eve.Redraw,  0);
      eve.AddToTail(dv.MainPulldown, eve.Redraw, 2);
    ELSE
      eve.AddToTail(win.ActiveWindow, eve.Redraw,  2);
    END;
    win.Rise(hwnd);
    crt.RiseWindow(hwnd);
    eve.AddToTail(hwnd, eve.Redraw,  0);

  | eve.Hide:
    p2 := win.GetAMPtr(dv.MainPulldown);
    WITH p2^ DO
      opened := win.Invalid_H;
    END;
    crt.HideWindow(hwnd);
    win.ActiveToTail;
    IF win.ActiveWindow = dv.MainPulldown THEN
      win.ActiveToTail;
    END;
    eve.AddToTail(dv.MainPulldown, eve.Redraw, 0);
    eve.AddToTail(win.ActiveWindow, eve.Redraw, 0);

  | eve.Paint, eve.Redraw:
    size := win.GetWindowSize(hwnd);
    crt.FillWindow(hwnd,' ',crt.Menu[crt.Menu_Background]);
    p := PMENU(win.GetAMPtr(hwnd));
    FOR i:=0  TO HIGH(p^.Buttons^) DO
      WITH p^.Buttons^[i] DO
        IF NOT key.GetKeyName(act.GetPrimaryShortCut(action), keyname) THEN
          keyname := '';
        END;
        crt.SetPos(2, i+1);
        CASE state OF
        | enabled:
          atr := crt.Menu[crt.Menu_Button];
          crt.WrStr(hwnd, act.ActionName[action], atr);
          crt.SetPos(2 + p^.maxname, i+1);
          crt.WrStr(hwnd, keyname, atr);
        | disabled :
          atr := crt.Menu[crt.Menu_Inactive];
          crt.WrStr(hwnd, act.ActionName[action], atr);
          crt.SetPos(2 + p^.maxname, i+1);
          crt.WrStr(hwnd, keyname, atr);
        | separator:
          crt.SetPos(1, i+1);
          atr := crt.Menu[crt.Menu_Frame];
          crt.WrNChar(hwnd, size.x2 - size.x1 -1, CHR(196), atr);
        END;
        IF p^.curr = i THEN
          IF p^.Buttons^[i].state = enabled THEN
            atr := crt.Menu[crt.Menu_CurrentButton];
          ELSE
            atr := crt.Menu[crt.Menu_CurrInactive];
          END;
          crt.Lite(hwnd, i+1, 1, atr);
        END;
        IF state = enabled THEN
          IF (hotkey <> 0C) AND (l_pos <> 0) THEN
            IF p^.curr = i THEN
              atr := crt.Menu[crt.Menu_CurrentLite];
            ELSE
              atr := crt.Menu[crt.Menu_Lite];
            END;
            crt.SetPos(l_pos+1, i+1);
            crt.WrChar(hwnd,act.ActionName[action][l_pos-1],atr);
          END;
        END;
      END;
    END;
    crt.DrawFrame(hwnd, size, crt.Single, crt.Menu[crt.Menu_Frame]);
    IF msg.ID =eve.Redraw THEN crt.UpdateRect(size); END;

  | eve.QueryAction:
    CASE VAL(act.ACTION, msg.par MOD 100H) OF
    | act.ContextMenu:
      act.ConfirmQuery;
    ELSE
    END;

  | eve.QueryKbHit:
    CASE msg.par OF
    | key.Down:
      p := PMENU(win.GetAMPtr(hwnd));
      act.ConfirmQueryByCond(p^.curr # HIGH(p^.Buttons^));
    | key.Up:
      p := PMENU(win.GetAMPtr(hwnd));
      act.ConfirmQueryByCond(p^.curr # 0);
    | key.Left, key.Right:
      act.ConfirmQueryByCond(std.QueryKey(dv.MainPulldown, msg.par));
    | key.Esc:
      act.ConfirmQuery;
    | key.Enter:
      p := PMENU(win.GetAMPtr(hwnd));
      act.ConfirmQueryByCond(p^.Buttons^[p^.curr].state = enabled);
    ELSE
    END;

  | eve.DoAction:
    eve.AddToTail(hwnd, eve.KbHit, msg.par DIV 100H);

  | eve.KbHit:
    CASE msg.par OF
    | key.Down , key.Up :
      p := PMENU(win.GetAMPtr(hwnd));
      WITH p^ DO
        save := curr;
        IF msg.par = key.Down THEN
          LOOP
            IF (curr = HIGH(Buttons^)) THEN EXIT END;
            INC(curr);
            IF Buttons^[curr].state # separator THEN EXIT END;
          END;
        ELSE
          LOOP
            IF (curr = 0) THEN EXIT END;
            DEC(curr);
            IF Buttons^[curr].state # separator THEN EXIT END;
          END;
        END;
        IF Buttons^[curr].state = separator THEN curr := save END;
      END;
      eve.AddToTail(hwnd, eve.Redraw,0);

    | key.Left, key.Right:
      win.ActiveToTail;
      crt.HideWindow(hwnd);
      eve.AddToTail(dv.MainPulldown,eve.KbHit,msg.par*256*256);
      eve.AddToTail(dv.MainPulldown,eve.KbHit,key.Enter*256*256);

    | key.Enter:
      p := PMENU(win.GetAMPtr(hwnd));
      eve.AddToTail(hwnd, eve.Hide, 0);
      eve.Flush;
      IF p^.Buttons^[p^.curr].state = enabled THEN
        WITH p^.Buttons^[p^.curr] DO
          ASSERT( action # act.None);
          eve.AddToTail(win.ActiveWindow, eve.DoAction, ORD(action));
        END;
      END;

    | key.Esc:
      eve.AddToTail(hwnd,eve.Hide,0);
    ELSE
      p := PMENU(win.GetAMPtr(hwnd));
      WITH p^ DO
        FOR i := 0 TO HIGH(Buttons^) DO
          WITH Buttons^[i] DO
            IF (msg.par = ORD(hotkey)) OR (msg.par = ORD(CAP(hotkey))) THEN
              curr := i;
              eve.AddToTail(hwnd, eve.Redraw, 0);
              eve.AddToTail(hwnd, eve.KbHit, key.Enter);
              RETURN;
            END;
          END;
        END;
      END;
    END;
  ELSE
    std.DefaultProc(hwnd,msg);
  END;
END MenuProc;


PROCEDURE RecalcSize(menu: PMENU; VAR size: crt.SZ);
VAR
  keyname: ARRAY [0..25] OF CHAR;
  i, len, max1, max2: CARDINAL;
BEGIN
  max1 := 0;
  max2 := 0;
  FOR i := 0 TO HIGH(menu^.Buttons^) DO
    WITH menu^.Buttons^[i] DO
      IF state # separator THEN
        len := LENGTH(act.ActionName[action]);
        IF max1 < len THEN
          max1 := len;
        END;
        IF key.GetKeyName(act.GetPrimaryShortCut(action), keyname) THEN
          len := LENGTH(keyname);
          IF max2 < len THEN
            max2 := len;
          END;
        END;
      END;
    END;
  END;
  menu^.maxname := max1 + 2;
  size.x2 := size.x1 + max1 + max2 + 3 + 2;
END RecalcSize;


PROCEDURE File (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  menu: PMENU;
  Buttons: MBUTTONS;
  size: crt.SZ;
BEGIN
  ASSERT(action = act.FileMenu);
  IF mode # act.mode_check THEN
    WITH MainMenu[FileMenu] DO
      IF hwnd = win.Invalid_H THEN
       <* IF DEST_K26 THEN *>
        NEW(Buttons, 10);
        IF NOT opt.DialogMode THEN
          Buttons^[0] := MBUTTON{enabled, act.LoadTableDevises,  'n', 1};
          Buttons^[1] := MBUTTON{enabled, act.Refresh,           'g', 1};
          Buttons^[2] := MBUTTON{enabled, act.Show_scr,          "'", 10};
          Buttons^[3] := MBUTTON{separator};
          Buttons^[4] := MBUTTON{enabled, act.Pallette,          "h", 1};
          Buttons^[5] := MBUTTON{enabled, act.Options,           'f', 2};
          Buttons^[6] := MBUTTON{enabled, act.SaveConfig,              'c', 1};
          Buttons^[7] := MBUTTON{separator};
          Buttons^[8] := MBUTTON{enabled, act.ReturnToBatch,     '[', 3};
          Buttons^[9] := MBUTTON{enabled, act.Halt,              'r', 1};
        ELSE
          Buttons^[0] := MBUTTON{enabled, act.Load,              'p', 1};
          Buttons^[1] := MBUTTON{enabled, act.LoadTableDevises,  'n', 1};
          Buttons^[2] := MBUTTON{enabled, act.Refresh,           'g', 1};
          Buttons^[3] := MBUTTON{enabled, act.Show_scr,          "'", 10};
          Buttons^[4] := MBUTTON{separator};
          Buttons^[5] := MBUTTON{enabled, act.Pallette,          'h', 1};
          Buttons^[6] := MBUTTON{enabled, act.Options,           'f', 2};
          Buttons^[7] := MBUTTON{enabled, act.SaveConfig,              'c', 1};
          Buttons^[8] := MBUTTON{separator};
          Buttons^[9] := MBUTTON{enabled, act.Quit,              '[', 3};
        END;
       <* ELSE *>
        NEW(Buttons, 14);
        IF NOT opt.DialogMode THEN
          Buttons^[0]  := MBUTTON{enabled, act.Refresh,  'r', 1};
          Buttons^[1]  := MBUTTON{enabled, act.Show_scr,  '', 0};
          Buttons^[2]  := MBUTTON{enabled, act.Show_wnd,  '', 0};
          Buttons^[3]  := MBUTTON{separator};
          Buttons^[4]  := MBUTTON{enabled, act.Pallette,  'p', 1};
          Buttons^[5]  := MBUTTON{enabled, act.Options,   'o', 1};
          Buttons^[6]  := MBUTTON{separator};
          Buttons^[7]  := MBUTTON{enabled, act.SaveConfig,'s', 1};
          Buttons^[8]  := MBUTTON{enabled, act.SaveKeyboardLayout, 'k', 6};
          Buttons^[9]  := MBUTTON{enabled, act.SaveAsBatch,        'v', 3};
          Buttons^[10] := MBUTTON{enabled, act.RestoreFromBatch,   't', 4};
          Buttons^[11] := MBUTTON{separator};
          Buttons^[12] := MBUTTON{enabled, act.ReturnToBatch, 'e', 2};
          Buttons^[13] := MBUTTON{enabled, act.Halt  ,        '', 0};
        ELSE
          Buttons^[0]  := MBUTTON{enabled, act.Load,     'l', 1};
          Buttons^[1]  := MBUTTON{enabled, act.Refresh,  'r', 1};
          Buttons^[2]  := MBUTTON{enabled, act.Show_scr,  '', 0};
          Buttons^[3]  := MBUTTON{enabled, act.Show_wnd,  '', 0};
          Buttons^[4]  := MBUTTON{separator};
          Buttons^[5]  := MBUTTON{enabled, act.Pallette,  'p', 1};
          Buttons^[6]  := MBUTTON{enabled, act.Options,   'o', 1};
          Buttons^[7]  := MBUTTON{separator};
          Buttons^[8]  := MBUTTON{enabled, act.SaveConfig,      's', 1};
          Buttons^[9]  := MBUTTON{enabled, act.SaveKeyboardLayout, 'k', 6};
          Buttons^[10] := MBUTTON{enabled, act.SaveAsBatch,        'v', 3};
          Buttons^[11] := MBUTTON{enabled, act.RestoreFromBatch,   't', 4};
          Buttons^[12] := MBUTTON{separator};
          Buttons^[13] := MBUTTON{enabled, act.Quit  , 'x', 2};
        END;
       <* END *>

        size.x1 := x_pos; size.y1 := 1;
        size.x2 := x_pos; size.y2 := size.y1+HIGH(Buttons^)+2;

        hwnd := win.RegisterWindow (MenuProc, SIZE(MENU));
        ASSERT(hwnd # win.Invalid_H);

        menu := PMENU(win.GetAMPtr(hwnd));
        menu^.curr := 0;
        menu^.pos := ORD(FileMenu);
        menu^.Buttons := Buttons;
      ELSE
        menu := PMENU(win.GetAMPtr(hwnd));
        size := win.GetWindowSize(hwnd);
      END;
      RecalcSize(menu, size);

      win.SetWindowSize(hwnd, size);
      eve.AddToTail(hwnd, eve.Rise, 0);
    END;
  END;
  RETURN TRUE;
END File;


PROCEDURE Run (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  Buttons: MBUTTONS;
  menu: PMENU;
  size: crt.SZ;
BEGIN
  ASSERT(action = act.RunMenu);
  IF mode # act.mode_check THEN
    WITH MainMenu[RunMenu] DO
      IF hwnd = win.Invalid_H THEN
       <* IF DEST_K26 THEN *>
        NEW(Buttons, 12);
        Buttons^[00] := MBUTTON{ enabled, act.Run              , 'p', 1};
        Buttons^[01] := MBUTTON{ enabled, act.Into             , 'd', 5};
        Buttons^[02] := MBUTTON{ enabled, act.Over             , 'g', 5};
        Buttons^[03] := MBUTTON{ enabled, act.Skip             , 'r', 14};
        Buttons^[04] := MBUTTON{ enabled, act.Animation        , 'n', 10};
        Buttons^[05] := MBUTTON{ separator};
        Buttons^[06] := MBUTTON{ enabled, act.UptoCall         , 's', 15};
        Buttons^[07] := MBUTTON{ enabled, act.UptoRet          , 'j', 15};
        Buttons^[08] := MBUTTON{ enabled, act.UptoAddr         , 'f', 19};
        Buttons^[09] := MBUTTON{ separator};
        Buttons^[10] := MBUTTON{ enabled, act.Restart          , 'h', 1};
        Buttons^[11] := MBUTTON{ enabled, act.RestartAtAddress , 'c', 9};
       <* ELSE *>
        NEW(Buttons, 16);
        Buttons^[00] := MBUTTON{ enabled, act.Run       , 'r', 1};
        Buttons^[01] := MBUTTON{ enabled, act.Animation , 't', 11};
        Buttons^[02] := MBUTTON{ separator};
        Buttons^[03] := MBUTTON{ enabled, act.Into      , 'i', 6};
        Buttons^[04] := MBUTTON{ enabled, act.Over      , 'v', 7};
        Buttons^[05] := MBUTTON{ enabled, act.UptoRet   , 'o', 6};
        Buttons^[06] := MBUTTON{ separator};
        Buttons^[07] := MBUTTON{ enabled, act.Skip      , 'c', 8};
        Buttons^[08] := MBUTTON{ enabled, act.UptoEpilog, 'e', 8};
        Buttons^[09] := MBUTTON{ enabled, act.UptoAddr  , 'a', 11};
        Buttons^[10] := MBUTTON{ separator};
        Buttons^[11] := MBUTTON{ enabled, act.Restart   , 's', 3};
        Buttons^[12] := MBUTTON{ enabled, act.RestartAtStartup, 'u', 17};
        Buttons^[13] := MBUTTON{ enabled, act.RestartAtEntryPoint, 'p', 18};
        Buttons^[14] := MBUTTON{ separator};
        Buttons^[15] := MBUTTON{ enabled, act.JumpToMain, 'j', 1};
       <* END *>

        hwnd := win.RegisterWindow(MenuProc,SIZE(MENU));
        ASSERT(hwnd # win.Invalid_H);

        size.x1 := x_pos; size.y1 := 1;
        size.x2 := x_pos; size.y2 := size.y1+HIGH(Buttons^)+2;

        win.SetWindowSize(hwnd,size);
        menu := PMENU(win.GetAMPtr(hwnd));
        menu^.curr := 0;
        menu^.pos := ORD(RunMenu);
        menu^.Buttons := Buttons;
      ELSE
        menu := PMENU(win.GetAMPtr(hwnd));
        size := win.GetWindowSize(hwnd);
      END;

      RecalcSize(menu, size);

      win.SetWindowSize(hwnd, size);
      eve.AddToTail(hwnd, eve.Rise, 0);
    END;
  END;
  RETURN TRUE;
END Run;


PROCEDURE Breaks (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  menu: PMENU;
  Buttons: MBUTTONS;
  size: crt.SZ;
BEGIN
  ASSERT(action = act.BreaksMenu);
  IF mode # act.mode_check THEN
    WITH MainMenu[BreaksMenu] DO
      IF hwnd = win.Invalid_H THEN
       <* IF DEST_K26 THEN *>
        NEW(Buttons, 17);
        Buttons^[00] := MBUTTON{ enabled, act.Sticky,         'g', 1};
        Buttons^[01] := MBUTTON{ enabled, act.OneTime,        'h', 5};
        Buttons^[02] := MBUTTON{ enabled, act.D_Sticky,       'j', 2};
        Buttons^[03] := MBUTTON{ enabled, act.D_OneTime,      'l', 2};
        Buttons^[04] := MBUTTON{ enabled, act.ExprPoint,      'e', 4};
        Buttons^[05] := MBUTTON{ enabled, act.Counter,        'x', 9};
        Buttons^[06] := MBUTTON{ enabled, act.Watchpoint,     'y', 3};
        Buttons^[07] := MBUTTON{ enabled, act.ConditionBreak, 'c', 2};
        Buttons^[08] := MBUTTON{ separator };
        Buttons^[09] := MBUTTON{ enabled, act.Disable,   'p', 1};
        Buttons^[10] := MBUTTON{ enabled, act.Enable,    'n', 2};
        Buttons^[11] := MBUTTON{ enabled, act.Delete,    'f', 3};
        Buttons^[12] := MBUTTON{ separator };
        Buttons^[13] := MBUTTON{ enabled, act.ViewAll,   'd', 8};
        Buttons^[14] := MBUTTON{ enabled, act.DisableAll,'r', 3};
        Buttons^[15] := MBUTTON{ enabled, act.EnableAll, 't', 5};
        Buttons^[16] := MBUTTON{ enabled, act.EraseAll,  ',', 2};
       <* ELSE *>
        NEW(Buttons, 19);
        Buttons^[00] := MBUTTON{ enabled, act.Sticky,    's', 1};
        Buttons^[01] := MBUTTON{ enabled, act.OneTime,   'b', 1};
        Buttons^[02] := MBUTTON{ enabled, act.D_Sticky,  '',  0};
        Buttons^[03] := MBUTTON{ enabled, act.D_OneTime, '',  0};
        Buttons^[04] := MBUTTON{ enabled, act.ExprPoint, 'e', 1};
        Buttons^[05] := MBUTTON{ enabled, act.Counter,   'p', 1};
        Buttons^[06] := MBUTTON{ enabled, act.Watchpoint,'w', 1};
        Buttons^[07] := MBUTTON{ separator };
        Buttons^[08] := MBUTTON{ enabled, act.Access,    'a', 1};
        Buttons^[09] := MBUTTON{ enabled, act.Condition, 'c', 1};
        Buttons^[10] := MBUTTON{ separator };
        Buttons^[11] := MBUTTON{ enabled, act.Disable,   '',  0};
        Buttons^[12] := MBUTTON{ enabled, act.Enable,    '',  0};
        Buttons^[13] := MBUTTON{ enabled, act.Delete,    '',  0};
        Buttons^[14] := MBUTTON{ separator };
        Buttons^[15] := MBUTTON{ enabled, act.ViewAll,   'v', 1};
        Buttons^[16] := MBUTTON{ enabled, act.DisableAll,'',  0};
        Buttons^[17] := MBUTTON{ enabled, act.EnableAll, '',  0};
        Buttons^[18] := MBUTTON{ enabled, act.EraseAll,  '',  0};
       <* END *>

        hwnd := win.RegisterWindow(MenuProc,SIZE(MENU));
        ASSERT(hwnd # win.Invalid_H);

        size.x1 := x_pos; size.y1 := 1;
        size.x2 := x_pos; size.y2 := size.y1+HIGH(Buttons^)+2;

        win.SetWindowSize(hwnd,size);
        menu := PMENU(win.GetAMPtr(hwnd));
        menu^.curr := 0;
        menu^.pos := ORD(BreaksMenu);
        menu^.Buttons := Buttons;
      ELSE
        menu := PMENU(win.GetAMPtr(hwnd));
        size := win.GetWindowSize(hwnd);
      END;

      RecalcSize(menu, size);

      win.SetWindowSize(hwnd, size);
      eve.AddToTail(hwnd, eve.Rise, 0);
    END;
  END;
  RETURN TRUE;
END Breaks;


PROCEDURE Code (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  menu: PMENU;
  Buttons: MBUTTONS;
  size: crt.SZ;
BEGIN
  ASSERT(action = act.CodeMenu);
  IF mode # act.mode_check THEN
    WITH MainMenu[CodeMenu] DO
      IF hwnd = win.Invalid_H THEN
       <* IF DEST_K26 THEN *>
        NEW(Buttons,11);
        Buttons^[00] := MBUTTON{ enabled, act.SourceMode,   '', 1};
        Buttons^[01] := MBUTTON{ enabled, act.AssemblyMode, '', 4};
        Buttons^[02] := MBUTTON{ enabled, act.MixMode,      '', 1};
        Buttons^[03] := MBUTTON{ separator };
        Buttons^[04] := MBUTTON{ enabled, act.CallStack,    'c', 1};
        Buttons^[05] := MBUTTON{ separator };
        Buttons^[06] := MBUTTON{ enabled, act.Components,   'r', 1};
        Buttons^[07] := MBUTTON{ enabled, act.Modules,      'v', 1};
        Buttons^[08] := MBUTTON{ enabled, act.Procs,        'g', 1};
        Buttons^[09] := MBUTTON{ separator };
        Buttons^[10] := MBUTTON{ enabled, act.TableDevises, 'e', 1};
       <* ELSE *>
        NEW(Buttons, 14);
        Buttons^[00] := MBUTTON{ enabled, act.MainWindow,   'a', 2};
        Buttons^[01] := MBUTTON{ separator };
        Buttons^[02] := MBUTTON{ enabled, act.SourceMode,   's', 1};
        Buttons^[03] := MBUTTON{ enabled, act.AssemblyMode, 'a', 1};
        Buttons^[04] := MBUTTON{ enabled, act.MixMode,      'i', 2};
        Buttons^[05] := MBUTTON{ separator };
        Buttons^[06] := MBUTTON{ enabled, act.CallStack,  'c',  1};
        Buttons^[07] := MBUTTON{ separator };
        Buttons^[08] := MBUTTON{ enabled, act.Components, 'o', 2};
        Buttons^[09] := MBUTTON{ enabled, act.Modules,    'm', 1};
        Buttons^[10] := MBUTTON{ enabled, act.Procs,      'p', 1};
        Buttons^[11] := MBUTTON{ enabled, act.Publics,    'u', 2};
        Buttons^[12] := MBUTTON{ separator };
        Buttons^[13] := MBUTTON{ enabled, act.Threads,    't', 1};
       <* END *>

        hwnd := win.RegisterWindow(MenuProc,SIZE(MENU));
        ASSERT(hwnd # win.Invalid_H);

        size.x1 := x_pos; size.y1 := 1;
        size.x2 := x_pos; size.y2 := size.y1+HIGH(Buttons^)+2;

        win.SetWindowSize(hwnd,size);
        menu := PMENU(win.GetAMPtr(hwnd));
        menu^.curr := 0;
        menu^.pos := ORD(CodeMenu);
        menu^.Buttons := Buttons;
      ELSE
        menu := PMENU(win.GetAMPtr(hwnd));
        size := win.GetWindowSize(hwnd);
      END;

      RecalcSize(menu, size);

      win.SetWindowSize(hwnd, size);
      eve.AddToTail(hwnd, eve.Rise, 0);
    END;
  END;
  RETURN TRUE;
END Code;


PROCEDURE Data (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  menu: PMENU;
  Buttons: MBUTTONS;
  size: crt.SZ;
BEGIN
  ASSERT(action = act.DataMenu);
  IF mode # act.mode_check THEN
    WITH MainMenu[DataMenu] DO
      IF hwnd = win.Invalid_H THEN
       <* IF DEST_K26 THEN *>
        NEW(Buttons, 15);
        Buttons^[00] := MBUTTON{ enabled, act.Examine,    'p',  1};
        Buttons^[01] := MBUTTON{ enabled, act.Evaluate,   'd',  1};
        Buttons^[02] := MBUTTON{ separator };
        Buttons^[03] := MBUTTON{ enabled, act.ModuleVars, 'v', 12};
        Buttons^[04] := MBUTTON{ enabled, act.LocalVars,  'k', 1};
        Buttons^[05] := MBUTTON{ separator };
        Buttons^[06] := MBUTTON{ enabled, act.Dump,      'l', 1};
        Buttons^[07] := MBUTTON{ enabled, act.Registers, 'h', 1};
        Buttons^[08] := MBUTTON{ enabled, act.Stack,     'n', 11};
        Buttons^[09] := MBUTTON{ separator };
        Buttons^[10] := MBUTTON{ enabled, act.AddWatch, 'a', 1};
        Buttons^[11] := MBUTTON{ enabled, act.DelWatch, 'd', 1};
        Buttons^[12] := MBUTTON{ enabled, act.Watches,  's', 1};
        Buttons^[13] := MBUTTON{ separator };
        Buttons^[14] := MBUTTON{ enabled, act.DelAllWatches, '', 0};
       <* ELSIF DEST_XDS THEN  *>
       <* IF TARGET_OS = "WINNT" THEN *>
        NEW(Buttons, 12);
       <* ELSE *>
        NEW(Buttons, 11);
       <* END *>
        Buttons^[00] := MBUTTON{ enabled, act.Examine,    'x', 2};
        Buttons^[01] := MBUTTON{ enabled, act.Evaluate,   'e', 1};
        Buttons^[02] := MBUTTON{ separator };
        Buttons^[03] := MBUTTON{ enabled, act.GlobalVars, 'g', 1};
        Buttons^[04] := MBUTTON{ enabled, act.ModuleVars, 'v', 8};
        Buttons^[05] := MBUTTON{ enabled, act.LocalVars,  'l', 1};
        Buttons^[06] := MBUTTON{ separator };
        Buttons^[07] := MBUTTON{ enabled, act.Dump,       'm', 1};
        Buttons^[08] := MBUTTON{ enabled, act.Stack,      's', 1};
        Buttons^[09] := MBUTTON{ separator };
        Buttons^[10] := MBUTTON{ enabled, act.Registers,  'r', 1};
       <* IF TARGET_OS = "WINNT" THEN *>
        Buttons^[11] := MBUTTON{ enabled, act.FloatRegisters, 'f', 1};
       <* END *>
       <* END *>

        hwnd := win.RegisterWindow(MenuProc,SIZE(MENU));
        ASSERT(hwnd # win.Invalid_H);

        size.x1 := x_pos; size.y1 := 1;
        size.x2 := x_pos; size.y2 := size.y1+HIGH(Buttons^)+2;

        win.SetWindowSize(hwnd,size);
        menu := PMENU(win.GetAMPtr(hwnd));
        menu^.curr := 0;
        menu^.pos := ORD(DataMenu);
        menu^.Buttons := Buttons;
      ELSE
        menu := PMENU(win.GetAMPtr(hwnd));
        size := win.GetWindowSize(hwnd);
      END;

      RecalcSize(menu, size);

      win.SetWindowSize(hwnd, size);
      eve.AddToTail(hwnd, eve.Rise, 0);
    END;
  END;
  RETURN TRUE;
END Data;


<* IF DEST_XDS THEN *>

PROCEDURE Show (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  menu: PMENU;
  Buttons: MBUTTONS;
  size: crt.SZ;
BEGIN
  ASSERT(action = act.ShowMenu);
  IF mode # act.mode_check THEN
    WITH MainMenu[ShowMenu] DO
      IF hwnd = win.Invalid_H THEN
        NEW(Buttons, 9);
        Buttons^[00] := MBUTTON{ enabled, act.AddWatch,   'a', 1};
        Buttons^[01] := MBUTTON{ enabled, act.DelWatch,   'd', 1};
        Buttons^[02] := MBUTTON{ enabled, act.Watches,    'w', 6};
        Buttons^[03] := MBUTTON{ separator };
        Buttons^[04] := MBUTTON{ enabled, act.DelAllWatches, '', 0};
        Buttons^[05] := MBUTTON{ separator };
        Buttons^[06] := MBUTTON{ enabled, act.OutputDebugStrings, 'o', 7};
        Buttons^[07] := MBUTTON{ separator };
        Buttons^[08] := MBUTTON{ enabled, act.ExceptionsHistory, 'h', 12};

        hwnd := win.RegisterWindow(MenuProc,SIZE(MENU));
        ASSERT(hwnd # win.Invalid_H);

        size.x1 := x_pos; size.y1 := 1;
        size.x2 := x_pos; size.y2 := size.y1+HIGH(Buttons^)+2;

        win.SetWindowSize(hwnd,size);
        menu := PMENU(win.GetAMPtr(hwnd));
        menu^.curr := 0;
        menu^.pos := ORD(ShowMenu);
        menu^.Buttons := Buttons;
      ELSE
        menu := PMENU(win.GetAMPtr(hwnd));
        size := win.GetWindowSize(hwnd);
      END;

      RecalcSize(menu, size);

      win.SetWindowSize(hwnd, size);
      eve.AddToTail(hwnd, eve.Rise, 0);
    END;
  END;
  RETURN TRUE;
END Show;


<* IF DEFINED(mode) AND (mode="work") THEN *> -- FIXME

PROCEDURE Tools (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  menu: PMENU;
  Buttons: MBUTTONS;
  size: crt.SZ;
BEGIN
  ASSERT(action = act.ToolsMenu);
  IF mode # act.mode_check THEN
    WITH MainMenu[ToolsMenu] DO
      IF hwnd = win.Invalid_H THEN
       <* IF (TARGET_OS = "WINNT") AND DEFINED (xd_debug) AND xd_debug THEN *>
        NEW(Buttons, 4);
       <* ELSE *>
        NEW(Buttons, 3);
       <* END *>
        Buttons^[00] := MBUTTON {enabled, act.ModuleTypes       , 'm', 1};
        Buttons^[01] := MBUTTON {enabled, act.Aliases           , 'a', 1};
        Buttons^[02] := MBUTTON {enabled, act.Controls          , 'c', 1};
       <* IF (TARGET_OS = "WINNT") AND DEFINED (xd_debug) AND xd_debug THEN *>
        Buttons^[03] := MBUTTON {enabled, act.ViewTD            , 'v', 1};
       <* END *>
        hwnd := win.RegisterWindow (MenuProc, SIZE(MENU));
        ASSERT(hwnd # win.Invalid_H);
        size.x1 := x_pos; size.y1 := 1;
        size.x2 := x_pos; size.y2 := size.y1+HIGH(Buttons^)+2;
        win.SetWindowSize (hwnd, size);
        menu := PMENU(win.GetAMPtr(hwnd));
        menu^.curr := 0;
        menu^.pos := ORD(ToolsMenu);
        menu^.Buttons := Buttons;
      ELSE
        menu := PMENU(win.GetAMPtr(hwnd));
        size := win.GetWindowSize(hwnd);
      END;
      RecalcSize(menu, size);
      win.SetWindowSize(hwnd, size);
      eve.AddToTail(hwnd, eve.Rise, 0);
    END;
  END;
  RETURN TRUE;
END Tools;

<* END *>

<* END *>


PROCEDURE Search (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  menu: PMENU;
  Buttons: MBUTTONS;
  size: crt.SZ;
BEGIN
  ASSERT(action = act.SearchMenu);
  IF mode # act.mode_check THEN
    WITH MainMenu[SearchMenu] DO
      IF hwnd = win.Invalid_H THEN
        NEW(Buttons, 10);
       <* IF DEST_K26 THEN *>
        Buttons^[00] := MBUTTON{ enabled, act.Find,     'n', 8};
        Buttons^[01] := MBUTTON{ enabled, act.FindPrev, 'h', 8};
        Buttons^[02] := MBUTTON{ enabled, act.FindNext, 'k', 8};
        Buttons^[03] := MBUTTON{ separator };
        Buttons^[04] := MBUTTON{ enabled, act.NextProc, 'd', 14};
        Buttons^[05] := MBUTTON{ enabled, act.PrevProc, 'y', 14};
        Buttons^[06] := MBUTTON{ separator };
        Buttons^[07] := MBUTTON{ enabled, act.GotoLine, 'c', 1};
        Buttons^[08] := MBUTTON{ enabled, act.GotoExec, 'b', 4};
        Buttons^[09] := MBUTTON{ enabled, act.GotoAddr, 'f', 4};
       <* ELSE *>
        Buttons^[00] := MBUTTON{ enabled, act.Find,     'f', 1};
        Buttons^[01] := MBUTTON{ enabled, act.FindPrev, '',  0};
        Buttons^[02] := MBUTTON{ enabled, act.FindNext, '',  0};
        Buttons^[03] := MBUTTON{ separator };
        Buttons^[04] := MBUTTON{ enabled, act.NextProc, 'n', 1};
        Buttons^[05] := MBUTTON{ enabled, act.PrevProc, 'p', 1};
        Buttons^[06] := MBUTTON{ separator };
        Buttons^[07] := MBUTTON{ enabled, act.GotoLine, 'g', 1};
        Buttons^[08] := MBUTTON{ enabled, act.GotoExec, 'h', 1};
        Buttons^[09] := MBUTTON{ enabled, act.GotoAddr, 'a', 5};
       <* END *>

        hwnd := win.RegisterWindow(MenuProc, SIZE(MENU));
        ASSERT(hwnd # win.Invalid_H);

        size.x1 := x_pos; size.y1 := 1;
        size.x2 := x_pos; size.y2 := size.y1+HIGH(Buttons^)+2;

        win.SetWindowSize(hwnd,size);
        menu := PMENU(win.GetAMPtr(hwnd));
        menu^.curr := 0;
        menu^.pos := ORD(SearchMenu);
        menu^.Buttons := Buttons;
      ELSE
        menu := PMENU(win.GetAMPtr(hwnd));
        size := win.GetWindowSize(hwnd);
      END;

      RecalcSize(menu, size);

      win.SetWindowSize(hwnd, size);
      eve.AddToTail(hwnd, eve.Rise, 0);
    END;
  END;
  RETURN TRUE;
END Search;


PROCEDURE Windows (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.WindowsMenu);
  IF mode # act.mode_check THEN
    win.InitWindowsList (MainMenu[WindowsMenu].x_pos);
  END;
  RETURN TRUE;
END Windows;


PROCEDURE Help (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  menu: PMENU;
  Buttons: MBUTTONS;
  size: crt.SZ;
BEGIN
  ASSERT(action = act.HelpMenu);
  IF mode # act.mode_check THEN
    WITH MainMenu[HelpMenu] DO
      IF hwnd = win.Invalid_H THEN
        size.x1 := x_pos;
        size.y1 := 1;
        size.x2 := x_pos;
      <* IF DEST_K26 THEN *>
        NEW(Buttons, 4);
        size.y2 := size.y1+HIGH(Buttons^)+2;
        Buttons^[0] := MBUTTON{enabled, act.Help, 'g', 1};
        Buttons^[1] := MBUTTON{enabled, act.ContextMenu, '', 0};
        Buttons^[2] := MBUTTON{separator};
        Buttons^[3] := MBUTTON{enabled, act.About, '', 0};
       <* ELSIF DEST_XDS THEN *>
        NEW(Buttons, 3);
        size.y2 := size.y1+HIGH(Buttons^)+2;
        Buttons^[0] := MBUTTON{enabled, act.Help, 'h', 1};
        Buttons^[1] := MBUTTON{separator};
        Buttons^[2] := MBUTTON{enabled, act.About, 'a', 1};
        DEC(size.x1, 3);
       <* END *>

        hwnd := win.RegisterWindow(MenuProc, SIZE(MENU));
        ASSERT(hwnd # win.Invalid_H);

        menu := PMENU(win.GetAMPtr(hwnd));
        menu^.curr := 0;
        menu^.pos := ORD(HelpMenu);
        menu^.Buttons := Buttons;
      ELSE
        menu := PMENU(win.GetAMPtr(hwnd));
        size := win.GetWindowSize(hwnd);
      END;
      RecalcSize(menu, size);
      win.SetWindowSize(hwnd, size);
      eve.AddToTail(hwnd, eve.Rise, 0);
    END;
  END;
  RETURN TRUE;
END Help;



<* IF SCHERN_K26 THEN *>
(*                                          CHERN                      *)
PROCEDURE prf_Break(action: act.ACTION;check : BOOLEAN): BOOLEAN;
VAR
  menu: PMENU;
  Buttons: MBUTTONS;
  size: crt.SZ;
BEGIN
  ASSERT(action = act.prf_1);
 IF mode # act.mode_check THEN
  IF ProfBreakMenu = win.Invalid_H THEN
    NEW(Buttons, 10);

<* IF DEST_K26 THEN *>

    Buttons^[00] := MBUTTON{ enabled, act.prf_br1, 'n', 1};
    Buttons^[01] := MBUTTON{ enabled, act.prf_br2, 'k', 1};
    Buttons^[02] := MBUTTON{ enabled, act.prf_br3, 'h', 1};
    Buttons^[03] := MBUTTON{ enabled, act.prf_br4, 'd', 1};
    Buttons^[04] := MBUTTON{ enabled, act.prf_br5, 'd', 1};
    Buttons^[05] := MBUTTON{ separator };
    Buttons^[06] := MBUTTON{ enabled, act.prf_br6, 'y', 1};
    Buttons^[07] := MBUTTON{ enabled, act.prf_br7, 'y', 1};
    Buttons^[08] := MBUTTON{ enabled, act.prf_br8, 'y', 1};
    Buttons^[09] := MBUTTON{ enabled, act.prf_br9, 'c', 1};

<* ELSE *>
(*
    Buttons^[00] := MBUTTON{ enabled, act.Find,     'f', 1};
    Buttons^[01] := MBUTTON{ enabled, act.FindNext, '',  0};
    Buttons^[02] := MBUTTON{ enabled, act.FindPrev, '',  0};
    Buttons^[03] := MBUTTON{ separator };
    Buttons^[04] := MBUTTON{ enabled, act.NextProc, 'n', 1};
    Buttons^[05] := MBUTTON{ enabled, act.PrevProc, 'p', 1};
    Buttons^[06] := MBUTTON{ separator };
    Buttons^[07] := MBUTTON{ enabled, act.GotoLine, 'g', 1};
    Buttons^[08] := MBUTTON{ enabled, act.GotoExec, 'e', 5};
    Buttons^[09] := MBUTTON{ enabled, act.GotoAddr, 'a', 5};
*)
<* END *>

    ProfBreakMenu := win.RegisterWindow(MenuProc, SIZE(MENU));
    ASSERT(ProfBreakMenu # win.Invalid_H);

    size.x1 := MainMenu[7].x_pos-10; size.y1 := 1;
    size.x2 := MainMenu[7].x_pos-10; size.y2 := size.y1+HIGH(Buttons^)+2;

    win.SetWindowSize(ProfBreakMenu,size);
    menu := PMENU(win.GetAMPtr(ProfBreakMenu));
    menu^.curr := 0;
    menu^.pos := 7;
    menu^.Buttons := Buttons;
  ELSE
    menu := PMENU(win.GetAMPtr(ProfBreakMenu));
    size := win.GetWindowSize(ProfBreakMenu);
  END;

  RecalcSize(menu, size);

  win.SetWindowSize(ProfBreakMenu, size);
  eve.AddToTail(ProfBreakMenu, eve.Rise, 0);
 END;
 RETURN TRUE;
END prf_Break;

PROCEDURE prf_Blok(action: act.ACTION; check : BOOLEAN): BOOLEAN;
VAR
  menu: PMENU;
  Buttons: MBUTTONS;
  size: crt.SZ;
BEGIN
  ASSERT(action = act.prf_2);
 IF mode # act.mode_check THEN
  IF ProfBlokMenu = win.Invalid_H THEN
<* IF DEST_K26 THEN *>
    NEW(Buttons, 8);

    Buttons^[00] := MBUTTON{ enabled, act.prf_blok1, 'n', 1};
    Buttons^[01] := MBUTTON{ enabled, act.prf_blok2, 'k', 1};
    Buttons^[02] := MBUTTON{ enabled, act.prf_blok3, 'h', 1};
    Buttons^[03] := MBUTTON{ separator };
    Buttons^[04] := MBUTTON{ enabled, act.prf_blok4, 'y', 1};
    Buttons^[05] := MBUTTON{ enabled, act.prf_blok5, 'y', 1};
    Buttons^[06] := MBUTTON{ enabled, act.prf_blok6, 'y', 1};
    Buttons^[07] := MBUTTON{ enabled, act.prf_blok7, 'c', 1};

<* ELSE *>
(*
    Buttons^[00] := MBUTTON{ enabled, act.Find,     'f', 1};
    Buttons^[01] := MBUTTON{ enabled, act.FindNext, '',  0};
    Buttons^[02] := MBUTTON{ enabled, act.FindPrev, '',  0};
    Buttons^[03] := MBUTTON{ separator };
    Buttons^[04] := MBUTTON{ enabled, act.NextProc, 'n', 1};
    Buttons^[05] := MBUTTON{ enabled, act.PrevProc, 'p', 1};
    Buttons^[06] := MBUTTON{ separator };
    Buttons^[07] := MBUTTON{ enabled, act.GotoLine, 'g', 1};
    Buttons^[08] := MBUTTON{ enabled, act.GotoExec, 'e', 5};
    Buttons^[09] := MBUTTON{ enabled, act.GotoAddr, 'a', 5};
*)
<* END *>

    ProfBlokMenu := win.RegisterWindow(MenuProc, SIZE(MENU));
    ASSERT(ProfBlokMenu # win.Invalid_H);

    size.x1 := MainMenu[7].x_pos; size.y1 := 1;
    size.x2 := MainMenu[7].x_pos; size.y2 := size.y1+HIGH(Buttons^)+2;

    win.SetWindowSize(ProfBlokMenu,size);
    menu := PMENU(win.GetAMPtr(ProfBlokMenu));
    menu^.curr := 0;
    menu^.pos := 7;
    menu^.Buttons := Buttons;
  ELSE
    menu := PMENU(win.GetAMPtr(ProfBlokMenu));
    size := win.GetWindowSize(ProfBlokMenu);
  END;

  RecalcSize(menu, size);

  win.SetWindowSize(ProfBlokMenu, size);
  eve.AddToTail(ProfBlokMenu, eve.Rise, 0);
 END;
 RETURN TRUE;
END prf_Blok;

PROCEDURE prof_p(action: act.ACTION; check : BOOLEAN): BOOLEAN;
VAR
  menu: PMENU;
  Buttons: MBUTTONS;
  size: crt.SZ;
BEGIN
  ASSERT(action = act.ProfMenu);
 IF mode # act.mode_check THEN
  IF ProfMenu = win.Invalid_H THEN
<* IF DEST_K26 THEN  *>
    NEW(Buttons, 11);

    Buttons^[00] := MBUTTON{ enabled, act.prf_1, 'n', 1};
    Buttons^[01] := MBUTTON{ separator };
    Buttons^[02] := MBUTTON{ enabled, act.prf_2, 'k', 1};
    Buttons^[03] := MBUTTON{ separator };
    Buttons^[04] := MBUTTON{ enabled, act.prf_3, 'h', 1};
    Buttons^[05] := MBUTTON{ separator };
    Buttons^[06] := MBUTTON{ enabled, act.prf_4, 'd', 1};
    Buttons^[07] := MBUTTON{ separator };
    Buttons^[08] := MBUTTON{ enabled, act.prf_5, 'y', 1};
    Buttons^[09] := MBUTTON{ enabled, act.prf_6, 'c', 1};
    Buttons^[10] := MBUTTON{ enabled, act.prf_7, 'b', 1};

<* ELSE *>
(*
    Buttons^[00] := MBUTTON{ enabled, act.Find,     'f', 1};
    Buttons^[01] := MBUTTON{ enabled, act.FindNext, '',  0};
    Buttons^[02] := MBUTTON{ enabled, act.FindPrev, '',  0};
    Buttons^[03] := MBUTTON{ separator };
    Buttons^[04] := MBUTTON{ enabled, act.NextProc, 'n', 1};
    Buttons^[05] := MBUTTON{ enabled, act.PrevProc, 'p', 1};
    Buttons^[06] := MBUTTON{ separator };
    Buttons^[07] := MBUTTON{ enabled, act.GotoLine, 'g', 1};
    Buttons^[08] := MBUTTON{ enabled, act.GotoExec, 'e', 5};
    Buttons^[09] := MBUTTON{ enabled, act.GotoAddr, 'a', 5};
*)
<* END *>

    ProfMenu := win.RegisterWindow(MenuProc, SIZE(MENU));
    ASSERT(ProfMenu # win.Invalid_H);

    size.x1 := MainMenu[7].x_pos; size.y1 := 1;
    size.x2 := MainMenu[7].x_pos; size.y2 := size.y1+HIGH(Buttons^)+2;

    win.SetWindowSize(ProfMenu,size);
    menu := PMENU(win.GetAMPtr(ProfMenu));
    menu^.curr := 0;
    menu^.pos := 7;
    menu^.Buttons := Buttons;
  ELSE
    menu := PMENU(win.GetAMPtr(ProfMenu));
    size := win.GetWindowSize(ProfMenu);
  END;

  RecalcSize(menu, size);

  win.SetWindowSize(ProfMenu, size);
  eve.AddToTail(ProfMenu, eve.Rise, 0);
 END;
 RETURN TRUE;
END prof_p;

PROCEDURE prv_Init(action: act.ACTION; check : BOOLEAN): BOOLEAN;
BEGIN
  ASSERT(action = act.prf_3);
 IF mode # act.mode_check THEN
  prv.Init;
 END;
 RETURN TRUE;
END prv_Init;

(*                         CHERN                              end   *)
<* END *>



<* IF DEST_K26 THEN *>

CONST
  _MainMenu = MAIN_MENU{ { win.Invalid_H, 0, ' Файл '     , 1, key.AltA, ORD('a'), act.FileMenu     },
                         { win.Invalid_H, 0, ' Запуск '   , 1, key.AltP, ORD('p'), act.RunMenu      },
                         { win.Invalid_H, 0, ' Остановы ' , 1, key.AltJ, ORD('j'), act.BreaksMenu   },
                         { win.Invalid_H, 0, ' Код '      , 1, key.AltR, ORD('r'), act.CodeMenu    },
                         { win.Invalid_H, 0, ' Данные '   , 1, key.AltL, ORD('l'), act.DataMenu     },
                         { win.Invalid_H, 0, ' Поиск '    , 1, key.AltG, ORD('g'), act.SearchMenu   },
                         { win.Invalid_H, 0, ' Окна '     , 3, key.AltY, ORD('y'), act.WindowsMenu  },
                        <* IF SCHERN_K26 THEN  *>
                         { 0, ' Профилирование'  , 2, key.AltF, ORD('f'), act.ProfMenu     },     (* CHERN *)
                        <* END *>
                         { win.Invalid_H, 0, ' Помошь '   , 3, key.AltV, ORD('v'), act.HelpMenu     }
                        };

<* ELSIF DEST_XDS THEN *>

CONST
  _MainMenu = MAIN_MENU{ { win.Invalid_H, 0, ' File '    , 1, key.AltF, ORD('f'), act.FileMenu    },
                         { win.Invalid_H, 0, ' Run '     , 1, key.AltR, ORD('r'), act.RunMenu     },
                         { win.Invalid_H, 0, ' Breaks '  , 1, key.AltB, ORD('b'), act.BreaksMenu  },
                         { win.Invalid_H, 0, ' Code '    , 1, key.AltC, ORD('c'), act.CodeMenu    },
                         { win.Invalid_H, 0, ' Data '    , 1, key.AltD, ORD('d'), act.DataMenu    },
                         { win.Invalid_H, 0, ' Show '    , 3, key.AltO, ORD('o'), act.ShowMenu    },
                         { win.Invalid_H, 0, ' Search '  , 1, key.AltS, ORD('s'), act.SearchMenu  },
                        <* IF DEFINED(mode) AND (mode="work") THEN *> -- FIXME
                         { win.Invalid_H, 0, ' Tools '   , 1, key.AltT, ORD('t'), act.ToolsMenu   },
                        <* END *>
                         { win.Invalid_H, 0, ' Windows ' , 1, key.AltW, ORD('i'), act.WindowsMenu },
                         { win.Invalid_H, 0, ' Help '    , 1, key.AltH, ORD('h'), act.HelpMenu    }
                        };
<* END *>


CONST
  DebuggerStateWorking = 0;         -- must be zero already
  DebuggerStateWaiting = 040574H;   -- must be defferent from any other indication

  DebuggerStateWaitingMessage = " " + 17C + " ";


PROCEDURE DebuggerStateIndicator (waiting: BOOLEAN);
BEGIN
  IF waiting THEN
    eve.AddToTail (dv.MainPulldown, eve.Redraw, DebuggerStateWaiting);
  ELSE
    eve.AddToTail (dv.MainPulldown, eve.Redraw, DebuggerStateWorking);
  END;
  eve.Flush;
END DebuggerStateIndicator;


PROCEDURE PulldownProc(hwnd: win.HWND; msg: eve.MSG);
VAR
  atr  : crt.ATTR;
  p    : PPULLDOWN;
  x,y  : CARDINAL;
  _p   : CARDINAL;
  size : crt.SZ;
  i    : PullDownMenus;

CONST
  CurrentVisible = 2;

BEGIN
  p := win.GetAMPtr(hwnd);
  CASE msg.ID OF
  | eve.Defocus:
    IF p^.opened # win.Invalid_H THEN
      eve.AddToTail(p^.opened, eve.Hide, 0);
      p^.opened := win.Invalid_H;
    END;

  | eve.Mouse_Dbl, eve.Mouse_Pressed, eve.Mouse_Moved:
    ASSERT(win.GetMouse(msg, x, y));
    _p := MAX(CARDINAL);
    FOR i := MIN(PullDownMenus) TO MAX(PullDownMenus) DO
      WITH MainMenu[i] DO
        IF (x>=x_pos) & (x<=x_pos+LENGTH(name)) THEN
          _p := ORD(i);
        END;
      END;
    END;
    WITH p^ DO
      IF (_p # MAX(CARDINAL)) AND ((_p # ORD(curr)) OR (opened = win.Invalid_H)) THEN
        curr := VAL(PullDownMenus, _p);
        IF (opened # win.Invalid_H) THEN
          eve.AddToTail(opened, eve.Hide, 0);
          opened := win.Invalid_H;
        END;
        eve.AddToTail(hwnd, eve.Redraw, CurrentVisible);
        act.ExecuteAction (MainMenu[curr].action, act.mode_loud);
      END;
    END;

  | eve.Rise:
    IF win.ActiveWindow # hwnd THEN
      beforePulldown := win.ActiveWindow;
    END;
    std.DefaultProc(hwnd, msg);

  | eve.Paint, eve.Redraw:
    crt.FillWindow(hwnd,' ',crt.Pulldown[crt.Pull_Background]);
    x := 1;
    FOR i := MIN(PullDownMenus) TO MAX(PullDownMenus) DO
      crt.SetPos(x, 0);
      IF ((hwnd = win.ActiveWindow) OR (msg.par=2)) AND (p^.curr = i) THEN
        atr := crt.Pulldown[crt.Pull_CurrentButton];
      ELSE
       atr := crt.Pulldown[crt.Pull_Button];
      END;
      crt.WrStr(hwnd, MainMenu[i].name, atr);
      IF ((hwnd = win.ActiveWindow) OR (msg.par=2)) AND (p^.curr = i) THEN
        atr := crt.Pulldown[crt.Pull_CurrentLite];
      ELSE
        atr := crt.Pulldown[crt.Pull_Lite];
      END;
      crt.SetPos(x+MainMenu[i].pos, 0);
      crt.WrChar(hwnd, MainMenu[i].name[MainMenu[i].pos], atr);
      INC(x, LENGTH(MainMenu[i].name));
    END;
    size := win.GetWindowSize(hwnd);
    IF msg.par = DebuggerStateWaiting THEN
      atr := crt.Attr (crt.Yellow, crt.LightRed);
      crt.SetPos (size.x2+1-LENGTH(DebuggerStateWaitingMessage), 0);
      crt.WrStr(hwnd, DebuggerStateWaitingMessage, atr);
    END;
    IF msg.ID =eve.Redraw THEN
      crt.UpdateRect(size);
    END;

  | eve.QueryKbHit:
    CASE msg.par OF
    | key.Right:
      act.ConfirmQueryByCond(p^.curr # MAX(PullDownMenus));
    | key.Left:
      act.ConfirmQueryByCond(p^.curr # MIN(PullDownMenus));
    | key.Enter, key.Down:
      act.ConfirmQueryByCond(act.QueryAction(MainMenu[VAL(PullDownMenus,p^.curr)].action));
    | key.Esc:
      act.ConfirmQuery;
    ELSE
    END;

  | eve.KbHit:
    CASE msg.par OF
    | key.Right, key.Left :
      WITH p^ DO
        IF msg.par = key.Right THEN
          IF curr < MAX(PullDownMenus) THEN
            INC(curr);
          END;
        ELSE
          IF curr > MIN(PullDownMenus) THEN
            DEC(curr);
          END;
        END;
      END;
      eve.AddToTail(hwnd, eve.Redraw,0);
    | key.Enter, key.Down:
      act.ExecuteAction (MainMenu[VAL(PullDownMenus,p^.curr)].action, act.mode_loud);
      eve.AddToTail(hwnd, eve.Redraw, 2);
    | key.Esc:
      win.ActiveToTail;
      eve.AddToTail(win.ActiveWindow, eve.Redraw, 0);
      eve.AddToTail(hwnd,             eve.Redraw, 0);
    ELSE
      FOR i := MIN(PullDownMenus) TO MAX(PullDownMenus) DO
        IF (msg.par = MainMenu[i].hot) OR
           (msg.par = MainMenu[i].l_hot) OR
           (msg.par = ORD(CAP(CHR(MainMenu[i].l_hot))))
        THEN
          p^.curr := i;
          eve.AddToTail(hwnd, eve.Redraw, 0);
          eve.AddToTail(hwnd, eve.KbHit, key.Enter);
          RETURN;
        END;
      END;
      std.DefaultProc(hwnd,msg);
    END;
  ELSE
    std.DefaultProc(hwnd,msg);
  END;
END PulldownProc;


PROCEDURE IniMainPulldown (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  size: crt.SZ;
  p   : PPULLDOWN;
  x   : CARDINAL;
  i   : PullDownMenus;
BEGIN
  ASSERT(action = act.Pulldown);
  IF mode # act.mode_check THEN
  IF dv.MainPulldown = win.Invalid_H THEN

      x := 1;
      FOR i := MIN(PullDownMenus) TO MAX(PullDownMenus) DO
        MainMenu[i].x_pos := x;
        INC(x, LENGTH(MainMenu[i].name));
        act.IniShortCut(MainMenu[i].hot, MainMenu[i].action);
      END;

      dv.MainPulldown := win.RegisterWindow(PulldownProc, SIZE(PULLDOWN));
      ASSERT(dv.MainPulldown # win.Invalid_H);
      win.ExcludeFromList(dv.MainPulldown);

      size.x1 := 0;
      size.y1 := 0;
      size.x2 := crt.Xmax-1;
      size.y2 := 0;
      win.SetWindowSize(dv.MainPulldown, size);

      p := win.GetAMPtr(dv.MainPulldown);
      p^.curr := MIN(PullDownMenus);
      p^.opened := win.Invalid_H;
      win.SetHeaderByStr(dv.MainPulldown, 'Pulldown');
    END;
    eve.AddToHead(dv.MainPulldown, eve.Rise, 0);
  END;
  RETURN TRUE;
END IniMainPulldown;


PROCEDURE SkipMenus(): win.HWND;
VAR
  w: win.HWND;
BEGIN
  w := win.ActiveWindow;
  WHILE (w = dv.MainPulldown) OR (win.GetHandler(w) = MenuProc) OR win.IsModal(w) DO
    w := win.GetPrevious(w);
  END;
  RETURN w;
END SkipMenus;


BEGIN
  MainMenu := _MainMenu;
  beforePulldown := win.Invalid_H;

  act.IniAction(act.Pulldown, IniMainPulldown);
  act.IniAction(MainMenu[FileMenu].action    , File);
  act.IniAction(MainMenu[RunMenu].action     , Run);
  act.IniAction(MainMenu[BreaksMenu].action  , Breaks);
  act.IniAction(MainMenu[CodeMenu].action    , Code);
  act.IniAction(MainMenu[DataMenu].action    , Data);
  act.IniAction(MainMenu[SearchMenu].action  , Search);
  act.IniAction(MainMenu[WindowsMenu].action , Windows);
  act.IniAction(MainMenu[HelpMenu].action    , Help);

 <* IF DEST_XDS THEN *>
  act.IniAction(MainMenu[ShowMenu].action    , Show);
 <* IF DEFINED(mode) AND (mode="work") THEN *> -- FIXME
  act.IniAction(MainMenu[ToolsMenu].action   , Tools);
 <* END *>
 <* END *>

 <* IF SCHERN_K26 THEN   *>
  ProfMenu      := win.Invalid_H;                                  (* CHERN*)
  ProfBreakMenu := win.Invalid_H;                                  (* CHERN*)
  ProfBlokMenu  := win.Invalid_H;                                  (* CHERN*)
(*                                CHERN      *)
  act.IniAction(act.prf_1, prf_Break);
  act.IniAction(act.prf_2, prf_Blok);
  act.IniAction(act.prf_4, tpr.Init_Time_proc);
  act.IniAction(act.prf_3, prv_Init);
  act.IniAction(act.prf_5, wsv.ActSaveProf);
  act.IniAction(act.prf_6, wsv.ActReadProf);
  act.IniAction(act.prf_7, prf.Deactivate);
--   ;
  act.IniAction(act.prf_br1, lpr.MarkSt);
  act.IniAction(act.prf_br2, lpr.UnMarkSt);
  act.IniAction(act.prf_br3, lpr.AllSt);
  act.IniAction(act.prf_br4, lpr.AllProcMod);
  act.IniAction(act.prf_br5, lpr.UnMarkAll);
  act.IniAction(act.prf_br6, lpr.GetFirstBreak);
  act.IniAction(act.prf_br7, lpr.GetNextBreak);
  act.IniAction(act.prf_br8, lpr.GetPrevBreak);
  act.IniAction(act.prf_br9, wbr.ShowCountExec);
--
  act.IniAction(act.prf_blok1, lpr.MarkTimeBl);
  act.IniAction(act.prf_blok2, lpr.MarkTimeBl);
  act.IniAction(act.prf_blok3, lpr.UnMarkTimeBl);
  act.IniAction(act.prf_blok4, lpr.firstTimeBl);
  act.IniAction(act.prf_blok5, lpr.nextTimeBl);
  act.IniAction(act.prf_blok6, lpr.predTimeBl);
  act.IniAction(act.prf_blok7, wbr.ShowCountTime);
(*                              END -  CHERN      *)
  act.IniAction(MainMenu[7].action, prof_p);
 <* END *>
END Dlg_Menu.

