<* Storage+ *>
IMPLEMENTATION MODULE DlgCfg;

IMPORT fmt := FormStr;
IMPORT icc := IOConsts;
IMPORT ioc := IOChan;
IMPORT seq := SeqFile;
IMPORT sys := SYSTEM;
IMPORT tio := TextIO;
IMPORT exc := EXCEPTIONS;
IMPORT arg := ProgEnv;
IMPORT ws  := WholeStr;

IMPORT mes := MsgNo;
IMPORT xs  := xStr;
IMPORT opt := Options;
IMPORT pro := Protocol;
IMPORT red := RedFile;
IMPORT fil := File;

IMPORT exp := Expr;
IMPORT lst := Lists;

IMPORT crt := CRT;
IMPORT key := Keys;

IMPORT eve := DlgEvent;
IMPORT mod := DlgMods;
IMPORT win := Dlg_Win;
IMPORT act := Dlg_Acts;
IMPORT std := Dlg_Std;
IMPORT dv  := Dlg_Vars;
IMPORT dmm := Dlg_Mem;
IMPORT brw := DlgBrows;
IMPORT dw  := DlgWatch;
IMPORT dex := Dlg_Exec;
IMPORT stk := CallStk;

IMPORT dt  := DI_Types;

IMPORT kex := KrnExec;

IMPORT xi  := xdRTS;

<* IF DEST_K26 THEN *>
IMPORT int := IntVMain;
<* END *>

<* IF DEST_XDS THEN *>
IMPORT Translit;
<* END *>

TYPE
  OPTION_ELEM = ( Traced_Run_Delay
                , HilighCode
                , HilighCall
                , FullDisasm
                , DumpDisplay
                , Save_Opt_File
                , WholeHex
                , KbdFile
                , ShowAllModules
                , WarningBell
                , UseSingleStructWindow
                , DumpType
                , ShowModuleWithoutSource
                , DisplayDereferencePointer
               <* IF DEST_K26 THEN *>
                , TraceRegisters
                , IntStateSystem
                , IntStateTest
                , IntStateStart
               <* ELSIF DEST_XDS THEN *>
                , StopOnHardwareExceptionFirstChance
                , StopOnSoftwareException
                , SSS_Delay
                , Skip_Disasm
                , StripPathFromFullName
                , StripPathFromPartialName
                , AutoDetectActualType
                , FrameImageSingle
                , FrameImageDouble
                , FrameImageMove
                , TransliterateFromTo
               <* END *>
                , MergeEqualTypes
                );

  OPTION_TYPE = ( WholeEq, StrEq, BoolEq);

  -- Action() всегда работает с *.new !!!
  OPTION_ACTION = PROCEDURE (): BOOLEAN;

  OPTION = RECORD
             Name : std.MESSAGE;
             NameR: std.MESSAGE;
             CASE opt: OPTION_TYPE OF
             | WholeEq:
               var    : POINTER TO CARDINAL;
               var_def: CARDINAL;
               var_new: std.MESSAGE;
             | BoolEq:
               cond    : std.PBOOLEAN;
               cond_def: BOOLEAN;
               cond_new: BOOLEAN;
             | StrEq:
               line: xs.txt_ptr;
               line_len: CARDINAL;
               line_def: std.MESSAGE;
               line_new: std.MESSAGE;
             END;
             display: BOOLEAN;
             -- Action() всегда работает с *.new !!!
             Action : OPTION_ACTION;
           END;

  OPTIONS_SET = ARRAY OPTION_ELEM OF OPTION;

  OPTIONS = POINTER TO OPTIONS_SET;

  OPTION2LINE = ARRAY OPTION_ELEM OF CARDINAL;



VAR
  CurrentOptions: OPTIONS; -- Текущие значения опций

  Shown_Options    : CARDINAL;
  Shown_Options_Len: CARDINAL;
  source: exc.ExceptionSource;


CONST
  CfgFileExt = 'CFG';
  KbdFileExt = 'KBD';
  SesFileExt = 'SES';





(* Сохранить настройку клавиатуры. *)

PROCEDURE SaveKbd (filename-: ARRAY OF CHAR);
VAR
  f  : ioc.ChanId;
  res: seq.OpenResults;
  i  : CARDINAL;

  act1, act2: act.ACTION;
  key_prim  : CARDINAL;
  key_name  : ARRAY [0..16] OF CHAR;
  buf, pname: ARRAY [0..1023] OF CHAR;

BEGIN
  IF filename = '' THEN RETURN; END;
  COPY(filename, buf);
  fil.ChangeExtension(buf, KbdFileExt);
  red.Write(buf, pname);
  seq.OpenWrite(f, pname, seq.write + seq.text + seq.old, res);
  IF res # seq.opened THEN RETURN; END;
  seq.Rewrite(f);
  tio.WriteString(f, '[ Actions and keys ]');
  tio.WriteLn(f);
  FOR act1 := act.ACTION(ORD(MIN(act.ACTION))+2) TO MAX(act.ACTION) DO
    fmt.print(buf, '%-32s = ', act.ActionName[act1]);
    tio.WriteString(f, buf);
    key_prim := act.GetPrimaryShortCut(act1);
    IF key_prim # 0 THEN
      ASSERT(key.GetKeyName(key_prim, key_name));
      tio.WriteString(f, key_name);
      FOR i := 0 TO 0FFH DO
        IF act.GetActionByKey(i, act2) THEN
          IF (i # key_prim) AND (act1 = act2) THEN
            ASSERT(key.GetKeyName(i, key_name));
            fmt.print(buf, ' %s', key_name);
            tio.WriteString(f, buf);
          END;
        END;
      END;
      FOR i := 0 TO 0FFH DO
        IF act.GetActionByKey(i * 100H, act2) THEN
          IF (i * 100H # key_prim) AND (act1 = act2) THEN
            ASSERT(key.GetKeyName(i * 100H, key_name));
            fmt.print(buf, ' %s', key_name);
            tio.WriteString(f, buf);
          END;
        END;
      END;
    END;
    tio.WriteLn(f);
  END;
  seq.Close(f);
END SaveKbd;


PROCEDURE Raise (condition: BOOLEAN; line: CARDINAL);
BEGIN
  IF NOT condition THEN
    exc.RAISE(source, line, '');
  END;
END Raise;


(* Загрузить настройку клавиатуры. *)
PROCEDURE LoadKbd (name-: ARRAY OF CHAR): BOOLEAN;
VAR
  f   : ioc.ChanId;
  res : seq.OpenResults;
  Line: CARDINAL;

  action  : act.ACTION;
  key_code: key.KEY;
  buf     ,
  buf1    ,
  buf2    ,
  buf3    ,
  filename: xs.String;

BEGIN
  IF name = '' THEN RETURN TRUE; END;
  COPY(name, buf);
  fil.ChangeExtension(buf, KbdFileExt);
  IF red.Read(buf, filename) # red.Ok THEN
    arg.ProgramName(filename);
    fil.SplitPath (filename, buf1, buf2, buf3);
    fil.ExtractFileName (buf, buf3);
    IF buf1[0] # '\' THEN xs.Insert ('\', 0, buf); END;
    fmt.print (filename, "%s%s%s", buf1, buf2, buf3);
  END;
  seq.OpenRead(f, filename, seq.read + seq.old + seq.text, res);
  IF res # seq.opened THEN RETURN FALSE; END;
  tio.ReadString(f, buf);
  Raise (buf = '[ Actions and keys ]', 1);
  tio.SkipLine(f);
  Line := 2;
  LOOP
    tio.ReadToken(f, buf);
    IF (ioc.ReadResult(f) # icc.allRight) THEN EXIT; END;
    buf2 := '';
    REPEAT
      xs.Append(buf, buf2);
      xs.Append(' ', buf2);
      tio.ReadToken(f, buf);
      Raise (ioc.ReadResult(f) = icc.allRight, Line);
    UNTIL buf = '=';
    buf2[LENGTH(buf2)-1] := 0C;
    Raise (act.GetActionByName(buf2, action), Line);
    act.DeleteLinksForAction (action);
    LOOP
      tio.ReadToken(f,buf);
      IF (ioc.ReadResult(f) # icc.allRight) THEN EXIT; END;
      Raise (key.GetKeyByName(buf, key_code), Line);
      act.IniShortCut(key_code, action);
    END;
    tio.SkipLine(f);
    INC(Line);
  END;
  seq.Close(f);
  RETURN TRUE;
EXCEPT
  IF exc.IsCurrentSource(source) THEN
    std.ErrorNo(mes.ErrorInKbdFile, filename, exc.CurrentNumber(source));
    seq.Close(f);
    RETURN FALSE;
  END;
END LoadKbd;


TYPE
  BOOLEANNAME = ARRAY BOOLEAN OF ARRAY [0..7] OF CHAR;

CONST
  BooleanName = BOOLEANNAME { "No", "Yes" };


PROCEDURE SaveConfig(filename-: ARRAY OF CHAR);
VAR
  w    : std.WINDOW;
  size : crt.SZ;
  f    : ioc.ChanId;
  buf  ,
  pname: ARRAY [0..1023] OF CHAR;
  res  : seq.OpenResults;
  i, j : CARDINAL;
  cfg  : BOOLEAN;
  curr : OPTION_ELEM;
  dump_hwnd: win.HWND;
  dump_attr: dmm.DUMP;
  name, equ: xs.String;

BEGIN
  IF filename = '' THEN
    arg.ProgramName(pname);
    fil.ExtractFileName(pname, buf);
    fil.ChangeExtension(buf, CfgFileExt);
    cfg := TRUE;
  ELSE
    fil.GetExtension(filename, buf);
    xs.Uppercase(buf);
    cfg := buf = CfgFileExt;
    COPY(filename, buf);
    IF NOT cfg THEN fil.ChangeExtension(buf, SesFileExt); END;
  END;
  fil.RemoveExtension(opt.KbdFile);
  red.Write(buf, pname);
  seq.OpenWrite(f, pname, seq.write + seq.text + seq.old, res);
  IF res # seq.opened THEN RETURN END;
  seq.Rewrite(f);
  tio.WriteString(f, '[ Options ]');
  tio.WriteLn(f);
  FOR curr := MIN(OPTION_ELEM) TO MAX(OPTION_ELEM) DO
    WITH CurrentOptions^[curr] DO
      CASE opt OF
      | WholeEq:
        fmt.print(buf, '  %s = %d', Name, var^);
      | BoolEq:
        fmt.print(buf, '  %s = %s', Name, BooleanName[cond^]);
      | StrEq:
        COPY(line^, pname);
        IF HIGH(pname) >= line_len THEN
          pname[line_len] := 0C;
        END;
        fmt.print(buf, '  %s = %s', Name, pname);
      END;
     tio.WriteString(f, buf);
     tio.WriteLn(f);
    END;
  END;
  tio.WriteString(f, '[ Colors ]');
  tio.WriteLn(f);
  FOR i := 0 TO HIGH(crt.Pallette^) DO
    WITH crt.Pallette^[i] DO
      fmt.print(buf,'  %s',Name);
      tio.WriteString(f, buf);
      tio.WriteLn(f);
      FOR j := 0 TO N-1 DO
        fmt.print(buf,'   %s = %s on %s', Names^[j], crt.Names[crt.Fg(Attrs^[j])], crt.Names[crt.Bg(Attrs^[j])]);
        tio.WriteString(f, buf);
        tio.WriteLn(f);
      END;
    END;
  END;

  (* Запись содержимого окна слежения *)
  tio.WriteString(f, '[ Watches ]');
  tio.WriteLn(f);
  IF NOT cfg THEN
    WITH dw.Watches DO
      IF count > 0 THEN
        FOR i := 0 TO count-1 DO
          WITH Watch^[i] DO
            tio.WriteString(f, expr^);
            tio.WriteLn(f);
          END;
        END;
      END;
    END;
  END;

  (* Запись алиасов *)
  tio.WriteString(f, '[ Aliases ]');
  tio.WriteLn(f);
  IF NOT cfg THEN
    FOR i := 1 TO lst.EquNamesNo() DO
      tio.WriteString(f, '  ');
      lst.GetName (i-1, name);
      tio.WriteString(f, name);
      tio.WriteString(f, ' = ');
      lst.GetEquName (name, equ);
      tio.WriteString(f, equ);
      tio.WriteLn(f);
    END;
  END;

  tio.WriteString(f, '[ Windows layout ]');
  tio.WriteLn(f);
  FOR w := MIN(std.WINDOW) TO MAX(std.WINDOW) DO
    IF (std.Wnds[w].hwnd # win.Invalid_H) THEN
      fmt.print(buf, '   [ %s ]', std.Wnds[w].name);
      tio.WriteString(f,buf);
      tio.WriteLn(f);

      size := win.GetWindowSize(std.Wnds[w].hwnd);

      WITH size DO
        fmt.print(buf, '     size=( %$2d, %$2d, %$2d, %$2d)', x1, y1, x2, y2);
      END;
      tio.WriteString(f, buf);
      IF win.Visible(std.Wnds[w].hwnd) THEN
        tio.WriteString(f, ' V');
      ELSE
        tio.WriteString(f, ' -');
      END;
      tio.WriteLn(f);
    END;
  END;

  IF NOT cfg THEN
    (* Запись всех открытых дампов *)
    dump_hwnd := dmm.FindOpenedDumpWindow (win.Invalid_H);
    WHILE dump_hwnd # win.Invalid_H DO
      tio.WriteString(f, '   [ Dump_Window ]');
      tio.WriteLn(f);
      dmm.GetDumpAttr (dump_hwnd, dump_attr);
      size := win.GetWindowSize(dump_hwnd);
      WITH size DO
        fmt.print(buf, '     size=( %$2d, %$2d, %$2d, %$2d) V %$8X %$8X %$8X %u', x1, y1, x2, y2,
                       dump_attr.addr_first, dump_attr.min, dump_attr.max, dump_attr.Mode);
      END;
      tio.WriteString(f, buf);
      tio.WriteLn(f);
      dump_hwnd := dmm.FindOpenedDumpWindow (dump_hwnd);
    END;
  END;

  tio.WriteString(f, '.');
  tio.WriteLn(f);
  seq.Close(f);

  IF cfg THEN SaveKbd(opt.KbdFile); END;
END SaveConfig;

(* восттанавливает состояния окон и дpугие настройки из opt файла с именем,
   совпадающим с загpужаемым abs файлом. Если такого не существует, беpется
   dvx.opt. Если и его нет, то будут умолчательные значения вpемени компиляуии *)

PROCEDURE LoadConfig (name: ARRAY OF CHAR);
VAR
  filename: xs.String;

  buf : xs.String;
  tmp : xs.String;
  f   : seq.ChanId;
  res : seq.OpenResults;
  pos : CARDINAL;
  k   : std.WINDOW;
  size: crt.SZ;
  ok  : BOOLEAN;
  fg  : crt.COLOR;
  bg  : crt.COLOR;
  i, j: CARDINAL;
  Line: CARDINAL;
  ses : BOOLEAN;
  curr: OPTION_ELEM;

  dump_attr: dmm.DUMP;

  PROCEDURE FindColor (str-: ARRAY OF CHAR): crt.COLOR;
  VAR
    color: crt.COLOR;
  BEGIN
    color := crt.FindColor(str);
    RETURN color;
  EXCEPT
    Raise (FALSE, Line);
  END FindColor;

BEGIN
  LOOP
    IF name = '' THEN
      -- try load *.cfg file by *.red
      ses := FALSE;
      arg.ProgramName(filename);
      fil.ExtractFileName(filename, buf);
      fil.ChangeExtension(buf, CfgFileExt);
    ELSE
      -- try load *.ses file by .red
      ses := TRUE;
      COPY(name, buf);
      fil.ChangeExtension(buf, SesFileExt);
    END;
    IF red.Read(buf, filename) = red.Ok THEN EXIT; END;
    IF ses THEN
      -- try load *.cfg instead *.ses now
      COPY("", name);
    ELSE
      -- try load *.cfg file at home directory
      ses := FALSE;
      arg.ProgramName(filename);
      fil.ChangeExtension(filename, CfgFileExt);
      EXIT;
    END;
  END;
  seq.OpenRead(f, filename, seq.read+seq.old+seq.text, res);
  IF res # seq.opened THEN RETURN; END;

  tio.ReadString(f, buf);
  Raise (buf = '[ Options ]', 1);
  tio.SkipLine(f);
  Line := 2;
  LOOP
    tio.ReadToken(f, buf); -- имя опции
    IF (ioc.ReadResult(f) # icc.allRight) OR (buf = '.') THEN
      seq.Close(f);
      RETURN;
    END;
    curr := MIN(OPTION_ELEM);
    ok := TRUE;
    LOOP
      IF buf = CurrentOptions^[curr].Name THEN
        EXIT;
      END;
      IF curr = MAX(OPTION_ELEM) THEN
        ok := FALSE;
        EXIT;
      END;
      INC(curr);
    END;
    IF NOT ok THEN EXIT; END;
    tio.ReadToken(f, buf);
    Raise (buf = '=', Line);
    tio.ReadToken(f, buf);
    WITH CurrentOptions^[curr] DO
      CASE opt OF
      | WholeEq:
        var^ := xs.StrToCard (buf, 10, ok);
        Raise (ok, Line);
        COPY(buf, var_new);
      | BoolEq:
        IF buf = BooleanName[TRUE] THEN
          cond^ := TRUE;
        ELSE
          Raise (buf = BooleanName[FALSE], Line);
          cond^ := FALSE;
        END;
        cond_new := cond^;
      | StrEq:
        -- возможно, приемник не сможет принять слишком длинную строку
        IF HIGH(buf) >= line_len THEN
          buf[line_len] := 0C;
        END;
        COPY(buf, line^);
        COPY(buf, line_new);
      END;
      IF Action # NIL THEN Raise (Action(), Line); END;
    END;
    tio.SkipLine(f);
    INC(Line);
  END;

  -- buf уже содержит следующую строку за опциями
  Raise (buf = '[', Line);
  tio.ReadString(f, buf);
  Raise (buf = ' Colors ]', Line);
  tio.SkipLine(f);
  INC(Line);
  FOR i := 0 TO HIGH(crt.Pallette^) DO
    WITH crt.Pallette^[i] DO
      tio.ReadToken(f,buf);
      Raise (buf = Name, Line);
      tio.SkipLine(f);
      INC(Line);
      j := 0;
      LOOP
        tio.ReadToken(f,buf);
        WHILE (buf # Names^[j]) AND (j<N) DO INC(j) END;
        tio.ReadToken(f,buf);
        Raise (buf = '=', Line);
        tio.ReadToken(f,buf);
        fg := FindColor(buf);
        tio.ReadToken(f,buf);
        Raise (buf = 'on', Line);
        tio.ReadToken(f,buf);
        bg := FindColor(buf);
        Attrs^[j] := crt.Attr(fg,bg);
        tio.SkipLine(f);
        INC(Line);
        INC(j);
        IF j = N THEN EXIT END;
      END;
    END;
  END;

  tio.ReadString(f, buf);
  Raise (buf = '[ Watches ]', Line);
  LOOP
    tio.SkipLine(f);
    INC(Line);
    tio.ReadString(f, buf);
    Raise (ioc.ReadResult(f) = icc.allRight, Line);
    IF buf[0] = '[' THEN EXIT; END;
    dw.WatchFromPack (buf);
  END;

  Raise (buf = '[ Aliases ]', Line);
  tio.SkipLine(f);
  INC(Line);
  LOOP
    tio.ReadToken(f, buf);
    IF buf[0] = '[' THEN
      tio.ReadString(f, tmp);
      xs.Append (tmp, buf);
      EXIT;
    END;
    COPY(buf, name);
    tio.ReadToken(f, buf);
    Raise (buf = '=', Line);
    tio.ReadToken(f, buf);
    Raise (buf # '', Line);
    lst.PutEquName (name, buf);
    tio.SkipLine(f);
    INC(Line);
  END;

  Raise (buf = '[ Windows layout ]', Line);
  tio.SkipLine(f);
  INC(Line);
  LOOP
    tio.ReadToken(f,buf);
    IF (ioc.ReadResult(f) # icc.allRight) OR (buf = '.') THEN
      EXIT;
    END;
    Raise (buf = '[', Line);

    tio.ReadToken(f,buf);
    pos := MAX(CARDINAL);
    FOR k := MIN(std.WINDOW) TO MAX(std.WINDOW) DO
      IF std.Wnds[k].name = buf THEN
        pos := ORD(k);
      END;
    END;
    Raise ((pos # MAX(CARDINAL)) OR (buf = 'Dump_Window'), Line);
    tio.SkipLine(f);
    INC(Line);

    tio.ReadToken(f, buf);

    tio.ReadToken(f, buf);
    buf[LENGTH(buf)-1] := 0C;
    size.x1 := xs.StrToCard (buf, 10, ok);
    Raise (ok, Line);

    tio.ReadToken(f,buf);
    buf[LENGTH(buf)-1] := 0C;
    size.y1 := xs.StrToCard (buf, 10, ok);
    Raise (ok, Line);

    tio.ReadToken(f,buf);
    buf[LENGTH(buf)-1] := 0C;
    size.x2 := xs.StrToCard (buf, 10, ok);
    Raise (ok, Line);

    tio.ReadToken(f,buf);
    buf[LENGTH(buf)-1] := 0C;
    size.y2 := xs.StrToCard (buf, 10, ok);
    Raise (ok, Line);

    Raise ( (size.x2 < crt.Xmax) AND (size.y2 < crt.Ymax) AND
            (size.x1+1 < size.x2) AND (size.y1+1 < size.y2) AND
            (1 <= size.y1), Line);

    tio.ReadToken(f, buf);
    Raise ((buf = 'V') OR (buf = '-'), Line);

    IF pos # MAX(CARDINAL) THEN
      std.Wnds[VAL(std.WINDOW, pos)].size := size;
      IF buf = 'V' THEN
        std.Wnds[VAL(std.WINDOW, pos)].init(TRUE);
      ELSE
        IF std.Wnds[VAL(std.WINDOW, pos)].hwnd = win.Invalid_H THEN
          std.Wnds[VAL(std.WINDOW, pos)].init(FALSE);
        ELSE
          eve.AddToTail(std.Wnds[VAL(std.WINDOW, pos)].hwnd, eve.Hide,0);
        END;
      END;
      win.SetWindowSize(std.Wnds[VAL(std.WINDOW, pos)].hwnd, size);
      IF kex.Loaded AND (VAL(std.WINDOW, pos) = (std.MainWindow)) THEN
        mod.SetCurrLine(mod.Curr^.curr);
      END;
    ELSE
      tio.ReadToken(f, buf);
      dump_attr.addr_first := xs.StrToCard (buf, 16, ok);
      Raise (ok, Line);
      tio.ReadToken(f, buf);
      dump_attr.min := xs.StrToCard (buf, 16, ok);
      Raise (ok, Line);
      tio.ReadToken(f, buf);
      dump_attr.max := xs.StrToCard (buf, 16, ok);
      Raise (ok, Line);
      Raise ((dump_attr.min<=dump_attr.addr_first) AND (dump_attr.addr_first<=dump_attr.max), Line);
      tio.ReadToken(f, buf);
      dump_attr.Mode := dt.SYM_TYPE_ID(xs.StrToCard (buf, 10, ok));
      Raise (ok, Line);
      dump_attr.addr := dump_attr.addr_first;
      dump_attr.curr := 0;
      dump_attr.write := TRUE;;
      dmm.OpenNewDumpWindow (dump_attr, size);
    END;

    tio.SkipLine(f);
    INC(Line);

  END;
  tio.SkipLine(f);
  INC(Line);

  seq.Close(f);
EXCEPT
  IF exc.IsCurrentSource(source) THEN
    std.ErrorNo(mes.ErrorInConfigFile, filename, exc.CurrentNumber(source));
    seq.Close(f);
    RETURN;
  END;
END LoadConfig;


VAR
  PalletteList: win.HWND;
  ColorsList: win.HWND;
  OptionsDialog: win.HWND;

PROCEDURE ColorsHandler (hwnd: win.HWND; msg: eve.MSG);
VAR
  p, p2: std.PLIST;

  PROCEDURE write_line(num: CARDINAL);
  VAR
    buf : xs.String;
    l : CARDINAL;
  BEGIN
    WITH crt.Pallette^[p2^.curr] DO
      WITH p^ DO
        l := num - p^.frame + 1;
        crt.SetPos(1, l);
        fmt.print(buf, '%-13.13s: %-9.9s on %-9.9s', Names^[num], crt.Names[crt.Fg(Attrs^[num])],
                  crt.Names[crt.Bg(Attrs^[num])]);
        crt.WrStrFromPos(hwnd, buf, Colors^[crt.List_Line], pos);
        IF (p^.curr = num) AND (hwnd = win.ActiveWindow) THEN
          crt.Lite(hwnd, l , 1, Colors^[crt.List_CurrentLine]);
        END;
        crt.Lite(hwnd, l , 16, Attrs^[num]);
      END;
    END;
  END write_line;

VAR
  size     : crt.SZ;
  i, len   : CARDINAL;
  last, tmp: CARDINAL;
  color    : crt.COLOR;

BEGIN
  p  := win.GetAMPtr(hwnd);
  p2 := win.GetAMPtr(PalletteList);
  CASE msg.ID OF
  | eve.Redraw, eve.Paint:
    size := win.GetWindowSize(hwnd);
    IF ( msg.par # 3) AND (msg.par # 4) THEN
      crt.FillWindow(hwnd, ' ', p^.Colors^[crt.List_Background])
    END;
    WITH size DO len := y2 - y1 - 1; END;
    WITH p^ DO
      N := crt.Pallette^[p2^.curr].N;
      IF N > 0 THEN
        CASE msg.par OF
        | 3:
          IF hwnd = win.ActiveWindow THEN
            write_line(curr);
          END;
        | 4:
          IF hwnd = win.ActiveWindow THEN
            crt.Lite(hwnd, curr-frame+1 , 1, Colors^[crt.List_Background]);
            tmp := curr;
            curr := MAX(CARDINAL);
            write_line(tmp);
            curr := tmp;
          END;
        ELSE
          last := std.Min(frame+len-1, N-1);
          FOR i:= frame TO last DO
            write_line(i)
          END;
        END;
      END;
    END;
    std.ListBox (hwnd, msg);

  | eve.KbHit:
    WITH p^ DO
      CASE msg.par OF
      | key.Left, key.Right:
        WITH crt.Pallette^[p2^.curr] DO
          color := crt.Fg(Attrs^[curr]);
          IF msg.par = key.Left THEN
            IF color = MIN(crt.COLOR) THEN
              color := MAX(crt.COLOR);
            ELSE
              DEC(color);
            END;
          ELSE
            IF color = MAX(crt.COLOR) THEN
              color := MIN(crt.COLOR);
            ELSE
              INC(color);
            END;
          END;
          Attrs^[curr] := crt.Attr(color, crt.Bg(Attrs^[curr]));
        END;
        eve.AddToTail(hwnd, eve.Redraw, 0);

      | key.CtrlLeft, key.CtrlRight:
        WITH crt.Pallette^[p2^.curr] DO
          color := crt.Bg(Attrs^[curr]);
          IF msg.par = key.CtrlLeft THEN
            IF color = MIN(crt.COLOR) THEN
              color := MAX(crt.COLOR);
            ELSE
              DEC(color);
            END;
          ELSE
            IF color = MAX(crt.COLOR) THEN
              color := MIN(crt.COLOR);
            ELSE
              INC(color);
            END;
          END;
          Attrs^[curr] := crt.Attr(crt.Fg(Attrs^[curr]), color);
        END;
        eve.AddToTail(hwnd, eve.Redraw, 0);
      | key.Enter, key.Esc:
        eve.AddToTail(hwnd, eve.Hide, 0);
        eve.AddToTail(eve.AllWindows, eve.Redraw, 1);
        eve.Flush;
      ELSE
        std.ListBox (hwnd, msg);
      END;
    END;
  ELSE
    std.ListBox (hwnd, msg);
  END;
END ColorsHandler;

PROCEDURE InitColors;
VAR
  p    : std.PLIST;
  size : crt.SZ;
BEGIN
  IF ColorsList = win.Invalid_H THEN
    ColorsList := win.RegisterWindow(ColorsHandler,SIZE(std.LIST));
    ASSERT(ColorsList # win.Invalid_H);

    win.SetModal(ColorsList);

    win.SetHeader(ColorsList, mes.Colors);
    p := win.GetAMPtr(ColorsList);
    p^ := std.EmptyList;
    WITH p^ DO
      N         := crt.Pallette^[p^.curr].N;
      Colors    := sys.ADR(crt.List);
    END;
  ELSE
    p := win.GetAMPtr(ColorsList);
    p^.curr      := 0;
    p^.frame     := 0;
    p^.pos       := 0;
  END;
  p := win.GetAMPtr(PalletteList);
  WITH size DO
    x1 := 24; y1 := 1;
    x2 := 64; y2 := 2+crt.Pallette^[p^.curr].N;
  END;
  win.SetWindowSize(ColorsList, size);
  eve.AddToTail(ColorsList, eve.Rise, 0);
END InitColors;

PROCEDURE PaletteHandler (hwnd: win.HWND; msg: eve.MSG);
VAR
  p: std.PLIST;

  PROCEDURE write_line(num: CARDINAL);
  BEGIN
    WITH p^ DO
      WITH crt.Pallette^[num] DO
        crt.SetPos(2, num - p^.frame + 1);
        crt.WrStrFromPos(hwnd, NameR, Colors^[crt.List_Line], pos);
      END;
    END;
  END write_line;

VAR
  size     : crt.SZ;
  i, len   : CARDINAL;
  last     : CARDINAL;

BEGIN
  p  := win.GetAMPtr(hwnd);
  CASE msg.ID OF
  | eve.Redraw, eve.Paint:
    size := win.GetWindowSize(hwnd);
    IF ( msg.par # 3) AND (msg.par # 4) THEN
      crt.FillWindow(hwnd, ' ', p^.Colors^[crt.List_Background])
    END;
    WITH size DO len := y2 - y1 - 1; END;
    WITH p^ DO
      N := HIGH(crt.Pallette^) + 1;
      IF N > 0 THEN
        CASE msg.par OF
        | 3:
          IF hwnd = win.ActiveWindow THEN
            write_line(curr);
            crt.Lite(hwnd, curr - frame + 1 , 1, Colors^[crt.List_CurrentLine]);
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
            crt.Lite(hwnd, curr - frame + 1 , 1, Colors^[crt.List_CurrentLine]);
          END;
        END;
      END;
    END;
    std.ListBox (hwnd, msg);

  | eve.KbHit:
    CASE msg.par OF
    | key.Enter:
      InitColors;
    | key.Esc:
      eve.AddToTail(hwnd, eve.Hide, 0);
    ELSE
      std.ListBox (hwnd, msg);
    END;
  ELSE
    std.ListBox (hwnd, msg);
  END;
END PaletteHandler;

PROCEDURE InitWindow(action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  p    : std.PLIST;
  size : crt.SZ;
BEGIN
  ASSERT(action = act.Pallette);
  IF mode # act.mode_check THEN
    IF PalletteList = win.Invalid_H THEN
      PalletteList := win.RegisterWindow(PaletteHandler, SIZE(std.LIST));
      ASSERT(PalletteList # win.Invalid_H);

      win.SetModal(PalletteList);

      win.SetHeader(PalletteList, mes.Patterns);
      p := win.GetAMPtr(PalletteList);
      p^ := std.EmptyList;
      WITH p^ DO
        N         := HIGH(crt.Pallette^)+1;
        Colors    := sys.ADR(crt.List);
        Frame     := crt.Double;
      END;
    END;
    WITH size DO
      x1 := 0 ; y1 := 1;
      x2 := 32; y2 := 3+HIGH(crt.Pallette^);
    END;
    win.SetWindowSize(PalletteList, size);
    eve.AddToTail(PalletteList, eve.Rise, 0);
  END;
  RETURN TRUE;
END InitWindow;



------------------------ Actions for options ------------------------

<* IF DEST_XDS THEN *>

CONST
  Opt2Line = OPTION2LINE { 00    -- Traced_Run_Delay
                         , 09    -- HilighCode
                         , 10    -- HilighCall
                         , 11    -- FullDisasm
                         , 12    -- DumpDisplay
                         , 18    -- Save_Opt_File
                         , 14    -- WholeHex
                         , 19    -- KbdFile
                         , 07    -- ShowAllModules
                         , 17    -- WarningBell
                         , 16    -- UseSingleStructWindow
                         , 99    -- DumpType -- нет соответствия
                         , 08    -- ShowModuleWithoutSource
                         , 13    -- DisplayDereferencePointer
                         , 03    -- StopOnHardwareExceptionFirstChance
                         , 04    -- StopOnSoftwareException
                         , 01    -- SSS_Delay
                         , 02    -- Skip_Disasm
                         , 05    -- StripPathFromFullName
                         , 06    -- StripPathFromPartialName
                         , 15    -- AutoDetectActualType
                         , 99    -- FrameImageSingle    -- нет соответствия
                         , 99    -- FrameImageDouble    -- нет соответствия
                         , 99    -- FrameImageMove      -- нет соответствия
                         , 99    -- TransliterateFromTo -- нет соответствия
                         , 99    -- MergeEqualTypes     -- нет соответствия
                         };



PROCEDURE ActionStripFileName (): BOOLEAN;
BEGIN
  mod.ResetTryRead;
  RETURN TRUE;
END ActionStripFileName;

PROCEDURE ActionShowSoftwareExceptions (): BOOLEAN;
BEGIN
  dex.Traps[dex.JavaLangRaisedException].Enabled := 
                        CurrentOptions^[StopOnSoftwareException].cond_new;
  dex.Traps[dex.LongjmpNo].Enabled := 
                        CurrentOptions^[StopOnSoftwareException].cond_new;
  dex.Traps[dex.JavaLangUncaughtException].Enabled := 
                        CurrentOptions^[StopOnSoftwareException].cond_new;
  dex.RemoveRTShandler;
  dex.SetRTShandler;

  xi.RemoveXDInterfacesHandlers ();
  xi.SetXDInterfacesHandlers (CurrentOptions^[StopOnSoftwareException].cond_new);

  RETURN TRUE;
END ActionShowSoftwareExceptions;


TYPE
  FROM_TO_ID = ARRAY [0..15] OF CHAR;

  TABLE = ARRAY Translit.TRANSLITERATE_FROM_TO OF FROM_TO_ID;


CONST
  TransliterateFromToTable = TABLE { ""          -- None
                                   , "Alt->ISO"  -- from Alternative to ISO
                                   , "Alt->KOI8" -- from Alternative to Koi8
                                   , "Alt->Win"  -- from Alternative to Windows
                                   , "ISO->Alt"  -- from ISO to Alternative
                                   , "ISO->KOI8" -- from ISO to Koi8
                                   , "ISO->Win"  -- from ISO to Windows
                                   , "KOI8->Alt" -- from Koi8 to Alternative
                                   , "KOI8->ISO" -- from Koi8 to ISO
                                   , "KOI8->Win" -- from Koi8 to Windows
                                   , "Win->Alt"  -- from Windows to Alternative
                                   , "Win->ISO"  -- from Windows to ISO
                                   , "Win->KOI8" -- from Windows to Koi8
                                   };

VAR
  TransliterateFromToStr: FROM_TO_ID;


PROCEDURE ActionTranslitFromTo (): BOOLEAN;
VAR
  from_to: Translit.TRANSLITERATE_FROM_TO;
BEGIN
  FOR from_to := MIN(Translit.TRANSLITERATE_FROM_TO) TO MAX(Translit.TRANSLITERATE_FROM_TO) DO
    IF CurrentOptions^[TransliterateFromTo].line_new = TransliterateFromToTable [from_to] THEN
      opt.TranslirateTextFromTo := from_to;
      RETURN TRUE;
    END;
  END;
  RETURN FALSE;
END ActionTranslitFromTo;

<* END *>


<* IF DEST_K26 THEN *>

CONST
  Opt2Line = OPTION2LINE { 00    -- Traced_Run_Delay
                         , 02    -- HilighCode
                         , 99    -- HilighCall -- нет соответствия
                         , 03    -- FullDisasm
                         , 04    -- DumpDisplay
                         , 08    -- Save_Opt_File
                         , 09    -- WholeHex
                         , 10    -- KbdFile
                         , 01    -- ShowAllModules
                         , 07    -- WarningBell
                         , 06    -- UseSingleStructWindow
                         , 99    -- DumpType                  -- нет соответствия
                         , 99    -- ShowModuleWithoutSource   -- нет соответствия
                         , 99    -- DisplayDereferencePointer -- нет соответствия
                         , 05    -- Trace registers
                         , 12    -- IntStateSystem
                         , 13    -- IntStateTest
                         , 14    -- IntStateStart
                         , 99    -- MergeEqualTypes           -- нет соответствия
                         };


PROCEDURE TraceRegistersAction(): BOOLEAN;
BEGIN
  IF CurrentOptions^[TraceRegisters].cond_new THEN
    dmm.IniRegisters;
  ELSE
    dmm.ClearRegisters;
  END;
  RETURN TRUE;
END TraceRegistersAction;

VAR
  StateSystem: BOOLEAN;
  StateTest  : BOOLEAN;
  StateStart : BOOLEAN;

PROCEDURE UpdateStates;
BEGIN
  StateSystem := int.system IN int.Mode;
  StateTest   := int.tests IN int.Mode;
  StateStart  := int.start IN int.Mode;
END UpdateStates;


PROCEDURE ToggleInterpretatorState (): BOOLEAN;
BEGIN
  IF CurrentOptions^[IntStateSystem].cond_new THEN INCL(int.Mode, int.system); ELSE EXCL(int.Mode, int.system); END;
  IF CurrentOptions^[IntStateTest  ].cond_new THEN INCL(int.Mode, int.tests);  ELSE EXCL(int.Mode, int.tests);  END;
  IF CurrentOptions^[IntStateStart ].cond_new THEN INCL(int.Mode, int.start);  ELSE EXCL(int.Mode, int.start);  END;
  RETURN TRUE;
END ToggleInterpretatorState;

<* END *>


(* Показывать все модули *)
PROCEDURE ActionShowAllModules(): BOOLEAN;
BEGIN
  mod.RefreshModulesKey;
  RETURN TRUE;
END ActionShowAllModules;


(* Загрузка файла раскладки клавиатуры *)
PROCEDURE ActionKbdFile (): BOOLEAN;
BEGIN
  RETURN LoadKbd (CurrentOptions^[KbdFile].line_new);
END ActionKbdFile;


<* IF DEST_XDS THEN *>

PROCEDURE ActionUpdateCallStack (): BOOLEAN;
BEGIN
  IF kex.Loaded AND kex.ProgramContextOk THEN
    WITH CurrentOptions^[HilighCall] DO
      ASSERT(opt=BoolEq);
      IF cond_new THEN
        dex.ResetCallStack;
        stk.ScanCallStack;
      END;
    END;
  END;
  RETURN TRUE;
END ActionUpdateCallStack;

<* END *>

(* Обновляет диалоговые окно с опциями *)
PROCEDURE UpdateOptions;
BEGIN
  eve.AddToTail (OptionsDialog, eve.Redraw, 0);
END UpdateOptions;


(* Устанавливает опцию WholeHex в соответствии с переключателем *)
PROCEDURE ToggleWholeHexOption;
VAR
  p: std.PDIALOG;
BEGIN
  p := win.GetAMPtr(OptionsDialog);
  CurrentOptions^[WholeHex].cond_new := p^.Lines^[Opt2Line[WholeHex]].ractive = 0;
  UpdateOptions;
END ToggleWholeHexOption;


VAR
  UseKbdOption: BOOLEAN;

(* Устанавливает опцию KbdFile в соответствии со значением переключателя *)
PROCEDURE ToggleKbdLayoutOption;
VAR
  p: std.PDIALOG;
BEGIN
  p := win.GetAMPtr(OptionsDialog);
  WITH p^ DO
    WITH Lines^[Opt2Line[KbdFile]+1] DO
      IF Lines^[Opt2Line[KbdFile]].cactive^ THEN
        state := std.d_enabled;
        eve.AddToTail (OptionsDialog, eve.KbHit, key.Tab);
      ELSE
        state := std.d_disabled;
      END;
    END;
  END;
  UpdateOptions;
END ToggleKbdLayoutOption;


(* Проверка состояния опций *)
PROCEDURE CheckOptions (VAR error: OPTION_ELEM): BOOLEAN;
VAR
  curr: OPTION_ELEM;
  res : ws.ConvResults;
  card: CARDINAL;
  p   : std.PDIALOG;
BEGIN
  p := win.GetAMPtr(OptionsDialog);
  FOR curr := MIN(OPTION_ELEM) TO MAX(OPTION_ELEM) DO
    IF (curr # KbdFile) OR (p^.Lines^[Opt2Line[curr]+1].state = std.d_enabled) THEN
      WITH CurrentOptions^[curr] DO
        IF opt = WholeEq THEN
          ws.StrToCard (var_new, card, res);
          IF res # ws.strAllRight THEN
            error := curr;
            RETURN FALSE;
          END;
        END;
        IF (Action # NIL) AND NOT Action() THEN
          error := curr;
          RETURN FALSE;
        END;
      END;
    END;
  END;
  RETURN TRUE;
END CheckOptions;


(* Сохраняет опции *)
PROCEDURE SaveOptions;
VAR
  curr: OPTION_ELEM;
  res : ws.ConvResults;
  p   : std.PDIALOG;
BEGIN
  p := win.GetAMPtr(OptionsDialog);
  FOR curr := MIN(OPTION_ELEM) TO MAX(OPTION_ELEM) DO
      IF (curr # KbdFile) OR (p^.Lines^[Opt2Line[curr]+1].state = std.d_enabled) THEN
        WITH CurrentOptions^[curr] DO
        CASE opt OF
        | WholeEq:
          ws.StrToCard (var_new, var^, res);
        | BoolEq:
          cond^ := cond_new;
        | StrEq:
          IF HIGH(line_new) >= line_len THEN
            line_new[line_len] := 0C;
          END;
          COPY(line_new, line^);
        END;
      END;
    END;
  END;
END SaveOptions;


(* Выдает сообщение о неправильном соотоянии опции *)
PROCEDURE InvalidOption (curr: OPTION_ELEM);
VAR
  p: std.PDIALOG;
  f: xs.String;
  s: xs.String;
BEGIN
  p := std.PDIALOG(win.GetAMPtr(OptionsDialog));
  p^.curr := Opt2Line[curr];
  pro.GetMsg (mes.InvalidOption, f);
  fmt.print (s, f, CurrentOptions^[curr].NameR);
  std.Error (s);
END InvalidOption;


(* Проверяет состояние опций и в случае ошибки выдает сообщение *)
(* Сохраняет заданые значения опций как текущие                 *)
PROCEDURE SaveCurrentOptions (hwnd: win.HWND): BOOLEAN;
VAR
  curr: OPTION_ELEM;
BEGIN
  -- Проверка состояния опций
  IF NOT CheckOptions (curr) THEN
    InvalidOption (curr);
    RETURN FALSE;
  END;
  -- Сохранение заданных значений
  SaveOptions;
  eve.AddToTail(hwnd, eve.Hide, 0);
  eve.AddToTail(eve.AllWindows, eve.Redraw, 0);
  RETURN TRUE;
END SaveCurrentOptions;


-- Reset options to default value
PROCEDURE ResetToDefault;
VAR
  curr: OPTION_ELEM;
  p   : std.PDIALOG;
BEGIN
  FOR curr := MIN(OPTION_ELEM) TO MAX(OPTION_ELEM) DO
    WITH CurrentOptions^[curr] DO
      CASE opt OF
      | WholeEq : ws.CardToStr (var_def, var_new);
      | BoolEq  : cond_new := cond_def;
      | StrEq   : COPY(line_def, line_new);
      END;
    END;
  END;
  p := std.PDIALOG(win.GetAMPtr(OptionsDialog));
  WITH p^ DO
    -- Setup radix value
    WITH Lines^[Opt2Line[WholeHex]] DO
      IF CurrentOptions^[WholeHex].cond_def THEN
        ractive := 0;
        rcurr := 0;
      ELSE
        ractive := 1;
        rcurr := 1;
      END;
    END;
    -- Setup keyboard layout
    IF CurrentOptions^[KbdFile].line_def = '' THEN
      Lines^[Opt2Line[KbdFile]].cactive^ := FALSE;
      Lines^[Opt2Line[KbdFile]+1].state := std.d_disabled;
    ELSE
      Lines^[Opt2Line[KbdFile]].cactive^ := TRUE;
      Lines^[Opt2Line[KbdFile]+1].state := std.d_enabled;
    END;
  END;
  UpdateOptions;
END ResetToDefault;


<* IF DEST_XDS THEN *>

-- Save config
PROCEDURE SaveConfigFromOptionsDialog;
VAR
  curr: OPTION_ELEM;
BEGIN
  -- Проверка состояния опций
  IF NOT CheckOptions (curr) THEN
    InvalidOption (curr);
    RETURN;
  END;
  -- Сохранение заданных значений
  SaveOptions;
  act.ExecuteAction (act.SaveConfig, act.mode_loud);
  UpdateOptions;
END SaveConfigFromOptionsDialog;

<* END *>


<* PUSH *>
<* WOFF314+ *>
<* WOFF900+ *>
PROCEDURE InitOptWindow (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
VAR
  Lines: std.PLINES;
  p    : std.PDIALOG;
  size : crt.SZ;
  curr : OPTION_ELEM;
  Radix: std.PRADIO;
 <* IF DEST_XDS THEN *>
  free : CARDINAL;
 <* END *>
BEGIN
  ASSERT(action = act.Options);
  IF mode # act.mode_check THEN
    IF OptionsDialog = win.Invalid_H THEN

     <* IF DEST_K26 THEN *>
      UpdateStates;
      -- Quantity options and some lines for frames, prompts, etc.
      NEW(Lines, 17);
      -- Settings options
      WITH CurrentOptions^[Traced_Run_Delay] DO
        ASSERT((opt = WholeEq) AND display);
        Lines^[Opt2Line[Traced_Run_Delay]] := std.LINE{26, 2, std.edit_str, sys.ADR(var_new), 4, std.d_enabled};
      END;
      WITH CurrentOptions^[ShowAllModules] DO
        ASSERT((opt = BoolEq) AND display);
        Lines^[Opt2Line[ShowAllModules]] := std.LINE{ 2, 3, std.check, NameR, sys.ADR(cond_new), UpdateOptions, 1, key.AltG, std.d_enabled};
      END;
      WITH CurrentOptions^[HilighCode] DO
        ASSERT((opt = BoolEq) AND display);
        Lines^[Opt2Line[HilighCode]] := std.LINE{ 2, 4, std.check, NameR, sys.ADR(cond_new), UpdateOptions, 1, key.AltD, std.d_enabled};
      END;
      WITH CurrentOptions^[FullDisasm] DO
        ASSERT((opt = BoolEq) AND display);
        Lines^[Opt2Line[FullDisasm]] := std.LINE{ 2, 5, std.check, NameR, sys.ADR(cond_new), UpdateOptions, 8, key.AltL, std.d_enabled};
      END;
      WITH CurrentOptions^[DumpDisplay] DO
        ASSERT((opt = BoolEq) AND display);
        Lines^[Opt2Line[DumpDisplay]] := std.LINE{ 2, 6, std.check, NameR, sys.ADR(cond_new), UpdateOptions, 4, key.AltF, std.d_enabled};
      END;
      WITH CurrentOptions^[TraceRegisters] DO
        ASSERT((opt = BoolEq) AND display);
        Lines^[Opt2Line[TraceRegisters]] := std.LINE{ 2, 7, std.check, NameR, sys.ADR(cond_new), UpdateOptions, 1, key.AltN, std.d_enabled};
      END;
      WITH CurrentOptions^[UseSingleStructWindow] DO
        ASSERT((opt = BoolEq) AND display);
        Lines^[Opt2Line[UseSingleStructWindow]] := std.LINE{ 2, 8, std.check, NameR, sys.ADR(cond_new), UpdateOptions, 1, key.AltJ, std.d_enabled};
      END;
      WITH CurrentOptions^[WarningBell] DO
        ASSERT((opt = BoolEq) AND display);
        Lines^[Opt2Line[WarningBell]] := std.LINE{ 2, 9, std.check, NameR, sys.ADR(cond_new), UpdateOptions, 1, key.AltP, std.d_enabled};
      END;
      WITH CurrentOptions^[Save_Opt_File] DO
        ASSERT((opt = BoolEq) AND display);
        Lines^[Opt2Line[Save_Opt_File]] := std.LINE{ 2, 10, std.check, NameR, sys.ADR(cond_new), UpdateOptions, 1, key.AltC, std.d_enabled};
      END;
      -- Radio buttons for radix option WholeHex
      NEW (Radix, 2);
      Radix^[0] := std.RADIO{27, 11, 'Hex', 1, key.AltH };
      Radix^[1] := std.RADIO{37, 11, 'Dec', 2, key.AltE };
      Lines^[Opt2Line[WholeHex]] := std.LINE{4, 11, std.radio , 'Система счисления', 0, 0, 0, 0, Radix, ToggleWholeHexOption, std.d_enabled};
      -- Use keyboard layout
      WITH CurrentOptions^[KbdFile] DO
        Lines^[Opt2Line[KbdFile]] := std.LINE{ 2, 12, std.check , NameR, sys.ADR(UseKbdOption), ToggleKbdLayoutOption, 1, key.AltB, std.d_disabled};
        ASSERT((opt = StrEq) AND display);
        Lines^[Opt2Line[KbdFile]+1] := std.LINE{ 6, 13, std.edit_str, sys.ADR(line_new), 25, std.d_enabled};
      END;
      WITH CurrentOptions^[IntStateSystem] DO
        ASSERT((opt = BoolEq) AND display);
        Lines^[Opt2Line[IntStateSystem]] := std.LINE{ 2, 14, std.check, NameR, sys.ADR(cond_new), UpdateOptions, 6, key.AltV, std.d_enabled};
      END;
      WITH CurrentOptions^[IntStateTest] DO
        ASSERT((opt = BoolEq) AND display);
        Lines^[Opt2Line[IntStateTest]] := std.LINE{ 2, 15, std.check, NameR, sys.ADR(cond_new), UpdateOptions, 7, key.AltS, std.d_enabled};
      END;
      WITH CurrentOptions^[IntStateStart] DO
        ASSERT((opt = BoolEq) AND display);
        Lines^[Opt2Line[IntStateStart]] := std.LINE{ 2, 16, std.check, NameR, sys.ADR(cond_new), UpdateOptions, 27, key.AltU, std.d_enabled};
      END;
      -- Reset to default
      Lines^[15] := std.LINE{ 9, 18, std.button, 'Значения по-умолчанию', ResetToDefault, 2, key.AltY, std.d_enabled };
      -- Prompts
      Lines^[16] := std.LINE{ 6, 2, std.msg, CurrentOptions^[Traced_Run_Delay].NameR, std.d_enabled };

      WITH size DO
        x1 := 17; y1 := 2;
        x2 := 62; y2 := 24;
      END;
     <* END *>

     <* IF DEST_XDS THEN *>
      -- Quantity options and some lines for frames, prompts, etc.
      NEW(Lines, 33);
      -- Settings options
      WITH CurrentOptions^[Traced_Run_Delay] DO
        ASSERT((opt = WholeEq) AND display);
        Lines^[Opt2Line[Traced_Run_Delay]] := std.LINE{32, 3, std.edit_str, sys.ADR(var_new), 4, std.d_enabled};
      END;
      WITH CurrentOptions^[SSS_Delay] DO
        ASSERT((opt = WholeEq) AND display);
        Lines^[Opt2Line[SSS_Delay]] := std.LINE{32, 4, std.edit_str, sys.ADR(var_new), 4, std.d_enabled};
      END;
      WITH CurrentOptions^[Skip_Disasm] DO
        ASSERT((opt = BoolEq) AND display);
        Lines^[Opt2Line[Skip_Disasm]] := std.LINE{ 4, 5, std.check, NameR, sys.ADR(cond_new), UpdateOptions, 1, key.AltN, std.d_enabled};
      END;
      WITH CurrentOptions^[StopOnHardwareExceptionFirstChance] DO
        ASSERT((opt = BoolEq) AND display);
        Lines^[Opt2Line[StopOnHardwareExceptionFirstChance]] := std.LINE{ 4, 6, std.check, NameR, sys.ADR(cond_new), UpdateOptions, 1, key.AltS, std.d_enabled};
      END;
      WITH CurrentOptions^[StopOnSoftwareException] DO
        ASSERT((opt = BoolEq) AND display);
        Lines^[Opt2Line[StopOnSoftwareException]] := std.LINE{ 4, 7, std.check, NameR, sys.ADR(cond_new), UpdateOptions, 10, key.AltW, std.d_enabled};
      END;
      WITH CurrentOptions^[StripPathFromFullName] DO
        ASSERT((opt = BoolEq) AND display);
        Lines^[Opt2Line[StripPathFromFullName]] := std.LINE{ 41, 3, std.check, NameR, sys.ADR(cond_new), UpdateOptions, 2, key.AltT, std.d_enabled};
      END;
      WITH CurrentOptions^[StripPathFromPartialName] DO
        ASSERT((opt = BoolEq) AND display);
        Lines^[Opt2Line[StripPathFromPartialName]] := std.LINE{ 41, 4, std.check, NameR, sys.ADR(cond_new), UpdateOptions, 3, key.AltR, std.d_enabled};
      END;
      WITH CurrentOptions^[ShowAllModules] DO
        ASSERT((opt = BoolEq) AND display);
        Lines^[Opt2Line[ShowAllModules]] := std.LINE{ 41, 5, std.check, NameR, sys.ADR(cond_new), UpdateOptions, 6, key.AltA, std.d_enabled};
      END;
      WITH CurrentOptions^[ShowModuleWithoutSource] DO
        ASSERT((opt = BoolEq) AND display);
        Lines^[Opt2Line[ShowModuleWithoutSource]] := std.LINE{ 41, 6, std.check, NameR, sys.ADR(cond_new), UpdateOptions, 6, key.AltM, std.d_enabled};
      END;
      WITH CurrentOptions^[HilighCode] DO
        ASSERT((opt = BoolEq) AND display);
        Lines^[Opt2Line[HilighCode]] := std.LINE{ 4, 10, std.check, NameR, sys.ADR(cond_new), UpdateOptions, 1, key.AltC, std.d_enabled};
      END;
      WITH CurrentOptions^[HilighCall] DO
        ASSERT((opt = BoolEq) AND display);
        Lines^[Opt2Line[HilighCall]] := std.LINE{ 4, 11, std.check, NameR, sys.ADR(cond_new), UpdateOptions, 3, key.AltL, std.d_enabled};
      END;
      WITH CurrentOptions^[FullDisasm] DO
        ASSERT((opt = BoolEq) AND display);
        Lines^[Opt2Line[FullDisasm]] := std.LINE{ 4, 12, std.check, NameR, sys.ADR(cond_new), UpdateOptions, 1, key.AltF, std.d_enabled};
      END;
      WITH CurrentOptions^[DumpDisplay] DO
        ASSERT((opt = BoolEq) AND display);
        Lines^[Opt2Line[DumpDisplay]] := std.LINE{ 4, 13, std.check, NameR, sys.ADR(cond_new), UpdateOptions, 1, key.AltD, std.d_enabled};
      END;
      WITH CurrentOptions^[DisplayDereferencePointer] DO
        ASSERT((opt = BoolEq) AND display);
        Lines^[Opt2Line[DisplayDereferencePointer]] := std.LINE{ 4, 14, std.check, NameR, sys.ADR(cond_new), UpdateOptions, 13, key.AltP, std.d_enabled};
      END;
      -- Radio buttons for radix option WholeHex
      NEW (Radix, 2);
      Radix^[0] := std.RADIO{50,10,'Hex', 1, key.AltH };
      Radix^[1] := std.RADIO{60,10,'Dec', 2, key.AltE };
      Lines^[Opt2Line[WholeHex]] := std.LINE{39, 10, std.radio , 'Radix', 0, 0, 0, 0, Radix, ToggleWholeHexOption, std.d_enabled};
      WITH CurrentOptions^[AutoDetectActualType] DO
        ASSERT((opt = BoolEq) AND display);
        Lines^[Opt2Line[AutoDetectActualType]] := std.LINE{41, 11, std.check, NameR, sys.ADR(cond_new), UpdateOptions, 4, key.AltO, std.d_enabled};
      END;
      WITH CurrentOptions^[UseSingleStructWindow] DO
        ASSERT((opt = BoolEq) AND display);
        Lines^[Opt2Line[UseSingleStructWindow]] := std.LINE{41, 12, std.check, NameR, sys.ADR(cond_new), UpdateOptions, 2, key.AltI, std.d_enabled};
      END;
      WITH CurrentOptions^[WarningBell] DO
        ASSERT((opt = BoolEq) AND display);
        Lines^[Opt2Line[WarningBell]] := std.LINE{41, 13, std.check, NameR, sys.ADR(cond_new), UpdateOptions, 9, key.AltB, std.d_enabled};
      END;
      WITH CurrentOptions^[Save_Opt_File] DO
        ASSERT((opt = BoolEq) AND display);
        Lines^[Opt2Line[Save_Opt_File]] := std.LINE{41, 14, std.check, NameR, sys.ADR(cond_new), UpdateOptions, 3, key.AltV, std.d_enabled};
      END;
      -- Use keyboard layout
      WITH CurrentOptions^[KbdFile] DO
        Lines^[Opt2Line[KbdFile]] := std.LINE{41, 15, std.check , NameR, sys.ADR(UseKbdOption), ToggleKbdLayoutOption, 5, key.AltK, std.d_enabled};
        ASSERT((opt = StrEq) AND display);
        Lines^[Opt2Line[KbdFile]+1] := std.LINE{45, 16, std.edit_str, sys.ADR(line_new), 25, std.d_enabled};
      END;
      free := Opt2Line[KbdFile]+1;
      INC (free);
      -- Reset to default
      Lines^[free] := std.LINE{ 2, 17, std.button, 'Reset to default', ResetToDefault, 14, key.AltU, std.d_enabled };
      INC (free);
      -- Save config
      Lines^[free] := std.LINE{23, 17, std.button, std.MESSAGE(act.ActionName[act.SaveConfig]), SaveConfigFromOptionsDialog, 11, key.AltG, std.d_enabled };
      INC (free);
      -- Frames
      Lines^[free] := std.LINE{ 2, 2, std.frame, 34, 6, crt.Single, std.d_enabled };
      INC (free);
      Lines^[free] := std.LINE{39, 2, std.frame, 34, 6, crt.Single, std.d_enabled };
      INC (free);
      Lines^[free] := std.LINE{ 2, 9, std.frame, 34, 6, crt.Single, std.d_enabled };
      INC (free);
      Lines^[free] := std.LINE{39, 9, std.frame, 34, 8, crt.Single, std.d_enabled };
      INC (free);
      -- Group titles
      Lines^[free] := std.LINE{ 6, 2, std.msg, " Execution options "     , std.d_enabled };
      INC (free);
      Lines^[free] := std.LINE{43, 2, std.msg, " Source options "        , std.d_enabled };
      INC (free);
      Lines^[free] := std.LINE{ 6, 9, std.msg, " Visualization options " , std.d_enabled };
      INC (free);
      Lines^[free] := std.LINE{43, 9, std.msg, " Configuration "         , std.d_enabled };
      INC (free);
      -- Prompts
      Lines^[free] := std.LINE{ 4, 3, std.msg, CurrentOptions^[Traced_Run_Delay].NameR, std.d_enabled };
      INC (free);
      Lines^[free] := std.LINE{ 4, 4, std.msg, CurrentOptions^[SSS_Delay].NameR, std.d_enabled };
     <* PUSH *>
     <* WOFF901+ *>
      INC (free);
     <* POP *>

      WITH size DO
        x1 := 2;  y1 := 1;
        x2 := 77; y2 := 23;
      END;
     <* END *>

      OptionsDialog := win.RegisterWindow(std.DialogProc,SIZE(std.DIALOG));
      ASSERT(OptionsDialog # win.Invalid_H);
      win.SetModal (OptionsDialog);
      win.SetMovable (OptionsDialog);
      win.SetHeader (OptionsDialog, mes.Options);
      win.SetWindowSize (OptionsDialog,size);

      p := std.PDIALOG(win.GetAMPtr(OptionsDialog));
      p^.curr     := 0;
      p^.on_error := win.Invalid_H;
      p^.Lines    := Lines;
      p^.action   := SaveCurrentOptions;
    END;

    -- Инициализация текущих значений
    FOR curr := MIN(OPTION_ELEM) TO MAX(OPTION_ELEM) DO
      WITH CurrentOptions^[curr] DO
        CASE opt OF
        | WholeEq : ws.CardToStr (var^, var_new);
        | BoolEq  : cond_new := cond^;
        | StrEq   : COPY(line^, line_new);
        END;
      END;
    END;
    p := std.PDIALOG(win.GetAMPtr(OptionsDialog));
    WITH p^ DO
      -- Setup radix value
      WITH Lines^[Opt2Line[WholeHex]] DO
        IF CurrentOptions^[WholeHex].cond^ THEN
          ractive := 0;
          rcurr := 0;
        ELSE
          ractive := 1;
          rcurr := 1;
        END;
      END;
      -- Setup keyboard layout
      IF CurrentOptions^[KbdFile].line^ = '' THEN
        Lines^[Opt2Line[KbdFile]].cactive^ := FALSE;
        Lines^[Opt2Line[KbdFile]+1].state := std.d_disabled;
      ELSE
        Lines^[Opt2Line[KbdFile]].cactive^ := TRUE;
        Lines^[Opt2Line[KbdFile]+1].state := std.d_enabled;
      END;
    END;

    eve.AddToTail (OptionsDialog, eve.Rise, 0);
  END;
  RETURN TRUE;
END InitOptWindow;
<* POP *>


VAR
  ConfigName: xs.String;

PROCEDURE UpdateConfigName;
BEGIN
  SaveConfig (ConfigName);
END UpdateConfigName;

PROCEDURE BrowseConfigName;
VAR
  buf  : xs.String;
BEGIN
  arg.ProgramName(ConfigName);
  fil.ExtractFileName(ConfigName, buf);
  fil.ChangeExtension(buf, CfgFileExt);
  red.Write(buf, ConfigName);
  brw.Browse(act.ActionName[act.SaveConfig], ConfigName, UpdateConfigName);
END BrowseConfigName;

PROCEDURE DefaultSaveConfig (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.SaveConfig);
  IF mode # act.mode_check THEN
    BrowseConfigName;
  END;
  RETURN TRUE;
END DefaultSaveConfig;

VAR
  KeyboardLayoutName: xs.String;

PROCEDURE UpdateKeyboardLayoutName;
BEGIN
  SaveKbd (KeyboardLayoutName);
END UpdateKeyboardLayoutName;

PROCEDURE BrowseKeyboardLayoutName;
BEGIN
  IF opt.KbdFile = '' THEN
    COPY('*.*', KeyboardLayoutName);
  ELSE
    COPY(opt.KbdFile, KeyboardLayoutName);
    fil.ChangeExtension(KeyboardLayoutName, KbdFileExt);
  END;
  brw.Browse(act.ActionName[act.SaveKeyboardLayout], KeyboardLayoutName, UpdateKeyboardLayoutName);
END BrowseKeyboardLayoutName;


PROCEDURE DefaultSaveKeyboardLayout (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.SaveKeyboardLayout);
  IF mode # act.mode_check THEN
    BrowseKeyboardLayoutName;
  END;
  RETURN TRUE;
END DefaultSaveKeyboardLayout;


VAR
  l   : CARDINAL;
  curr: OPTION_ELEM;

BEGIN
  act.IniAction(act.Pallette,           InitWindow);
  act.IniAction(act.Options,            InitOptWindow);
  act.IniAction(act.SaveConfig,         DefaultSaveConfig);
  act.IniAction(act.SaveKeyboardLayout, DefaultSaveKeyboardLayout);

  PalletteList := win.Invalid_H;
  ColorsList := win.Invalid_H;
  OptionsDialog := win.Invalid_H;

 <* IF DEST_K26 THEN *>
  NEW(CurrentOptions);
  CurrentOptions^[ Traced_Run_Delay         ] := OPTION{'Traced_Run_Delay',         'Задержка трассы, мс',       WholeEq, sys.ADR(dv.Delay)                     , dv.Delay                     , ""    , TRUE,  NIL                  };
  CurrentOptions^[ HilighCode               ] := OPTION{'HilighCode',               'Выделение кода',                  BoolEq,  sys.ADR(opt.CodeHilight)              , opt.CodeHilight              , FALSE , TRUE,  NIL                  };
  CurrentOptions^[ HilighCall               ] := OPTION{'HilighCall',               'Выделение вызовов процедур',      BoolEq,  sys.ADR(opt.CallHilight)              , opt.CallHilight              , FALSE , TRUE,  NIL };
  CurrentOptions^[ FullDisasm               ] := OPTION{'FullDisasm',               'Полный дизассемблер',                BoolEq,  sys.ADR(opt.DisasmMode)               , opt.DisasmMode               , FALSE , TRUE,  NIL                  };
  CurrentOptions^[ DumpDisplay              ] := OPTION{'DumpDisplay',              'Дизассемблер с дампом',       BoolEq,  sys.ADR(opt.Code)                     , opt.Code                     , FALSE , TRUE,  NIL                  };
  CurrentOptions^[ Save_Opt_File            ] := OPTION{'Save_Opt_File',            'Сохранять файл сеанса отладки',       BoolEq,  sys.ADR(opt.SaveOpt)                  , opt.SaveOpt                  , FALSE , TRUE,  NIL                  };
  CurrentOptions^[ WholeHex                 ] := OPTION{'WholeHex',                 'Целые в 16 с/с',         BoolEq,  sys.ADR(opt.WholeHex)                 , opt.WholeHex                 , FALSE , TRUE,  NIL                  };
  CurrentOptions^[ KbdFile                  ] := OPTION{'KbdFile',                  'Использовать файл настройки клавиатуры',             StrEq,   sys.ADR(opt.KbdFile), HIGH(opt.KbdFile)+1, std.MESSAGE(opt.KbdFile), ""    , TRUE,  ActionKbdFile        };
  CurrentOptions^[ TraceRegisters           ] := OPTION{'TraceRegisters',           'Трассировка регистров',                       BoolEq,  sys.ADR(opt.TraceRegisters), opt.TraceRegisters, opt.TraceRegisters,   TRUE,  TraceRegistersAction };
  CurrentOptions^[ IntStateSystem           ] := OPTION{'IntStateSystem',           'Системный режим',                  BoolEq,  sys.ADR(StateSystem), FALSE, FALSE, TRUE, ToggleInterpretatorState };
  CurrentOptions^[ IntStateTest             ] := OPTION{'IntStateTest',             'Тестовый режим',                   BoolEq,  sys.ADR(StateTest),   FALSE, FALSE, TRUE, ToggleInterpretatorState };
  CurrentOptions^[ IntStateStart            ] := OPTION{'IntStateStart',            'Непосредственный старт программы', BoolEq,  sys.ADR(StateStart),  FALSE, FALSE, TRUE, ToggleInterpretatorState };
  CurrentOptions^[ ShowAllModules           ] := OPTION{'ShowAllModules',           'Показывать все модули',                BoolEq,  sys.ADR(opt.ShowAllModules)           , opt.ShowAllModules           , FALSE , TRUE,  ActionShowAllModules };
  CurrentOptions^[ WarningBell              ] := OPTION{'WarningBell',              'Звуковой сигнал',                    BoolEq,  sys.ADR(opt.WarningBell)              , opt.WarningBell              , FALSE , TRUE,  NIL                  };
  CurrentOptions^[ UseSingleStructWindow    ] := OPTION{'UseSingleStructWindow',    'Одно окно для структур',         BoolEq,  sys.ADR(opt.UseSingleStructureWindow) , opt.UseSingleStructureWindow , FALSE , TRUE,  NIL                  };
  CurrentOptions^[ DumpType                 ] := OPTION{'DumpType',                 'Инициализация дампов',                       WholeEq, sys.ADR(opt.InitDumpType)             , opt.InitDumpType             , ""    , FALSE, NIL                  };
  CurrentOptions^[ ShowModuleWithoutSource  ] := OPTION{'ShowModuleWithoutSource',  'Показывать модули без исходных текстов', BoolEq, sys.ADR(opt.ShowModuleWithoutSource), opt.ShowModuleWithoutSource, FALSE, FALSE, NIL };
  CurrentOptions^[ DisplayDereferencePointer] := OPTION{'DisplayDereferencePointer','Разыменование указателей',            BoolEq,  sys.ADR(opt.DisplayDerefencePointer)  , opt.DisplayDerefencePointer  , FALSE , TRUE,  NIL                  };
 <* ELSIF DEST_XDS THEN *>
  TransliterateFromToStr := TransliterateFromToTable [Translit.nn];
  NEW(CurrentOptions);
  CurrentOptions^[ Traced_Run_Delay         ] := OPTION{'Traced_Run_Delay',         'Execution trace delay, ms',       WholeEq, sys.ADR(dv.Delay)                     , dv.Delay                       , ""    , TRUE,  NIL                   };
  CurrentOptions^[ HilighCode               ] := OPTION{'HilighCode',               'Code highlight',                  BoolEq,  sys.ADR(opt.CodeHilight)              , opt.CodeHilight                , FALSE , TRUE,  NIL                   };
  CurrentOptions^[ HilighCall               ] := OPTION{'HilighCall',               'Call highlight',                  BoolEq,  sys.ADR(opt.CallHilight)              , opt.CallHilight                , FALSE , TRUE,  ActionUpdateCallStack };
  CurrentOptions^[ FullDisasm               ] := OPTION{'FullDisasm',               'Full disasm mode',                BoolEq,  sys.ADR(opt.DisasmMode)               , opt.DisasmMode                 , FALSE , TRUE,  NIL                   };
  CurrentOptions^[ DumpDisplay              ] := OPTION{'DumpDisplay',              'Dump in disassembler mode',       BoolEq,  sys.ADR(opt.Code)                     , opt.Code                       , FALSE , TRUE,  NIL                   };
  CurrentOptions^[ Save_Opt_File            ] := OPTION{'Save_Opt_File',            'Save debug session layout',       BoolEq,  sys.ADR(opt.SaveOpt)                  , opt.SaveOpt                    , FALSE , TRUE,  NIL                   };
  CurrentOptions^[ WholeHex                 ] := OPTION{'WholeHex',                 'Show whole value in hex',         BoolEq,  sys.ADR(opt.WholeHex)                 , opt.WholeHex                   , FALSE , TRUE,  NIL                   };
  CurrentOptions^[ KbdFile                  ] := OPTION{'KbdFile',                  'Use keyboard layout',             StrEq,   sys.ADR(opt.KbdFile), HIGH(opt.KbdFile)+1, std.MESSAGE(opt.KbdFile)    , ""    , TRUE,  ActionKbdFile         };
  CurrentOptions^[ StopOnHardwareExceptionFirstChance] := OPTION{'StopOnHardwareExceptionFirstChance',        'Show exception at first try',     BoolEq,  sys.ADR(opt.ExceptionOnFirstChance)   , opt.ExceptionOnFirstChance     , FALSE , TRUE,  NIL                   };
  CurrentOptions^[ StopOnSoftwareException  ] := OPTION{'ShowSoftwareExceptions',   'Show software exceptions',         BoolEq,  sys.ADR(opt.ShowSoftwareException)    , opt.ShowSoftwareException      , FALSE , TRUE,  ActionShowSoftwareExceptions                   };
  CurrentOptions^[ SSS_Delay                ] := OPTION{'SSS_Delay',                'Debuggee selector delay, ms',     WholeEq, sys.ADR(opt.SSS_Delay)                , opt.SSS_Delay                  , ""    , TRUE,  NIL                   };
  CurrentOptions^[ Skip_Disasm              ] := OPTION{'Skip_Disasm',              'Never fall into disassembler',    BoolEq,  sys.ADR(opt.SkipDisasm)               , opt.SkipDisasm                 , FALSE , TRUE,  NIL                   };
  CurrentOptions^[ StripPathFromFullName    ] := OPTION{'StripPathFromFullName',    'Strip path from full name',       BoolEq,  sys.ADR(opt.StripPathFromFullName)    , opt.StripPathFromFullName      , FALSE , TRUE,  ActionStripFileName   };
  CurrentOptions^[ StripPathFromPartialName ] := OPTION{'StripPathFromPartialName', 'Strip path from partial name',    BoolEq,  sys.ADR(opt.StripPathFromPartialName) , opt.StripPathFromPartialName   , FALSE , TRUE,  ActionStripFileName   };
  CurrentOptions^[ ShowAllModules           ] := OPTION{'ShowAllModules',           'Show all modules',                BoolEq,  sys.ADR(opt.ShowAllModules)           , opt.ShowAllModules             , FALSE , TRUE,  ActionShowAllModules  };
  CurrentOptions^[ WarningBell              ] := OPTION{'WarningBell',              'Warning bell',                    BoolEq,  sys.ADR(opt.WarningBell)              , opt.WarningBell                , FALSE , TRUE,  NIL                   };
  CurrentOptions^[ UseSingleStructWindow    ] := OPTION{'UseSingleStructWindow',    'Single structure window',         BoolEq,  sys.ADR(opt.UseSingleStructureWindow) , opt.UseSingleStructureWindow   , FALSE , TRUE,  NIL                   };
  CurrentOptions^[ DumpType                 ] := OPTION{'DumpType',                 'Dump type',                       WholeEq, sys.ADR(opt.InitDumpType)             , opt.InitDumpType               , ""    , FALSE, NIL                   };
  CurrentOptions^[ ShowModuleWithoutSource  ] := OPTION{'ShowModuleWithoutSource',  'Show modules without source',     BoolEq,  sys.ADR(opt.ShowModuleWithoutSource)  , opt.ShowModuleWithoutSource    , FALSE , TRUE,  NIL                   };
  CurrentOptions^[ AutoDetectActualType     ] := OPTION{'AutoDetectActualType',     'Auto-detect actual type',         BoolEq,  sys.ADR(opt.AutoDetectActualType)     , opt.AutoDetectActualType       , FALSE , TRUE,  NIL                   };
  CurrentOptions^[ DisplayDereferencePointer] := OPTION{'DisplayDereferencePointer','Dereference pointers',            BoolEq,  sys.ADR(opt.DisplayDerefencePointer)  , opt.DisplayDerefencePointer    , FALSE , TRUE,  NIL                   };
  CurrentOptions^[ FrameImageSingle         ] := OPTION{'FrameImageSingle',         'Frame single',                    StrEq,   sys.ADR(crt.Frames[crt.Single])       , HIGH(crt.Frames[crt.Single])+1 , std.MESSAGE(crt.Frames[crt.Single]) , "", FALSE, NIL };
  CurrentOptions^[ FrameImageDouble         ] := OPTION{'FrameImageDouble',         'Frame double',                    StrEq,   sys.ADR(crt.Frames[crt.Double])       , HIGH(crt.Frames[crt.Double])+1 , std.MESSAGE(crt.Frames[crt.Double]) , "", FALSE, NIL };
  CurrentOptions^[ FrameImageMove           ] := OPTION{'FrameImageMove',           'Frame move',                      StrEq,   sys.ADR(crt.Frames[crt.Move])         , HIGH(crt.Frames[crt.Move])+1   , std.MESSAGE(crt.Frames[crt.Move])   , "", FALSE, NIL };
  CurrentOptions^[ TransliterateFromTo      ] := OPTION{'TransliterateFromTo',      'Transliterate sources from to',   StrEq,   sys.ADR(TransliterateFromToStr)       , HIGH(TransliterateFromToStr)   , "", "", FALSE, ActionTranslitFromTo  };
 <* END *>
  CurrentOptions^[MergeEqualTypes           ] := OPTION{'MergeEqualTypes',          ' Merge equal types',              BoolEq,  sys.ADR(opt.MergeEqualTypes)          , opt.MergeEqualTypes            , FALSE , TRUE,  NIL                   };
  Shown_Options     := 0;
  Shown_Options_Len := 0;
  FOR curr := MIN(OPTION_ELEM) TO MAX(OPTION_ELEM) DO
    WITH CurrentOptions^[curr] DO
      IF display THEN
        INC(Shown_Options);
        l := LENGTH(NameR);
        IF Shown_Options_Len < l THEN Shown_Options_Len := l; END;
      END;
    END;
  END;
  exc.AllocateSource(source);
FINALLY
  DISPOSE(CurrentOptions);
 <* IF DEFINED (xd_debug) & xd_debug THEN *>
  IF opt.Debug(opt.Another) THEN SaveKbd(''); END;
 <* END *>
END DlgCfg.
