<* Storage+ *>

IMPLEMENTATION MODULE DlgBrows;

IMPORT sys := SYSTEM;
IMPORT fmt := FormStr;
IMPORT fs  := FileSys;
IMPORT str := Strings;

IMPORT xs  := xStr;

IMPORT fil := File;
IMPORT crt := CRT;
IMPORT key := Keys;
IMPORT std := Dlg_Std;
IMPORT win := Dlg_Win;
IMPORT act := Dlg_Acts;
IMPORT eve := DlgEvent;
IMPORT dlt := DlgTypes;



TYPE
  ENTRYNAME = xs.String;

  ENTRY = RECORD
            info: fs.Entry;
            name: ENTRYNAME;
          END;

  PAENTRY = POINTER TO ARRAY OF ENTRY;

  ENTRYLIST  = RECORD
                 Entries: PAENTRY;
                 Count  : CARDINAL;
               END;


VAR
  FList : ENTRYLIST;
  DList : ENTRYLIST;

  FilName : ENTRYNAME;
  DirName : ENTRYNAME;
  DrvName : ENTRYNAME;
  DrvLabel: ENTRYNAME;

  PFilList: std.PLIST;
  PDirList: std.PLIST;
  PDrvList: std.PLIST;



CONST
  PatternSearchRedraw = 10;

TYPE
  BROWSE_FIELD = ( BF_FileName
                 , BF_FileList
                 , BF_DirList
                 , BF_DriveName
                 , BF_Ok
                 , BF_Cancel );


VAR
  HBrowse: win.HWND;

  PFName : xs.txt_ptr;
  Action : PROC;

  BrowseListAttr : crt.LIST_ATTR;
  BrowseEmptyList: std.LIST;
  BrowseField    : BROWSE_FIELD;

  PatternSearch: std.MESSAGE;


PROCEDURE CreateFileList (path-: ARRAY OF CHAR);
VAR
  dir : fs.Directory;
  info: fs.Entry;
BEGIN
  WITH FList DO
    Count := 0;
    fs.OpenDir (dir, path, info);
    WHILE info.done DO
      IF NOT info.isDir THEN
        INC (Count);
      END;
      fs.NextDirEntry (dir, info);
    END;
    fs.CloseDir (dir);
    IF Count = 0 THEN
      RETURN;
    END;
    IF Entries = NIL THEN
      NEW (Entries, Count);
      ASSERT (Entries # NIL);
    ELSIF Count > HIGH(Entries^) THEN
      DISPOSE (Entries);
      NEW (Entries, Count);
      ASSERT (Entries # NIL);
    END;
    Count := 0;
    fs.OpenDir (dir, path, info);
    WHILE info.done DO
      IF NOT info.isDir THEN
        Entries^[Count].info := info;
        fs.GetName (dir, Entries^[Count].name);
        INC(Count);
      END;
      fs.NextDirEntry (dir, info);
    END;
    fs.CloseDir (dir);
  END;
END CreateFileList;


<* PUSH *>
<* WOFF301+ *>
PROCEDURE FileName (hwnd: crt.HWND; num: CARDINAL; VAR str: ARRAY OF CHAR);
<* POP *>
BEGIN
  WITH FList DO
    IF (Entries # NIL) AND (num < Count) THEN
      fmt.print(str, '%s', Entries^[num].name);
    END;
  END;
END FileName;


CONST
  UpDir = '< Up >';


PROCEDURE CreateDirList (path-: ARRAY OF CHAR);
VAR
  drv : ENTRYNAME;
  dir : ENTRYNAME;
  fnm : ENTRYNAME;
  name: ENTRYNAME;
  root: BOOLEAN;
  Dir : fs.Directory;
  info: fs.Entry;
BEGIN
  fil.SplitPath(path, drv, dir, fnm);
  root := dir = '\';
  WITH DList DO
    Count := 0;
    fs.OpenDir (Dir, path, info);
    WHILE info.done DO
      IF info.isDir THEN
        fs.GetName (Dir, name);
        IF (name # ".") AND NOT (root AND (name = "..")) THEN
          INC (Count);
        END;
      END;
      fs.NextDirEntry (Dir, info);
    END;
    fs.CloseDir (Dir);
    IF Count = 0 THEN
      RETURN;
    END;
    IF Entries = NIL THEN
      NEW (Entries, Count);
      ASSERT (Entries # NIL);
    ELSIF Count > HIGH(Entries^) THEN
      DISPOSE (Entries);
      NEW (Entries, Count);
      ASSERT (Entries # NIL);
    END;
    Count := 0;
    fs.OpenDir (Dir, path, info);
    WHILE info.done DO
      IF info.isDir THEN
        fs.GetName (Dir, name);
        IF (name # ".") AND NOT (root AND (name = "..")) THEN
          IF name = '..' THEN
            COPY (UpDir, name);
          END;
          Entries^[Count].info := info;
          Entries^[Count].name := name;
          INC (Count);
        END;
      END;
      fs.NextDirEntry (Dir, info);
    END;
    fs.CloseDir (Dir);
  END;
END CreateDirList;


<* PUSH *>
<* WOFF301+ *>
PROCEDURE DirectoryName (hwnd: crt.HWND; num: CARDINAL; VAR str: ARRAY OF CHAR);
<* POP *>
BEGIN
  WITH DList DO
    IF (Entries # NIL) AND (num < Count) THEN
      fmt.print(str, '%s', Entries^[num].name);
    END;
  END;
END DirectoryName;


PROCEDURE InitDrvDirFile (init_path-: ARRAY OF CHAR);
VAR
  drive: CHAR;

  PROCEDURE MakeCurrDrive;
  BEGIN
    ASSERT (fs.GetDrive (drive));
    DrvName[0] := drive;
    DrvName[1] := ':';
    DrvName[2] := 0C;
    IF NOT fs.GetLabel (drive, DrvLabel) THEN
      COPY ("", DrvLabel);
    END;
  END MakeCurrDrive;

  PROCEDURE MakeCurrDir;
  VAR
    l: CARDINAL;
  BEGIN
    fs.GetCDName (DirName);
--    xs.Uppercase(DirName);
    l := LENGTH(DirName);
    IF l > 3 THEN
      xs.Append ('\', DirName);
    END;
  END MakeCurrDir;


VAR
  path: xs.String;
  res : BOOLEAN;

BEGIN
  COPY (init_path, path);
--  xs.Uppercase (path);
  fil.SplitPath (path, DrvName, DirName, FilName);
  IF DrvName = '' THEN
    res := fs.GetDrive (drive);
  ELSE
    drive := DrvName[0];
    res := fs.SetDrive (drive);
  END;
  IF res THEN
    IF DirName # '' THEN
      fmt.print (path, '%s%s', DrvName, DirName);
      IF LENGTH(path) > 3 THEN
        path[LENGTH(path)-1] := 0C;
      END;
      sys.EVAL (fs.SetCD (path));
    END;
  END;
  MakeCurrDrive;
  MakeCurrDir;

  fmt.print (path, '%s*.*', DirName);
  CreateDirList (path);

  IF FilName = '' THEN
    COPY('*.*', FilName);
  END;
  fmt.print (path, '%s%s', DirName, FilName);
  CreateFileList(path);

  IF PFilList = NIL THEN
    NEW (PFilList);
    ASSERT (PFilList#NIL);
  END;
  PFilList^ := BrowseEmptyList;
  PFilList^.N := FList.Count;
  PFilList^.locator := FileName;

  IF PDirList = NIL THEN
    NEW(PDirList);
    ASSERT(PDirList#NIL);
  END;
  PDirList^ := BrowseEmptyList;
  PDirList^.N := DList.Count;

  IF PDrvList = NIL THEN
    NEW (PDrvList);
    ASSERT (PDrvList#NIL);
  END;
  PDrvList^ := BrowseEmptyList;
  PDrvList^.N := 0;
END InitDrvDirFile;



(*
VAR
  StoredCurrentDrive    : CHAR;
  StoredCurrentDirectory: ENTRYNAME;
*)


PROCEDURE BrowseHandler (hwnd: win.HWND; msg: eve.MSG);


(*
  -- store current drive and directory before open dialog
  PROCEDURE store_curr_dir;
  BEGIN
    sys.EVAL (fs.GetDrive(StoredCurrentDrive));
    fs.GetCDName (StoredCurrentDirectory);
  END store_curr_dir;

  -- restore old drive and directory after close dialog
  PROCEDURE restore_curr_dir;
  BEGIN
    sys.EVAL (fs.SetDrive(StoredCurrentDrive));
    sys.EVAL (fs.SetCD(StoredCurrentDirectory));
  END restore_curr_dir;
*)


VAR
  p   : std.PDIALOG;
  size: crt.SZ;
  f_sz: crt.SZ;
  fld : BROWSE_FIELD;
  last: CARDINAL;
  len : CARDINAL;
  lpat: CARDINAL;
  Lctr: std.MESSAGE;
  list: std.PLIST;
  Name: std.LCTR_STR;


  PROCEDURE write_line (num: CARDINAL);
  VAR
    fname: std.MESSAGE;
  BEGIN
    CASE fld OF
    | BF_FileList, BF_DirList:
      WITH list^ DO
        IF num < N THEN
          Name(hwnd, num, fname);
          fname[pos+19] := '';
          crt.SetPos(p^.Lines^[ORD(fld)].x+1, p^.Lines^[ORD(fld)].y+num-frame+1);
          crt.WrStrFromPos(hwnd, fname, Colors^[crt.List_Line], pos);
        END;
      END;
    ELSE
    END;
  END write_line;


  PROCEDURE lite_line (attr: crt.ATTR);
  BEGIN
    CASE fld OF
    | BF_FileList, BF_DirList:
      WITH p^.Lines^[ORD(fld)] DO
        WITH list^ DO
          crt.LitePart(hwnd, y+curr-frame+1, x+1, x+frame_dx, attr);
        END;
      END;
    ELSE
    END;
  END lite_line;


  PROCEDURE curr_field;
  BEGIN
    len  := 0;
    lpat := 0;
    list := NIL;
    Name := NIL;
    CASE fld OF
    | BF_FileList, BF_DirList:
      WITH f_sz DO
        x1 := p^.Lines^[ORD(fld)].x + size.x1 + 1;
        y1 := p^.Lines^[ORD(fld)].y + size.y1 + 1;
        x2 := x1 + p^.Lines^[ORD(fld)].frame_dx - 2;
        y2 := y1 + p^.Lines^[ORD(fld)].frame_dy - 2;
      END;
      len  := f_sz.y2-f_sz.y1+1;
      lpat := f_sz.x2-f_sz.x1+1;
      IF fld = BF_FileList THEN
        list := PFilList;
        Name := FileName;
      ELSE
        list := PDirList;
        Name := DirectoryName;
      END;
    | BF_FileName, BF_DriveName:
      WITH f_sz DO
        x1 := p^.Lines^[ORD(fld)].x + size.x1;
        y1 := p^.Lines^[ORD(fld)].y + size.y1;
        x2 := x1 + p^.Lines^[ORD(fld)].len;
        y2 := y1;
      END;
    | BF_Ok:
      WITH size DO
        f_sz.x1 := x1 + (x2-x1-22) DIV 2;
        f_sz.x2 := f_sz.x1 + 7;
        f_sz.y1 := y2-2;
        f_sz.y2 := f_sz.y1;
      END;
    | BF_Cancel:
      WITH size DO
        f_sz.x1 := x1 + (x2-x1-24) DIV 2 + 12;
        f_sz.x2 := f_sz.x1 + 9;
        f_sz.y1 := y2-2;
        f_sz.y2 := f_sz.y1;
      END;
    END;
  END curr_field;


  PROCEDURE paint_list;
  VAR
    i: CARDINAL;
  BEGIN
    curr_field;
    CASE fld OF
    | BF_FileList, BF_DirList:
      IF (msg.par # 3) AND (msg.par # 4) THEN
        crt.FillWindowSize(hwnd, f_sz, ' ', list^.Colors^[crt.List_Background])
      END;
      WITH list^ DO
        IF N > 0 THEN
          CASE msg.par OF
          | 3: IF fld = BrowseField THEN
                 write_line(curr);
                 lite_line(Colors^[crt.List_CurrentLine]);
               END;
          | 4: IF fld = BrowseField THEN
                 lite_line(Colors^[crt.List_Background]);
                 write_line(curr);
               END;
          ELSE
            last := std.Min(frame+len-1, N-1);
            FOR i := frame TO last DO write_line(i); END;
            IF fld = BrowseField THEN
              lite_line(Colors^[crt.List_CurrentLine]);
              IF PatternSearch # '' THEN
                xs.Extract(PatternSearch, 0, lpat, Lctr);
                WITH p^.Lines^[ORD(fld)] DO
                  crt.SetPos(x+1, y+f_sz.y2-f_sz.y1+2);
                END;
                crt.WrStr(hwnd, Lctr, Colors^[crt.List_Background]);
              END;
            END;
          END;
        END;
      END;
    ELSE
    END;
  END paint_list;


  PROCEDURE list_locator (k: CARDINAL);
  VAR
    b,l,j,i: CARDINAL;
    name   : xs.String;
    up     : xs.String;
    ch     : CHAR;
  BEGIN
    curr_field;
    WITH list^ DO
      IF N = 0 THEN RETURN; END;
      <* PUSH *>
      <* WOFF312+ *>
      LOOP
      <* POP *>
        IF PatternSearch = '' THEN
          b := 0;
        ELSE
          b := curr;
          xs.Uppercase(PatternSearch);
        END;
        l := LENGTH(PatternSearch);
        j := l;
        IF k = key.BackSpace THEN
          IF l > 0 THEN DEC(l); b := 0; END;
        ELSIF (k < 255) AND (l < HIGH(PatternSearch)) THEN
          ch := xs.UpChar(CHR(k));
          IF ch IN xs.CHARSET{'A'..'Z', '0'..'9', '_', '.'} THEN
            PatternSearch[l] := ch;
            INC(l);
          ELSE
            EXIT;
          END;
        ELSE
          EXIT;
        END;
        PatternSearch[l] := 0C;
        FOR i := b TO N-1 DO
          Name(hwnd, i, name);
          xs.Extract(name, 0, l, up);
          xs.Uppercase(up);
          IF (up = PatternSearch) THEN
            xs.Extract(name, 0, l, PatternSearch);
            curr := i;
            INC(f_sz.y2,2);
            std.Normalize(f_sz, curr, frame, N);
            eve.AddToTail(hwnd, eve.Redraw, PatternSearchRedraw);
            RETURN;
          END;
        END;
        EXIT;
      END;
      PatternSearch[j] := '';
    END;
  END list_locator;


  PROCEDURE paint_other;
  BEGIN
    crt.SetPos(26, 3);
    crt.WrNChar(hwnd, size.x2-size.x1-27, ' ', crt.DialogAttr[crt.Dialog_Message]);
    crt.SetPos(26, 3);
    IF LENGTH(DirName) <= 20 THEN
      crt.WrStr(hwnd, DirName, crt.DialogAttr[crt.Dialog_Message]);
    ELSE
      crt.WrStrFromPos(hwnd, DirName, crt.DialogAttr[crt.Dialog_Message], LENGTH(DirName)-20);
      crt.WrStr(hwnd, '...', crt.DialogAttr[crt.Dialog_Message]);
    END;
    crt.SetPos(35, 16);
    crt.WrStr(hwnd, DrvLabel, crt.DialogAttr[crt.Dialog_Message]);
  END paint_other;


VAR
  Msg  : eve.MSG;
  curr : CARDINAL;
  path : ENTRYNAME;
  dir  : ENTRYNAME;
  drive: ENTRYNAME;
  file : ENTRYNAME;
  ok   : BOOLEAN;
  flush: BOOLEAN;


  PROCEDURE get_field (field: BROWSE_FIELD);
  BEGIN
    CASE field OF
    | BF_FileName, BF_DriveName:
      Msg.hwnd := hwnd;
      Msg.ID := eve.KbHit;
      Msg.par := key.Enter;
      WITH p^.Lines^[ORD(field)] DO
        std.LineEditor(hwnd, Msg, 0, 0, e_str, len, TRUE);
      END;
    ELSE
    END;
  END get_field;


  PROCEDURE update_field (field: BROWSE_FIELD);
  BEGIN
    CASE field OF
    | BF_FileName, BF_DriveName:
      Msg.hwnd := hwnd;
      Msg.ID := eve.Rise;
      Msg.par := 0;
      WITH p^.Lines^[ORD(field)] DO
        std.LineEditor(hwnd, Msg, 0, 0, e_str, len, TRUE);
      END;
    ELSE
    END;
  END update_field;


  PROCEDURE change_drive (drive_letter: CHAR): BOOLEAN;
  BEGIN
    IF ('A' <= drive_letter) AND (drive_letter <= 'Z') THEN
      IF fs.SetDrive (drive_letter) THEN
        fmt.print(path, '%c:', drive_letter);
        InitDrvDirFile(path);
        update_field(BF_DriveName);
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END change_drive;


  PROCEDURE find_wildcard (s-: ARRAY OF CHAR): BOOLEAN;
  VAR
    f1, f2: BOOLEAN;
    p: CARDINAL;
  BEGIN
    <* PUSH *>
    <* WOFF903+ *>
    p := xs.CharPos (s, '*', f1);
    p := xs.CharPos (s, '?', f2);
    <* POP *>
    RETURN f1 OR f2;
  END find_wildcard;


VAR
  xm  : CARDINAL;
  ym  : CARDINAL;


  PROCEDURE check_mouse (size: crt.SZ): BOOLEAN;
  BEGIN
    WITH size DO
      RETURN (x1 <= xm) AND (xm <= x2) AND (y1 <= ym) AND (ym <= y2);
    END;
  END check_mouse;


BEGIN
  p := std.PDIALOG(win.GetAMPtr(hwnd));
  size := win.GetWindowSize(hwnd);
  CASE msg.ID OF
  | eve.Rise:
--    store_curr_dir;
    InitDrvDirFile(PFName^);
    BrowseField := BF_FileName;
    std.DialogProc(hwnd, msg);

  | eve.Hide:
    std.DialogProc(hwnd, msg);
--    restore_curr_dir;

  | eve.Paint, eve.Redraw:
    IF msg.par = PatternSearchRedraw THEN
      msg.par := 0;
    ELSE
      PatternSearch := '';
    END;
    IF msg.par = 0 THEN
      flush := eve.InFlush;
      eve.InFlush := FALSE;
      curr := p^.curr;
      CASE BrowseField OF
      | BF_FileName:
      | BF_DriveName:
        Msg := msg;
        Msg.ID := eve.KbHit;
        Msg.par := key.Home;
        WITH p^.Lines^[ORD(BF_DriveName)] DO
          std.LineEditor(hwnd, Msg, 0, 0, e_str, len, TRUE);
        END;
      | BF_Ok:
        p^.curr := HIGH(p^.Lines^)+1;
      | BF_Cancel:
        p^.curr := HIGH(p^.Lines^)+2;
      ELSE
        p^.curr := MAX(CARDINAL);
      END;
      Msg := msg;
      Msg.ID := eve.Paint;
      std.DialogProc(hwnd, Msg);
      eve.InFlush := flush;
      p^.curr := curr;
      fld := BF_FileList;
      paint_list;
      fld := BF_DirList;
      paint_list;
      paint_other;
      IF msg.ID = eve.Redraw THEN crt.UpdateRect(size) END;
    ELSE
      fld := BrowseField;
      paint_list;
      IF msg.ID = eve.Redraw THEN crt.UpdateRect(f_sz) END;
    END;

  | eve.KbHit:
    CASE msg.par OF
    | key.Esc:
      std.DialogProc(hwnd, msg);

    | key.Ins:
      std.LineEditor(hwnd, msg, 0, 0, NIL, 0, FALSE);

    | key.BackSpace:
      CASE BrowseField OF
      | BF_FileList, BF_DirList:
        fld := BrowseField;
        list_locator(msg.par);
      | BF_DriveName:
      ELSE
        std.DialogProc(hwnd, msg);
      END;

    | key.Enter:
      CASE BrowseField OF
      | BF_FileName:
        get_field(BF_FileName);
        fil.SplitPath(FilName, drive, dir, file);
        IF find_wildcard (FilName) OR (FilName = '') OR (drive # '') OR (dir # '') THEN
          -- Найдены wildcard или путь -> обновим список файлов
          IF drive # '' THEN
            COPY(FilName, path);
          ELSIF dir # '' THEN
            IF dir[0] = '\' THEN
              str.Delete(dir, 0, 1);
            END;
            fmt.print(path, '%s%s%s', DirName, dir, file);
          ELSE
            fmt.print(path, '%s%s', DirName, file);
          END;
          InitDrvDirFile(path);
          update_field(BF_FileName);
          eve.AddToTail(hwnd, eve.Redraw, 0);
        ELSE
          -- Не нашлось wildcard -> файл выбран
          BrowseField := BF_Ok;
          eve.AddToTail(hwnd, eve.KbHit, key.Enter);
        END;

      | BF_FileList:
        fld := BF_FileList;
        curr_field;
        WITH list^ DO
          IF N # 0 THEN
            Name (hwnd, curr, FilName);
            update_field(BF_FileName);
          END;
        END;
        BrowseField := BF_FileName;
        eve.AddToTail(hwnd, eve.KbHit, key.Enter);

      | BF_DirList:
        DirectoryName(hwnd, PDirList^.curr, dir);
        IF NOT find_wildcard(FilName) THEN COPY('', FilName); END;
        IF dir = UpDir THEN
          str.FindPrev('\', DirName, LENGTH(DirName)-2, ok, curr);
          ASSERT(ok);
          xs.Extract(DirName, 0, curr+1, dir);
          fmt.print(path ,'%s%s', dir, FilName);
        ELSE
          fmt.print(path ,'%s%s\\%s', DirName, dir, FilName);
        END;
        InitDrvDirFile(path);
        update_field(BF_FileName);
        eve.AddToTail(hwnd, eve.Redraw, 0);

      | BF_Ok:
        IF find_wildcard (FilName) THEN
          BrowseField := BF_FileName;
          p^.curr := ORD(BF_FileName);
          update_field(BF_FileName);
          eve.AddToTail(hwnd, eve.Redraw, 0);
        ELSE
          std.DialogProc(hwnd, msg);
        END;

      ELSE
        std.DialogProc(hwnd, msg);
      END;

    | key.Tab:
      CASE BrowseField OF
      | BF_FileName:
        fld := BF_FileList;
        curr_field;
        WITH list^ DO
          IF N # 0 THEN
            Name(hwnd, curr, FilName);
            update_field(BF_FileName);
          END;
        END;
        eve.AddToTail(hwnd, eve.KbHit, key.Home);
      | BF_FileList:
        eve.AddToTail(hwnd, eve.KbHit, key.Home);
      | BF_DirList:
        std.DialogProc(hwnd, msg);
        eve.AddToTail(hwnd, eve.KbHit, key.Home);
      ELSE
        std.DialogProc(hwnd, msg);
      END;
      IF BrowseField = MAX(BROWSE_FIELD) THEN
        BrowseField := MIN(BROWSE_FIELD);
      ELSE
        INC(BrowseField);
      END;

    | key.Home:
      CASE BrowseField OF
      | BF_FileName, BF_DriveName:
        std.DialogProc(hwnd, msg);
      | BF_FileList, BF_DirList:
        fld := BrowseField;
        curr_field;
        list^.pos := 0;
        eve.AddToTail(hwnd, eve.Redraw, 0);
      ELSE
      END;

    | key.End, key.Del:
      CASE BrowseField OF
      | BF_FileName:
        std.DialogProc(hwnd, msg);
      ELSE
      END;

    | key.Up, key.Down
    , key.PgUp, key.PgDn
    , key.CtrlPgUp, key.CtrlPgDn
    , key.CtrlHome, key.CtrlEnd:
      CASE BrowseField OF
      | BF_FileName:
        IF msg.par = key.Down THEN
          eve.AddToTail(hwnd, eve.KbHit, key.Tab);
        END;

      | BF_FileList:
        fld := BrowseField;
        curr_field;
        WITH list^ DO
          IF N = 0 THEN RETURN; END;
          IF (curr = 0) AND (msg.par = key.Up) THEN
            BrowseField := BF_FileName;
            eve.AddToTail(hwnd, eve.Redraw, 0);
            RETURN;
          END;
          IF N < 2 THEN RETURN; END;
          eve.AddToTail(hwnd, eve.Paint, 4);
          eve.Flush;
          <* PUSH *>
          <* WOFF903+ *>
          IF std.Shift(msg.par, N, f_sz.y2-f_sz.y1+1, curr, frame) THEN END;
          <* POP *>
          Name(hwnd, curr, FilName);
          update_field(BF_FileName);
          eve.AddToTail(hwnd, eve.Redraw, 0);
        END;

      | BF_DirList:
        fld := BrowseField;
        curr_field;
        WITH list^ DO
          IF N < 2 THEN RETURN; END;
          eve.AddToTail(hwnd, eve.Paint, 4);
          eve.Flush;
          IF std.Shift(msg.par, N, f_sz.y2-f_sz.y1+1, curr, frame) THEN
            eve.AddToTail(hwnd, eve.Redraw, 3);
          ELSE
            eve.AddToTail(hwnd, eve.Redraw, 0);
          END;
        END;
      ELSE
      END;

    | key.Left, key.Right:
      CASE BrowseField OF
      | BF_FileList, BF_DirList:
        fld := BrowseField;
        curr_field;
        WITH list^ DO
          IF msg.par = key.Left THEN
            IF pos # 0 THEN DEC(pos); END;
          ELSE
            INC(pos);
          END;
        END;
        eve.AddToTail(hwnd, eve.Redraw, 0);
      | BF_FileName:
        std.DialogProc(hwnd, msg);
      ELSE
      END;

    ELSE
      CASE BrowseField OF
      | BF_FileName:
        std.DialogProc(hwnd, msg);
      | BF_DriveName:
        IF (msg.par <= ORD(MAX(CHAR))) AND change_drive(CAP(CHAR(msg.par))) THEN
          eve.AddToTail(hwnd, eve.Redraw, 0);
        END;
      | BF_FileList, BF_DirList:
        fld := BrowseField;
        list_locator(msg.par);
      ELSE
      END
    END;

  | eve.Mouse_Pressed, eve.Mouse_Moved, eve.Mouse_Released, eve.Mouse_Dbl:
    ASSERT(win.GetRelMouse(hwnd, msg, xm, ym));
    CASE crt.GetControl (size, xm, ym, dlt.WND_CTRL_SET{dlt.WinCtrl_Close}) OF
    | dlt.WinCtrl_Close:
      IF msg.ID = eve.Mouse_Pressed THEN
        eve.AddToTail(hwnd, eve.Hide, 0);
      END;
    ELSE
      ASSERT(win.GetMouse(msg, xm, ym));
      IF NOT std.CheckFrame(size, xm, ym) THEN
        fld := MIN(BROWSE_FIELD);
        LOOP
          curr_field;
          ok := check_mouse (f_sz);
          IF ok OR (fld = MAX(BROWSE_FIELD)) THEN EXIT; END;
          INC(fld);
        END;
        IF ok THEN
          CASE fld OF
          | BF_FileName, BF_DriveName:
            IF msg.ID = eve.Mouse_Pressed THEN
              std.DialogProc(hwnd, msg);
              BrowseField := fld;
            END;
          | BF_FileList, BF_DirList:
            CASE msg.ID OF
            | eve.Mouse_Pressed, eve.Mouse_Moved, eve.Mouse_Dbl:
              IF (BrowseField # fld) AND (msg.ID = eve.Mouse_Moved) THEN RETURN; END;
              IF fld = BF_FileList THEN
                get_field(BrowseField);
                p^.curr := ORD(BF_FileName);
                update_field(BF_FileName);
              END;
              BrowseField := fld;
              eve.AddToTail(hwnd, eve.Redraw, 0);
              DEC(xm, f_sz.x1-1);
              DEC(ym, f_sz.y1-1);
              WITH list^ DO
                IF frame + ym <= N THEN
                  curr := frame + (ym-1);
                  IF BrowseField = BF_FileList THEN
                    Name(hwnd, curr, FilName);
                    update_field(BF_FileName);
                  END;
                  IF msg.ID = eve.Mouse_Dbl THEN
                    eve.AddToTail(hwnd, eve.KbHit, key.Enter);
                  ELSIF msg.ID = eve.Mouse_Moved THEN
                    IF (frame # 0) AND (curr = frame) THEN
                      eve.AddToTail(hwnd, eve.KbHit, key.Up);
                    ELSIF (curr+1 # N) AND (curr+1 = frame+len) THEN
                      eve.AddToTail(hwnd, eve.KbHit, key.Down);
                    END;
                  END;
                END;
              END;
            ELSE
            END;
          | BF_Ok, BF_Cancel:
            curr := p^.curr;
            std.DialogProc(hwnd, msg);
            IF curr # p^.curr THEN BrowseField := fld; END;
          END;
        END;
      ELSE
        std.DialogProc(hwnd, msg);
      END;
    END;
  ELSE
  END;
END BrowseHandler;


PROCEDURE MakeName (hwnd: crt.HWND): BOOLEAN;
VAR
  fname: ENTRYNAME;
BEGIN
  ASSERT(hwnd=HBrowse);
  fmt.print(fname, '%s%s', DirName, FilName);
  COPY(fname, PFName^);
  eve.AddToTail(hwnd, eve.Hide, 0);
  Action();
  RETURN TRUE;
END MakeName;


PROCEDURE Browse (header: ARRAY OF CHAR; VAR path: ARRAY OF CHAR; action: PROC);

  PROCEDURE InitListAttr;
  BEGIN
    BrowseListAttr [ crt.List_Background   ] := crt.DialogAttr[crt.Dialog_Background];
    BrowseListAttr [ crt.List_Frame        ] := crt.DialogAttr[crt.Dialog_Background];
    BrowseListAttr [ crt.List_Header       ] := crt.DialogAttr[crt.Dialog_Background];
    BrowseListAttr [ crt.List_ActiveHeader ] := crt.DialogAttr[crt.Dialog_Background];
    BrowseListAttr [ crt.List_Line         ] := crt.DialogAttr[crt.Dialog_Background];
    BrowseListAttr [ crt.List_CurrentLine  ] := crt.DialogAttr[crt.Dialog_ActiveEditor];
  END InitListAttr;

VAR
  lines: std.PLINES;
  pdial: std.PDIALOG;
  size : crt.SZ;
  hdr  : std.MESSAGE;
BEGIN
  IF HBrowse = win.Invalid_H THEN
    NEW(lines, 7);

    lines^[ORD(BF_FileName) ] := std.LINE{  3, 3, std.edit_str, sys.ADR(FilName), 19, std.d_enabled};
    lines^[ORD(BF_FileList) ] := std.LINE{  2, 4, std.frame, 20,12, crt.Single, std.d_enabled};
    lines^[ORD(BF_DirList)  ] := std.LINE{ 25, 4, std.frame, 20,10, crt.Single, std.d_enabled};
    lines^[ORD(BF_DriveName)] := std.LINE{ 32,16, std.edit_str, sys.ADR(DrvName), 02, std.d_enabled};
    lines^[4                ] := std.LINE{  3, 2, std.msg, 'File name', std.d_enabled};
    lines^[5                ] := std.LINE{ 26, 2, std.msg, 'Directories', std.d_enabled};
    lines^[6                ] := std.LINE{ 26,16, std.msg, 'Drive', std.d_enabled};

    HBrowse := win.RegisterWindow(BrowseHandler, SIZE(std.DIALOG));
    ASSERT(HBrowse # win.Invalid_H);

    WITH size DO x1 := 16; y1 := 2; x2 := 63; y2 := 22; END;
    win.SetWindowSize(HBrowse, size);
    win.SetModal(HBrowse);

    pdial := std.PDIALOG( win.GetAMPtr(HBrowse) );
    WITH pdial^ DO
      curr := 0;
      on_error := win.Invalid_H;
      Lines := lines;
      action := MakeName;
    END;

    InitListAttr;
  END;

  fmt.print(hdr, "Browse: %s", header);
  win.SetHeaderByStr(HBrowse, hdr);

  PFName := sys.ADR(path);
  Action := action;

  eve.AddToTail(HBrowse, eve.Rise, 0);
END Browse;


BEGIN
  HBrowse  := win.Invalid_H;
  PFilList := NIL;
  PDirList := NIL;
  PDrvList := NIL;
  WITH BrowseEmptyList DO
    N := 0;
    curr := 0;
    frame := 0;
    pos := 0;
    Frame := crt.Single;
    Colors := sys.ADR(BrowseListAttr);
    locator := NIL;
    actions[0] := act.EMPTY_CONTEXT;
  END;
  FList := ENTRYLIST { NIL, 0 };
  DList := ENTRYLIST { NIL, 0 };
  PatternSearch := '';
END DlgBrows.
