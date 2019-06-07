<* Storage+ *>
IMPLEMENTATION MODULE Dlg_Mem;

IMPORT fmt := FormStr;
IMPORT sys := SYSTEM;
IMPORT ioc := IOChan;
IMPORT cc  := ChanConsts;
IMPORT rf  := RndFile;
IMPORT rio := RawIO;
IMPORT tio := TextIO;

IMPORT crt := CRT;
IMPORT exp := Expr;
IMPORT key := Keys;
IMPORT lst := Lists;

IMPORT std := Dlg_Std;
IMPORT win := Dlg_Win;
IMPORT dsm := Dlg_Dasm;
IMPORT act := Dlg_Acts;
IMPORT eve := DlgEvent;
IMPORT mod := DlgMods;
IMPORT puw := DlgPopup;
IMPORT dv  := Dlg_Vars;
IMPORT dw  := DlgWatch;
IMPORT dlt := DlgTypes;
IMPORT dex := Dlg_Exec;
IMPORT brw := DlgBrows;

IMPORT mem := Exe_Mem;
IMPORT exe := ExeMain;

IMPORT mes := MsgNo;
IMPORT pro := Protocol;
IMPORT r2s := Real2Str;

IMPORT kt  := KrnTypes;
IMPORT kex := KrnExec;
IMPORT kme := Krn_Mem;

IMPORT dt  := DI_Types;

IMPORT xs  := xStr;
IMPORT opt := Options;

IMPORT Events;

FROM Events IMPORT ACCESS_TYPE;

<* IF DEST_XDS THEN *>

IMPORT dbr := DlgBreak;
IMPORT trn := Translit;

<* END *>



TYPE
  PDUMP = POINTER TO DUMP;

VAR
  DumpWindow       : win.HWND;
  DumpDialog       : win.HWND;
  ActiveDumpWindow : win.HWND;



PROCEDURE ChangeOriginTo (b_addr: kt.ADDRESS): BOOLEAN;
VAR
  p: PDUMP;
  start, s_len: CARDINAL;
  access: kt.ATTRIBS;
  buf, fmt_str: xs.String;
BEGIN
  p := win.GetAMPtr (ActiveDumpWindow);
  p^.seg_info := mem.GetSegmentInfo (b_addr, start, s_len, access);
  IF p^.seg_info THEN
    WITH p^ DO
      addr_first := b_addr;
      addr := b_addr;
      curr := 0;
      min  := start;
     <* IF DEST_K26 THEN *>
      write:= (kt.write IN access);
     <* ELSE *>
      write:= TRUE;
     <* END *>
      max  := start + s_len - 1;
    END;
  ELSE
    std.NotifyNo (mes.Incorrect_address);
    WITH p^ DO
      addr_first := b_addr;
      addr := b_addr;
      curr := 0;
      min  := b_addr;
      write:= FALSE;
      max  := b_addr+10H;
      Mode := dt.st_byte;
    END;
  END;
  pro.GetMsg(mes.Dump, fmt_str);
  fmt.print(buf, fmt_str, b_addr);
  win.SetHeaderByStr(ActiveDumpWindow, buf);
  RETURN TRUE;
END ChangeOriginTo;


PROCEDURE ChangeOrigin(hwnd: win.HWND): BOOLEAN ;
VAR
  b_addr: kt.ADDRESS;
BEGIN
  IF NOT exp.GetAddress(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, dv.DumpAddrStr, b_addr) THEN
    std.SetErrorNo(exp.error);
    RETURN FALSE
  END;
  IF ChangeOriginTo(b_addr) THEN
    eve.AddToTail(hwnd, eve.Hide, 0);
    eve.Flush;
    eve.AddToTail(ActiveDumpWindow, eve.Redraw, 0);
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END ChangeOrigin;

PROCEDURE ChangeMemory(target, dialog: win.HWND):BOOLEAN;
VAR
  addr: kt.ADDRESS;
  p   : PDUMP;
  l,r : exp.ExprRes;
BEGIN
  p := win.GetAMPtr(target);
  IF NOT opt.IgnoreWriteProtected AND NOT p^.write THEN
    std.SetErrorMsgNo(mes.Dump_WriteProtected);
    RETURN FALSE;
  END;
  addr := p^.addr + p^.curr*dt.Types[p^.Mode].len;
  l.sort := exp.Variable;
  l.type := p^.Mode;
  l.location := addr;
  CASE p^.Mode OF
  | dt.st_mixed :
    l.var_type := dt.Types[dt.st_byte].std;
  ELSE
    l.var_type := dt.Types[p^.Mode].std;
  END;
  exp.CalcExpr(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, std.NewValue, r);
  IF (exp.error # 0) OR NOT exp.dfn THEN
    std.SetErrorNo(exp.error);
    RETURN FALSE;
  END;
  IF NOT exp.Assign(l,r) THEN
    std.SetErrorNo(exp.error);
    RETURN FALSE;
  END;
  eve.AddToTail(dialog, eve.Hide, 0);
  IF std.Wnds[std.WatchWindow].hwnd # win.Invalid_H THEN
    dw.RecalcWatches;
    eve.AddToTail(std.Wnds[std.WatchWindow].hwnd, eve.Redraw, 0);
  END;
  eve.Flush;
  IF dv.MainMode # dv.source THEN
    dsm.SaveCurrentState;
    dsm.RestoreCurrentState;
  END;
  eve.AddToTail(eve.AllWindows, eve.Redraw, 1);
  RETURN TRUE;
END ChangeMemory;


PROCEDURE ChangeDump(dialog: win.HWND):BOOLEAN;
BEGIN
  RETURN ChangeMemory(ActiveDumpWindow, dialog);
END ChangeDump;


PROCEDURE ChangeStack(dialog: win.HWND):BOOLEAN;
BEGIN
  RETURN ChangeMemory(std.Wnds[std.StackWindow].hwnd, dialog);
END ChangeStack;


PROCEDURE Insert(VAR str: ARRAY OF CHAR; piece-: ARRAY OF CHAR; pos: CARDINAL);
BEGIN
  sys.MOVE(sys.ADR(piece), sys.ADR(str[pos]), LENGTH(piece));
END Insert;

TYPE
  VALUE = RECORD
            CASE :INTEGER OF
            | 0: ch: CHAR;
            | 1: b1: sys.CARD8;
            | 2: b2: sys.CARD16;
            | 3: b4: sys.CARD32;
            | 4: i1: sys.INT8;
            | 5: i2: sys.INT16;
            | 6: i4: sys.INT32;
            END;
          END;


PROCEDURE DumpHandler (hwnd: win.HWND; msg: eve.MSG); FORWARD;


PROCEDURE OpenNewDumpWindow (dump: DUMP; size: crt.SZ);
VAR
  p  : PDUMP;
  st1: xs.String;
  st2: xs.String;
BEGIN
  IF DumpWindow = win.Invalid_H THEN
    DumpWindow := win.RegisterWindow (DumpHandler, SIZE(DUMP));
  ELSE
    DumpWindow := win.FindClosed (win.Invalid_H, DumpHandler);
    IF DumpWindow = win.Invalid_H THEN
      DumpWindow := win.RegisterWindow (DumpHandler, SIZE(DUMP));
    END;
  END;
  ASSERT(DumpWindow # win.Invalid_H);
  win.UnHide (DumpWindow);

  pro.GetMsg(mes.Dump, st1);
  fmt.print(st2, st1, dump.addr_first);

  win.SetHeaderByStr (DumpWindow, st2);
  win.SetWindowSize (DumpWindow, size);
  win.SetMovable (DumpWindow);
  win.SetResizable (DumpWindow, TRUE, TRUE);
  win.SetSwitchable (DumpWindow);

  p := win.GetAMPtr (DumpWindow);
  p^ := dump;
  IF (p^.Mode > MAX(dt.SYM_TYPE_ID)) OR NOT (p^.Mode IN act.TYPES_SET{ dt.st_byte, dt.st_word, dt.st_dword, dt.st_card8, dt.st_card16, dt.st_card32, dt.st_int8, dt.st_int16, dt.st_int32, dt.st_char, dt.st_mixed }) THEN
    p^.Mode := dt.st_mixed;
  END;

  ActiveDumpWindow := DumpWindow;
  eve.AddToTail (DumpWindow, eve.Rise, 0);
END OpenNewDumpWindow;


PROCEDURE InitDumpWindow (hwnd: win.HWND): BOOLEAN;
VAR
  dump: DUMP;
  size: crt.SZ;
  b_addr: kt.ADDRESS;
  start, s_len: CARDINAL;
  access: kt.ATTRIBS;
BEGIN
  IF NOT exp.GetAddress(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, dv.DumpAddrStr, b_addr) THEN
    std.SetErrorNo(exp.error);
    RETURN FALSE
  END;
  dump.seg_info := mem.GetSegmentInfo(b_addr, start, s_len, access);
  IF dump.seg_info THEN
    WITH dump DO
      Mode := dt.SYM_TYPE_ID (opt.InitDumpType);
      addr_first := b_addr;
      addr := b_addr;
      curr := 0;
      min  := start;
     <* IF DEST_K26 THEN *>
      write:= (kt.write IN access);
     <* ELSE *>
      write:= TRUE;
     <* END *>
      max  := start + s_len - 1;
    END;
  ELSE
    std.NotifyNo (mes.Incorrect_address);
    WITH dump DO
      Mode := dt.st_byte;
      addr_first := b_addr;
      addr := b_addr;
      curr := 0;
      min  := start;
      write:= FALSE;
      max  := b_addr+10H;
    END;
  END;
  WITH size DO
    x1 := 0;  y1 := 16;
    x2 := 79; y2 := 24;
  END;
  IF hwnd # win.Invalid_H THEN
    eve.AddToTail(hwnd, eve.Hide, 0);
  END;
  OpenNewDumpWindow (dump, size);
  eve.Flush;
  RETURN TRUE;
END InitDumpWindow;



<* IF DEST_XDS THEN *>


VAR
  SaveDumpToFileHwnd : win.HWND;
  SaveDumpFileName   : xs.String;
  SaveDumpAddress    : xs.String;
  SaveDumpLength     : xs.String;
  SaveDumpWriteAsText: BOOLEAN;
  SaveDumpWriteHeader: BOOLEAN;

CONST
  FileName   = 2;
  StartAddr  = 4;
  DumpLength = 6;
  WrHeader   = 9;


PROCEDURE DoSaveDumpToFile (hwnd: win.HWND): BOOLEAN;
VAR
  file  : ioc.ChanId;
  res   : cc.OpenResults;
  mode  : rf.FlagSet;
  str   : xs.String;
  addr  : kt.ADDRESS;
  len, i: CARDINAL;
  length: CARDINAL;
  start : CARDINAL;
  access: kt.ATTRIBS;
  byte  : sys.CARD8;
--  p    : std.PDIALOG;
BEGIN
--  p := win.GetAMPtr(hwnd);
  IF NOT exp.GetAddress(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, SaveDumpAddress, addr) THEN
    std.SetErrorNo(exp.error);
    RETURN FALSE;
  END;
  IF NOT mem.GetSegmentInfo(addr, start, length, access) THEN
    std.SetErrorMsgNo (mes.Incorrect_address);
    RETURN FALSE;
  END;
  IF NOT exp.GetCardValue(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, SaveDumpLength, len) THEN
    std.SetErrorNo(exp.error);
    RETURN FALSE;
  END;
  IF SaveDumpWriteAsText THEN
    mode := rf.text+rf.write+rf.old;
  ELSE
    mode := rf.raw+rf.write+rf.old;
  END;
  rf.OpenClean (file, SaveDumpFileName, mode, res);
  IF res # cc.opened THEN
    std.SetErrorMsg ("File open error", SaveDumpFileName);
    RETURN FALSE;
  END;
  byte := 0;
  IF SaveDumpWriteAsText THEN
    IF SaveDumpWriteHeader THEN
      fmt.print (str, "Block address 0x%$8X\n", start);
      tio.WriteString (file, str);
      fmt.print (str, "Block length  %u\n", length);
      tio.WriteString (file, str);
      tio.WriteString (file, "Block access");
      IF kt.execute IN access THEN tio.WriteString (file, "  execute"); END;
      IF kt.read    IN access THEN tio.WriteString (file, ", read"); END;
      IF kt.write   IN access THEN tio.WriteString (file, ", write"); END;
      IF kt.bit_32  IN access THEN tio.WriteString (file, ", 32-bit"); END;
      fmt.print (str, "\n\nWrite address 0x%$8X\n", addr);
      tio.WriteString (file, str);
      fmt.print (str, "Write length  %u\n", len);
      tio.WriteString (file, str);
    END;
    FOR i := 1 TO len DO
      IF i MOD 16 = 1 THEN
        fmt.print (str, "\n0x%$8X: ", addr+i-1);
        tio.WriteString (file, str);
      END;
      IF mem.Get(addr+i-1, sys.ADR(byte), 1) THEN
        fmt.print (str, "%$2X ", byte);
      ELSE
        fmt.print (str, "?? ", byte);
      END;
      tio.WriteString (file, str);
    END;
  ELSE
    FOR i := 1 TO len DO
      IF NOT mem.Get(addr+i-1, sys.ADR(byte), 1) THEN
        byte := 0;
      END;
      rio.Write (file, byte);
    END;
  END;
  rf.Close (file);
  eve.AddToTail(hwnd, eve.Hide, 0);
  eve.Flush;
  RETURN TRUE;
END DoSaveDumpToFile;

CONST
  SaveDumpToFileHeader = "Save dump to file";


PROCEDURE UpdateSaveDumpFileName;
VAR
  p: std.PDIALOG;
BEGIN
  p := win.GetAMPtr(SaveDumpToFileHwnd);
  p^.curr := FileName;
  eve.AddToTail (SaveDumpToFileHwnd, eve.Redraw, 0);
END UpdateSaveDumpFileName;


PROCEDURE BrowseSaveDumpFileName;
BEGIN
  IF SaveDumpFileName = '' THEN
    fmt.print(SaveDumpFileName, '*.dmp');
  END;
  brw.Browse(SaveDumpToFileHeader, SaveDumpFileName, UpdateSaveDumpFileName);
END BrowseSaveDumpFileName;


PROCEDURE SwitchWriteAsText;
VAR
  p: std.PDIALOG;
BEGIN
  p := win.GetAMPtr(SaveDumpToFileHwnd);
  WITH p^ DO
    IF curr # WrHeader THEN
      IF SaveDumpWriteAsText THEN
        Lines^[WrHeader].state := std.d_enabled;
      ELSE
        Lines^[WrHeader].state := std.d_disabled;
      END;
    END;
  END;
  eve.AddToTail (SaveDumpToFileHwnd, eve.Redraw, 0);
END SwitchWriteAsText;


PROCEDURE SaveDumpToFile;
VAR
  Lines: std.PLINES;
  p    : std.PDIALOG;
  size : crt.SZ;
  pdump: PDUMP;
BEGIN
  IF SaveDumpToFileHwnd = win.Invalid_H THEN
    NEW (Lines, 10);
    Lines^[0]          := std.LINE{ 2, 2, std.frame, 27, 2, crt.Single, std.d_enabled };
    Lines^[1]          := std.LINE{ 5, 2, std.msg, " File name ", std.d_enabled};
    Lines^[FileName]   := std.LINE{ 4, 3, std.edit_str, sys.ADR(SaveDumpFileName), 24, std.d_enabled};
    Lines^[3]          := std.LINE{31, 3, std.button, 'Browse', BrowseSaveDumpFileName, 1, key.AltB, std.d_enabled};
    Lines^[StartAddr]  := std.LINE{ 3, 6, std.msg, "Address", std.d_enabled};
    Lines^[5]          := std.LINE{11, 6, std.edit_str, sys.ADR(SaveDumpAddress), 10, std.d_enabled};
    Lines^[DumpLength] := std.LINE{ 3, 7, std.msg, "Length", std.d_enabled};
    Lines^[7]          := std.LINE{11, 7, std.edit_str, sys.ADR(SaveDumpLength), 10, std.d_enabled};
    Lines^[8]          := std.LINE{23, 6, std.check, "Write as text", sys.ADR(SaveDumpWriteAsText), SwitchWriteAsText, 10, key.AltT, std.d_enabled};
    Lines^[WrHeader]   := std.LINE{23, 7, std.check, "Write header", sys.ADR(SaveDumpWriteHeader), SwitchWriteAsText, 7, key.AltH, std.d_enabled};

    SaveDumpToFileHwnd := win.RegisterWindow (std.DialogProc, SIZE(std.DIALOG));
    ASSERT(SaveDumpToFileHwnd # win.Invalid_H);
    win.SetModal (SaveDumpToFileHwnd);
    win.SetMovable (SaveDumpToFileHwnd);
    win.SetHeaderByStr (SaveDumpToFileHwnd, SaveDumpToFileHeader);
    WITH size DO
      x1 := 18; y1 := 7;
      x2 := 60; y2 := 19;
    END;
    win.SetWindowSize (SaveDumpToFileHwnd, size);
    p := win.GetAMPtr(SaveDumpToFileHwnd);
    p^.curr := 0;
    IF std.ErrorMsg = win.Invalid_H THEN
      std.InitErrorMsg;
    END;
    p^.on_error := std.ErrorMsg;
    p^.Lines := Lines;
    p^.action := DoSaveDumpToFile;
  END;
  IF ActiveDumpWindow = win.Invalid_H THEN
    COPY("", SaveDumpFileName);
    COPY("0x00000000", SaveDumpAddress);
    COPY("0", SaveDumpLength);
    SaveDumpWriteAsText := FALSE;
    SaveDumpWriteHeader := FALSE;
  ELSE
    pdump := win.GetAMPtr(ActiveDumpWindow);
    WITH pdump^ DO
      fmt.print (SaveDumpAddress, "0x%$8X", addr_first+curr);
      fmt.print (SaveDumpFileName, "%s.dmp", SaveDumpAddress);
      fmt.print (SaveDumpLength, "%u", max-min+1);
    END;
    SaveDumpWriteAsText := TRUE;
    SaveDumpWriteHeader := TRUE;
  END;
  eve.AddToTail(SaveDumpToFileHwnd, eve.Rise, 0);
END SaveDumpToFile;

<* END *>

VAR
  width, height, cells: CARDINAL;


PROCEDURE DumpHandler (hwnd: win.HWND; msg: eve.MSG);
VAR
  DumpContextList: act.CONTEXT_LIST;
  p   : PDUMP;
  addr: kt.ADDRESS;
  curr: CARDINAL;
  ok  : BOOLEAN;
  name: xs.String;
  main: BOOLEAN;
  item: CARDINAL;


CONST
  Break    = 1;
  NewDump  = 2;
  DumpAt   = 3;
  GoHome   = 5;
  ChangeTo = 6;
  CodeAt   = 8;

 <* IF DEST_XDS THEN *>

  Save   = 12;
  ViewAs = Save+1;

 <* END *>


  PROCEDURE SetDumpContext;
  BEGIN
    addr := 0;
    ok := mem.Get(p^.addr+p^.curr*dt.Types[p^.Mode].len, sys.ADR(addr), 4);
    DumpContextList[0]        := act.CONTEXT{ act.push_key, 'Modify memory', key.Enter };
   <* IF DEST_XDS THEN *>
    DumpContextList[Break]    := act.CONTEXT{ act.context_item, 'Break on write' };
   <* ELSIF DEST_K26 THEN *>
    DumpContextList[1]        := act.CONTEXT{ act.do_action, act.ConditionBreak };
   <* END *>
    DumpContextList[NewDump]  := act.CONTEXT{ act.context_item, 'New Dump' };
    name := 'New Dump at';
    IF ok THEN fmt.append(name, ' %$8X', addr); END;
    DumpContextList[DumpAt]   := act.CONTEXT{ act.context_item,  act.CONTEXT_NAME(name) };
    DumpContextList[4]        := act.CONTEXT{ act.separate };
    DumpContextList[GoHome]   := act.CONTEXT{ act.context_item, 'Go to origin' };
    name := 'Change origin to';
    IF ok THEN fmt.append(name, ' %$8X', addr); END;
    DumpContextList[ChangeTo] := act.CONTEXT{ act.context_item, act.CONTEXT_NAME(name) };
    DumpContextList[7]        := act.CONTEXT{ act.do_action, act.ChangeOrigin };
    name := 'Code at';
    IF ok THEN fmt.append(name, ' %$8X', addr); END;
    DumpContextList[CodeAt]   := act.CONTEXT{ act.context_item, act.CONTEXT_NAME(name) };
    DumpContextList[9]        := act.CONTEXT{ act.separate };
    DumpContextList[10]       := act.CONTEXT{ act.types, act.TYPES_SET{ dt.st_byte, dt.st_word, dt.st_dword, dt.st_card8, dt.st_card16, dt.st_card32, dt.st_int8, dt.st_int16, dt.st_int32, dt.st_char, dt.st_mixed } };
   <* IF DEST_XDS THEN *>
    IF ok THEN
      DumpContextList[11]     := act.CONTEXT{ act.separate };
      DumpContextList[Save]   := act.CONTEXT{ act.context_item, SaveDumpToFileHeader };
      IF std.MakeSubmenuViewAs (DumpContextList[ViewAs]) THEN
        DumpContextList[ViewAs+1] := act.EMPTY_CONTEXT;
      END;
    ELSE
      DumpContextList[11]     := act.EMPTY_CONTEXT;
    END;
   <* ELSE *>
    DumpContextList[11]       := act.EMPTY_CONTEXT;
   <* END *>
  END SetDumpContext;

VAR
  size: crt.SZ;
  attr: crt.ATTR;
  x, y: CARDINAL;
  buf : ARRAY [0..511] OF CHAR;
  value: VALUE;
  len, i, j, pos: CARDINAL;
  part, curr_elem, store: ARRAY [0..31]  OF CHAR;
  start, s_len: CARDINAL;
  access: kt.ATTRIBS;

BEGIN
  p := win.GetAMPtr(hwnd);
  WITH p^ DO
    IF NOT seg_info THEN
      seg_info := mem.GetSegmentInfo (addr_first, start, s_len, access);
      IF seg_info THEN
        addr := addr_first;
        curr := 0;
        min := start;
       <* IF DEST_K26 THEN *>
        write:= (kt.write IN access);
       <* ELSE *>
        write:= TRUE;
       <* END *>
        max  := start + s_len - 1;
      END;
    END;
  END;
  size := win.GetWindowSize(hwnd);
  WITH size DO
    height := y2 - y1 - 1;
    width  := x2 - x1 - 1;
  END;
  IF width > 12 THEN
    cells := (width - 12) DIV (dt.Types[p^.Mode].s_len+1);
  ELSE
    cells := 0;
  END;
  CASE msg.ID OF
  | eve.Rise:
    ActiveDumpWindow := hwnd;
    std.DefaultProc(hwnd, msg);

  | eve.Paint, eve.Redraw :
    crt.FillWindow(hwnd, ' ', crt.Dump[crt.Dump_Background]);
    addr := p^.addr;
    FOR i := 1 TO height DO
      crt.SetPos(1, i);
      sys.FILL(sys.ADR(buf), ' ', width);
      buf[width] := 0C;
      IF (addr >= p^.min) AND (addr + dt.Types[p^.Mode].len - 1 <= p^.max) THEN
        fmt.print(part, "%$8X:", addr);
        Insert(buf, part, 1);
        crt.WrStr(hwnd, buf, crt.Dump[crt.Dump_Address]);
      END;
      IF width > 12 THEN
        pos := 12;
       <* PUSH *>
       <* WOFF312+ *>
        LOOP
       <* POP *>
          FOR j:=1 TO cells DO
            IF (addr < p^.min) OR (addr + dt.Types[p^.Mode].len - 1 > p^.max) THEN
              EXIT
            ELSE
              ok := mem.Get(addr, sys.ADR(value), dt.Types[p^.Mode].len);
              IF ok THEN
                CASE p^.Mode OF
                | dt.st_char :
                 <* IF DEST_XDS THEN *>
                  trn.TransliterateBuffer (opt.TranslirateTextFromTo, sys.ADR(value.b1), SIZE(value.b1));
                 <* END *>
                  fmt.print(part, dt.Types[dt.st_byte].fmt, value.b1);
                  IF value.ch # 0C THEN
                    buf[pos] := value.ch;
                  ELSE
                    buf[pos] := '.';
                  END;
                | dt.st_mixed :
                  fmt.print(part, dt.Types[dt.st_byte].fmt, value.b1);
                  Insert(buf, part, pos);
                  IF value.ch # 0C THEN
                    buf[11+cells*3+j] := value.ch;
                  ELSE
                    buf[11+cells*3+j] := '.';
                  END;
                  DEC(pos, 1);
                ELSE
                  CASE p^.Mode OF
                  | dt.st_int8, dt.st_int16, dt.st_int32:
                    CASE dt.Types[p^.Mode].len OF
                    | 1: fmt.print(part, dt.Types[p^.Mode].fmt, value.i1);
                    | 2: fmt.print(part, dt.Types[p^.Mode].fmt, value.i2);
                    | 4: fmt.print(part, dt.Types[p^.Mode].fmt, value.i4);
                    END;
                  ELSE
                    CASE dt.Types[p^.Mode].len OF
                    | 1: fmt.print(part, dt.Types[p^.Mode].fmt, value.b1);
                    | 2: fmt.print(part, dt.Types[p^.Mode].fmt, value.b2);
                    | 4: fmt.print(part, dt.Types[p^.Mode].fmt, value.b4);
                    END;
                  END;
                  Insert (buf, part, pos + dt.Types[p^.Mode].s_len - LENGTH(part));
                END;
              ELSE
                WITH p^ DO
                  IF seg_info THEN -- бум! информация о сегменте есть, а память прочитать нельзя
                    seg_info := FALSE;
                    addr := addr_first;
                    curr := 0;
                    min  := addr_first;
                    write:= FALSE;
                    max  := addr_first+10H;
                    Mode := dt.st_byte;
                    eve.AddToTail (hwnd, msg.ID, msg.par); -- и теперь еще раз тоже самое
                    RETURN;
                  END;
                END;
                sys.FILL(sys.ADR(buf[pos]), '?', dt.Types[p^.Mode].s_len);
                part := '';
              END;
              INC(pos, dt.Types[p^.Mode].s_len+1);
            END;
            INC(addr, dt.Types[p^.Mode].len);
            IF ((i-1)*cells+(j-1) = p^.curr) THEN
              curr_elem := part;
            END;
          END;
          EXIT;
        END;
      END;
      crt.WrNStr(hwnd, width, buf, crt.Dump[crt.Dump_Mem]);
      crt.LitePart(hwnd, i, 1, 12,   crt.Dump[crt.Dump_Address])
    END;
    WITH p^ DO
      attr := crt.Dump[crt.Dump_Frame];
      IF hwnd = win.ActiveWindow THEN
        crt.DrawFrame(hwnd, size, crt.Double, attr);
      ELSE
        crt.DrawFrame(hwnd, size, crt.Single, attr);
      END;
      crt.DrawControls (hwnd, size, attr, dlt.WND_CTRL_SET{dlt.WinCtrl_Close});
      fmt.print(buf, '%$8.8X (%s)', addr + curr*dt.Types[p^.Mode].len, curr_elem);
    END;
    IF hwnd = win.ActiveWindow THEN
      y := (p^.curr DIV cells);
      CASE p^.Mode OF
      | dt.st_mixed :
        x := 13 + (p^.curr MOD cells) * (dt.Types[p^.Mode].s_len);
        pos := x + dt.Types[p^.Mode].s_len - 1;
      | dt.st_char :
        x := 13 + (p^.curr MOD cells) * (dt.Types[p^.Mode].s_len+1);
        pos := x + dt.Types[p^.Mode].s_len + 1;
      ELSE
        x := 13 + (p^.curr MOD cells) * (dt.Types[p^.Mode].s_len + 1);
        pos := x + dt.Types[p^.Mode].s_len;
      END;
      crt.LitePart(hwnd, y + 1, x, pos, crt.Dump[crt.Dump_CurMem])
    END;
    win.GetHeader (hwnd, store);
    win.SetHeaderByStr (hwnd, buf);
    IF hwnd = win.ActiveWindow THEN
      attr := crt.Dump[crt.Dump_ActiveHeader];
    ELSE
      attr := crt.Dump[crt.Dump_Header];
    END;
    crt.DrawHeader (hwnd, size, attr);
    win.SetHeaderByStr (hwnd, store);
    IF msg.ID = eve.Redraw THEN
      crt.UpdateRect(size);
     END;

  | eve.Mouse_Pressed, eve.Mouse_Dbl, eve.Mouse_Moved:
    ASSERT(win.GetRelMouse(hwnd, msg, x, y));
    IF msg.ID = eve.Mouse_Pressed THEN
      CASE crt.GetControl (size, x, y, dlt.WND_CTRL_SET{dlt.WinCtrl_Close}) OF
      | dlt.WinCtrl_Close:
        eve.AddToTail(hwnd, eve.Hide, 0);
        RETURN;
      ELSE
        WITH size DO
          IF std.CheckFrame(size, x1+x, y1+y) THEN
            std.Move_Resize_mou (hwnd, 14+dt.Types[p^.Mode].s_len+1, 0, x1+x, y1+y);
            RETURN;
          END;
        END;
      END;
    END;
    height := size.y2 - size.y1;
    IF (y > 0) AND (y < height) THEN
      WITH p^  DO
        len := dt.Types[p^.Mode].s_len;
        IF Mode # dt.st_mixed THEN
          INC (len);
        END;
      END;
      IF (x >= 13) THEN
        DEC(x,13);
        IF (x <= cells*len - 1) THEN
          curr := (y-1)*cells + (x DIV len);
          addr := p^.addr + curr * dt.Types[p^.Mode].len;
          IF (addr >= p^.min) AND (addr + dt.Types[p^.Mode].len - 1 <= p^.max) THEN
            p^.curr := curr;
            eve.AddToTail(hwnd, eve.Redraw, 0);
            IF msg.ID = eve.Mouse_Dbl THEN
              eve.AddToTail (hwnd, eve.KbHit, key.Enter);
            END;
          ELSE
            crt.Beep;
          END;
        END;
      ELSIF (x > 1) AND (x < 10) AND (msg.ID # eve.Mouse_Moved) THEN
        fmt.print (dv.DumpAddrStr, exp.Fmt_ADDRval, p^.addr+(y-1)*cells*dt.Types[p^.Mode].len);
        eve.AddToTail (hwnd, eve.DoAction, ORD(act.ChangeOrigin));
      END;
    END;

  | eve.R_Mouse:
    ASSERT(win.GetMouse(msg, i, y));
    IF NOT std.CheckFrame(size, i, y) THEN
      x := i-size.x1;
      IF x < 12 THEN INC(x, 12-x); END;
      INC(x, size.x1);
      msg.par := x*10000H + y;
      eve.AddToTail(hwnd, eve.Mouse_Pressed, msg.par);
      eve.Flush;
      SetDumpContext;
      puw.PopupWindow(i, y, hwnd, DumpContextList);
    END;

  | eve.QueryKbHit:
    CASE std.GetKey (msg.par) OF
    | key.Enter:
      act.ConfirmQueryByCond (p^.seg_info);
    ELSE
      std.DefaultProc(hwnd, msg);
    END;

  | eve.QueryItem:
    std.GetItem (msg.par, main, item);
    IF main THEN
      CASE item OF
      | NewDump:
        act.ConfirmQuery();
      | GoHome:
        act.ConfirmQuery();
      | Break:
        act.ConfirmQuery();
      | ChangeTo:
        act.ConfirmQueryByCond (p^.seg_info);
      | DumpAt:
        act.ConfirmQueryByCond (p^.seg_info);
      | CodeAt:
        act.ConfirmQueryByCond (p^.seg_info);
     <* IF DEST_XDS THEN *>
      | Save:
        act.ConfirmQueryByCond (p^.seg_info) ;
      | ViewAs:
        act.ConfirmQueryByCond (p^.seg_info AND (lst.EquNamesNo() > 0));
     <* END *>
      ELSE
        std.DefaultProc (hwnd, msg);
      END;
   <* IF DEST_XDS THEN *>
    ELSE
      act.ConfirmQueryByCond (p^.seg_info AND (item < lst.EquNamesNo()));
   <* END *>
    END;

  | eve.ContextItem:
    std.GetItem (msg.par, main, item);
    IF main THEN
      CASE item OF
      | Break:
        <* IF DEST_XDS THEN *>
        IF NOT dbr.CreateNewAccessBreak(Events.Write, p^.addr+p^.curr*dt.Types[p^.Mode].len, dt.Types[p^.Mode].len) THEN
          eve.AddToTail(std.ErrorMsg, eve.Rise, 0);
        END;
        <* ELSIF DEST_K26 THEN *>
        eve.AddToTail(hwnd, eve.DoAction, ORD(act.ConditionBreak));
        <* END *>
      | NewDump:
        eve.AddToTail(hwnd, eve.DoAction, ORD(act.Dump));
      | GoHome:
        eve.AddToTail(hwnd, eve.DoAction, ORD(act.GotoExec));
      | CodeAt:
        addr := 0;
        ok := mem.Get(p^.addr+p^.curr*dt.Types[p^.Mode].len, sys.ADR(addr), 4);
        IF ok THEN
          IF mod.SetNewPosByAddr(addr) THEN
            eve.AddToTail(std.Wnds[std.MainWindow].hwnd, eve.Rise, 0);
          ELSE
            std.ErrorNo(mes.Incorrect_address);
          END;
        END;
      | ChangeTo:
        addr := 0;
        ok := mem.Get(p^.addr+p^.curr*dt.Types[p^.Mode].len, sys.ADR(addr), 4);
        IF ok THEN
          IF ChangeOriginTo(addr) THEN
            eve.AddToTail(hwnd, eve.Redraw, 0);
          ELSE
            eve.AddToTail(std.ErrorMsg, eve.Rise, 0);
          END;
        END;
      | DumpAt:
        addr := 0;
        ok := mem.Get(p^.addr+p^.curr*dt.Types[p^.Mode].len, sys.ADR(addr), 4);
        IF ok THEN
          fmt.print(dv.DumpAddrStr, 'A:%$8X', addr);
          IF InitDumpWindow(win.Invalid_H) THEN
            eve.AddToTail(hwnd, eve.Redraw, 0);
          ELSE
            eve.AddToTail(std.ErrorMsg, eve.Rise, 0);
          END;
        END;
     <* IF DEST_XDS THEN *>
      | Save:
        SaveDumpToFile;
      | ViewAs:
        ASSERT(FALSE);
     <* END *>
      ELSE
        std.DefaultProc(hwnd, msg);
      END;
   <* IF DEST_XDS THEN *>
    ELSE
      lst.GetName (item, name);
      lst.GetEquName (name, buf);
      fmt.print (dv.VarName, '%s<A:%$8X>', buf, p^.addr+p^.curr*dt.Types[p^.Mode].len);
      act.ExecuteAction (act.Examine, act.mode_silent);
   <* END *>
    END;

  | eve.QueryAction:
   <* PUSH *>
   <* WOFF902+ *>
    CASE std.GetAction (msg.par) OF
   <* POP *>
    | act.ContextMenu:
      act.ConfirmQuery;
    | act.ChangeTypes:
      act.ConfirmQueryByCond (p^.seg_info);
    | act.ChangeOrigin:
      act.ConfirmQuery;
   <* IF DEST_XDS THEN *>
    | act.Access:
      act.ConfirmQuery;
   <* END *>
    ELSE
      std.DefaultProc(hwnd, msg);
    END;

  | eve.DoAction:
   <* PUSH *>
   <* WOFF902+ *>
    CASE VAL(act.ACTION, msg.par MOD 100H) OF
   <* POP *>
    | act.ContextMenu:
      SetDumpContext;
      WITH size DO
        puw.PopupWindow(x1+(x2-x1) DIV 2, y1+(y2-y1) DIV 2, hwnd, DumpContextList);
      END;

    | act.GotoExec:
      p^.addr := p^.addr_first;
      p^.curr := 0;
      eve.AddToTail(hwnd, eve.Redraw, 0);

    | act.ChangeTypes:
      WITH p^ DO
        addr := addr + curr*dt.Types[Mode].len;
        curr := 0;
        Mode := act.ChangeTypesID;
      END;
      eve.AddToTail(hwnd, eve.Redraw, 0);

    | act.ChangeOrigin:
      std.OpenUniversalDialog(mes.InputAddress, ChangeOrigin, dv.DumpAddrStr);

   <* IF DEST_XDS THEN *>
    | act.Access:
      fmt.print(buf,'A:%$8X', p^.addr + p^.curr * dt.Types[p^.Mode].len);
      dbr.SetAccessAttr(buf, dt.Types[p^.Mode].len);
      act.ExecuteAction (act.Access, act.mode_silent);
      RETURN;
   <* END *>

    ELSE
      std.DefaultProc(hwnd, msg);
    END;

  | eve.KbHit:
    CASE msg.par OF
    | key.CtrlW:
      std.Move_Resize_kbd(hwnd, 14+dt.Types[p^.Mode].s_len+1, 0);
    | key.Left, key.Right:
      IF width > 12 THEN
        WITH p^ DO
          IF msg.par = key.Right THEN
            IF addr+(p^.curr+1)*dt.Types[p^.Mode].len <= max THEN
              IF max - (addr+(p^.curr+1)*dt.Types[p^.Mode].len) >= dt.Types[p^.Mode].len-1 THEN
                IF curr = height*cells-1 THEN
                  INC(addr, dt.Types[p^.Mode].len)
                ELSE
                  INC(curr);
                END;
              END;
            END;
          ELSE
            IF addr+p^.curr*dt.Types[p^.Mode].len >= min+dt.Types[p^.Mode].len THEN
              IF curr = 0 THEN
                DEC (addr, dt.Types[p^.Mode].len);
              ELSE
                DEC (curr);
              END;
            END;
          END;
        END;
        eve.AddToTail(hwnd, eve.Redraw, 0);
      END;
    | key.Enter:
      p := win.GetAMPtr(hwnd);
      fmt.print(buf,'Memory at %$8X', p^.addr+p^.curr*dt.Types[p^.Mode].len);
      std.IniInputDialog(ChangeDump, FALSE, buf);
    | key.PgUp, key.PgDn:
      size := win.GetWindowSize(hwnd);
      IF width > 12 THEN
        IF msg.par = key.PgUp THEN
          IF p^.addr - dt.Types[p^.Mode].len >= p^.min THEN
            DEC(p^.addr, cells*height*dt.Types[p^.Mode].len);
            IF p^.addr < p^.min THEN
              IF p^.curr > (p^.min - p^.addr) DIV dt.Types[p^.Mode].len THEN
                DEC(p^.curr, (p^.min - p^.addr) DIV dt.Types[p^.Mode].len)
              END;
              p^.addr := p^.min;
            END;
          ELSE
            p^.curr := 0;
          END;
        ELSE
          IF p^.addr + cells*height*dt.Types[p^.Mode].len <= p^.max THEN
            INC(p^.addr, cells*height*dt.Types[p^.Mode].len);
          ELSE
            p^.curr := std.Min(cells*height - 1, (p^.max - p^.addr) DIV dt.Types[p^.Mode].len);
          END;
        END;
        eve.AddToTail(hwnd, eve.Redraw, 0);
      END;
    | key.CtrlHome:
      p := win.GetAMPtr(hwnd);
      p^.curr := 0;
      eve.AddToTail(hwnd, eve.Redraw, 0);
    | key.CtrlEnd:
      IF width > 12 THEN
        p^.curr := std.Min(cells*height - 1, (p^.max - p^.addr) DIV dt.Types[p^.Mode].len);
        IF p^.addr + (p^.curr)*dt.Types[p^.Mode].len <= p^.max THEN
          IF (p^.max - (p^.addr + (p^.curr)*dt.Types[p^.Mode].len) < dt.Types[p^.Mode].len-1) THEN
            IF p^.curr > 0 THEN
              DEC(p^.curr)
            ELSE
              IF p^.addr - dt.Types[p^.Mode].len >= p^.addr_first THEN
                DEC(p^.addr, dt.Types[p^.Mode].len);
              END;
            END;
          END;
        END;
        eve.AddToTail(hwnd, eve.Redraw, 0);
      END;
    | key.Home:
      IF cells > 0 THEN
        WITH p^ DO
          curr := curr - (curr MOD cells);
        END;
        eve.AddToTail(hwnd, eve.Redraw, 0);
      END;
    | key.End:
      IF width > 12 THEN
        WITH p^ DO
          curr := curr + (cells - curr MOD cells) - 1;
          IF addr + (curr+1)*dt.Types[Mode].len > max THEN
            curr := ((max - addr) DIV dt.Types[Mode].len);
            IF max - (addr + (curr)*dt.Types[Mode].len) < dt.Types[Mode].len-1 THEN
              DEC(curr);
            END;
          END;
        END;
        eve.AddToTail(hwnd, eve.Redraw, 0);
      END;
    | key.Up, key.Down:
      IF width > 12 THEN
        WITH p^ DO
          IF msg.par = key.Down THEN
            IF addr + (curr+cells)*dt.Types[Mode].len <= max THEN
              IF max - (addr + (curr+cells)*dt.Types[Mode].len) >= dt.Types[p^.Mode].len-1 THEN
                IF (curr DIV cells)+1 = height THEN
                  INC(addr, cells*(dt.Types[Mode].len))
                ELSE
                  INC(curr, cells)
                END;
              END;
            END;
          ELSE
            IF curr < cells THEN
              IF addr >= min + dt.Types[Mode].len THEN
                DEC (addr, cells*(dt.Types[Mode].len));
                IF addr < min THEN
                  IF curr > (min - addr) DIV dt.Types[Mode].len THEN
                    DEC (curr, (min - addr) DIV dt.Types[Mode].len)
                  END;
                  addr := min;
                END;
              ELSE
                p^.curr := 0;
              END;
            ELSE
              DEC (curr, cells);
            END;
          END;
        END;
        eve.AddToTail(hwnd, eve.Redraw, 0);
      END;
    ELSE
      p := win.GetAMPtr(hwnd);
      IF (p^.Mode = dt.st_char) AND (msg.par < 256) AND (CHR(msg.par) IN key.LegalChar) THEN
        IF NOT p^.write THEN
          crt.Beep;
          RETURN;
        END;
        addr := p^.addr + p^.curr*dt.Types[p^.Mode].len;
        IF NOT mem.Put(addr, sys.ADR(msg.par), 1) THEN
          crt.Beep;
          RETURN;
        END;
        eve.AddToTail(hwnd, eve.KbHit, key.Right);
        IF std.Wnds[std.WatchWindow].hwnd # win.Invalid_H THEN
          dw.RecalcWatches;
          eve.AddToTail(std.Wnds[std.WatchWindow].hwnd, eve.Redraw, 0);
        END;
        eve.AddToTail(eve.AllWindows, eve.Redraw, 1);

      ELSE
        std.DefaultProc(hwnd, msg)
      END;
    END;
  ELSE
    std.DefaultProc(hwnd, msg)
  END;
END DumpHandler;


PROCEDURE StackHandler(hwnd: win.HWND; msg: eve.MSG);

CONST
  Break    = 1;
  DumpAt   = 2;
  CodeAt   = 3;
  GoHome   = 5;

VAR
  StackContext: act.CONTEXT_LIST;
  p   : PDUMP;
  addr: kt.ADDRESS;

  PROCEDURE SetStackContextList;
  VAR
    name: act.CONTEXT_NAME;
    ok  : BOOLEAN;

  BEGIN
    addr := 0;
    ok := mem.Get(p^.addr+p^.curr*dt.Types[p^.Mode].len, sys.ADR(addr), 4);

    StackContext[0] := act.CONTEXT{ act.push_key, 'Modify memory', key.Enter };
<* IF DEST_XDS THEN *>
    StackContext[Break] := act.CONTEXT{ act.context_item, 'Break on write' };
<* ELSIF DEST_K26 THEN *>
    StackContext[1] := act.CONTEXT{ act.do_action, act.ConditionBreak };
<* END *>
    name := 'New Dump at';
    IF ok THEN fmt.append(name, ' %$8X', addr); END;
    StackContext[DumpAt]   := act.CONTEXT{ act.context_item,  name };
    name := 'Code at';
    IF ok THEN fmt.append(name, ' %$8X', addr); END;
    StackContext[CodeAt]   := act.CONTEXT{ act.context_item, name };
    StackContext[4] := act.CONTEXT{ act.separate };
    StackContext[GoHome] := act.CONTEXT{ act.context_item, 'Go to Stack top' };
    StackContext[6] := act.CONTEXT{ act.separate };
    StackContext[7] := act.CONTEXT{ act.types, act.TYPES_SET{ dt.st_word, dt.st_dword, dt.st_card16, dt.st_card32, dt.st_int16, dt.st_int32 } };
    StackContext[8] := act.EMPTY_CONTEXT;
  END SetStackContextList;

VAR
  size: crt.SZ;
  ok  : BOOLEAN;
  attr: crt.ATTR;
  x, y: CARDINAL;
  temp: INTEGER;
  ebp : CARDINAL;
  esp : CARDINAL;
  buf : ARRAY [0..511] OF CHAR;
  value: VALUE;
  len, i, j, pos: CARDINAL;
  part, curr_elem, store: ARRAY [0..31]  OF CHAR;
  curr: CARDINAL;
  main: BOOLEAN;
  item: CARDINAL;

BEGIN
  p    := win.GetAMPtr(hwnd);
  size := win.GetWindowSize(hwnd);
  WITH size DO
    height := y2 - y1 - 1;
    width  := x2 - x1 - 1;
  END;
  IF width > 12 THEN
    cells := 1;
  ELSE
    cells := 0;
  END;
  CASE msg.ID OF
  | eve.Paint, eve.Redraw :
    crt.FillWindow(hwnd, ' ', crt.Stack[crt.Dump_Background]);
    ebp  := mem.GetFrame();
    esp  := mem.GetSP();
    addr := p^.addr;
    FOR i := 1 TO height DO
      crt.SetPos(1, i);
      sys.FILL(sys.ADR(buf), ' ', width);
      buf[width] := 0C;
      IF (addr >= p^.min) AND (addr + dt.Types[p^.Mode].len - 1 <= p^.max) THEN
        IF addr = esp THEN
          IF addr = ebp THEN
            <* IF TARGET_x86 THEN *>
              COPY('ESP, EBP:', part);
            <* ELSIF TARGET_VAX THEN *>
              COPY('SP, FP  :', part);
            <* END *>
          ELSE
            <* IF TARGET_x86 THEN *>
              COPY('ESP     :', part);
            <* ELSIF TARGET_VAX THEN *>
              COPY('SP      :', part);
            <* END *>
          END;
        ELSIF addr = ebp THEN
          <* IF TARGET_x86 THEN *>
            COPY('EBP     :', part);
          <* ELSIF TARGET_VAX THEN *>
            COPY('FP      :', part);
          <* END *>
        ELSE
          fmt.print(part,"%$8X:", addr);
        END;
        Insert(buf, part, 1);
        crt.WrStr(hwnd, buf, crt.Stack[crt.Dump_Address]);
      END;
      IF width > 12 THEN
        pos := 12;
        FOR j:=1 TO cells DO
          IF (addr < p^.min) OR (addr + dt.Types[p^.Mode].len - 1 > p^.max) THEN
            buf := '';
          ELSE
            ok := mem.Get(addr, sys.ADR(value), dt.Types[p^.Mode].len);
            IF ok THEN
              CASE p^.Mode OF
              | dt.st_char :
                IF value.ch # 0C THEN
                  buf[pos] := value.ch;
                ELSE
                  buf[pos] := '.';
                END;
                fmt.print(part, dt.Types[dt.st_byte].fmt, value.b1)
              | dt.st_mixed :
                fmt.print(part, dt.Types[dt.st_byte].fmt, value.b1);
                Insert(buf, part, pos);
                IF value.ch # 0C THEN
                  buf[11+cells*3+j] := value.ch;
                ELSE
                  buf[11+cells*3+j] := '.';
                END;
                DEC(pos, 1);
              ELSE
                CASE p^.Mode OF
                | dt.st_int8, dt.st_int16, dt.st_int32:
                  CASE dt.Types[p^.Mode].len OF
                  | 1: fmt.print(part, dt.Types[p^.Mode].fmt, value.i1);
                  | 2: fmt.print(part, dt.Types[p^.Mode].fmt, value.i2);
                  | 4: fmt.print(part, dt.Types[p^.Mode].fmt, value.i4);
                  END;
                ELSE
                  CASE dt.Types[p^.Mode].len OF
                  | 1: fmt.print(part, dt.Types[p^.Mode].fmt, value.b1);
                  | 2: fmt.print(part, dt.Types[p^.Mode].fmt, value.b2);
                  | 4: fmt.print(part, dt.Types[p^.Mode].fmt, value.b4);
                  END;
                END;
                Insert(buf, part, pos + dt.Types[p^.Mode].s_len - LENGTH(part));
              END;
            ELSE
              sys.FILL(sys.ADR(buf[pos]), '?', dt.Types[p^.Mode].s_len);
              part := '';
            END;
            INC(pos, dt.Types[p^.Mode].s_len+1);
          END;
          INC(addr, dt.Types[p^.Mode].len);
          IF ((i-1)*cells+(j-1) = p^.curr) THEN
            curr_elem := part;
          END;
        END;
      END;
      crt.WrNStr(hwnd, width, buf, crt.Stack[crt.Dump_Mem]);
      crt.LitePart(hwnd, i, 1, 12,   crt.Stack[crt.Dump_Address])
    END;
    WITH p^ DO
      attr := crt.Stack[crt.Dump_Frame];
      IF hwnd = win.ActiveWindow THEN
        crt.DrawFrame(hwnd, size, crt.Double, attr);
      ELSE
        crt.DrawFrame(hwnd, size, crt.Single, crt.Reg[crt.Reg_Frame]);
      END;
      crt.DrawControls(hwnd, size, attr, dlt.WND_CTRL_SET{dlt.WinCtrl_Close});
      fmt.print(buf, ' %$8.8X (%s) ', addr + curr*dt.Types[p^.Mode].len, curr_elem);

      temp := INTEGER(addr) - INTEGER(addr_first) + INTEGER(curr*dt.Types[p^.Mode].len);
      ASSERT(temp >= 0);
      pro.GetMsg(mes.HeaderStack, part);
      fmt.print(buf, part, temp);
    END;
    IF hwnd = win.ActiveWindow THEN
      y := (p^.curr DIV cells);
      CASE p^.Mode OF
      | dt.st_mixed :
        x := 13 + (p^.curr MOD cells) * (dt.Types[p^.Mode].s_len);
        pos := x + dt.Types[p^.Mode].s_len - 1;
      | dt.st_char :
        x := 13 + (p^.curr MOD cells) * (dt.Types[p^.Mode].s_len+1);
        pos := x + dt.Types[p^.Mode].s_len + 1;
      ELSE
        x := 13 + (p^.curr MOD cells) * (dt.Types[p^.Mode].s_len + 1);
        pos := x + dt.Types[p^.Mode].s_len;
      END;
      crt.LitePart(hwnd, y + 1, x, pos, crt.Stack[crt.Dump_CurMem])
-- для горизонтальной полосы-курсора на всю ширину окна
--     crt.LitePart (hwnd, y + 1, 1, size.x2-size.x1, crt.Stack[crt.Dump_CurMem])
   END;
    win.GetHeader (hwnd, store);
    win.SetHeaderByStr (hwnd, buf);
    IF hwnd = win.ActiveWindow THEN
      attr := crt.Stack[crt.Dump_ActiveHeader];
    ELSE
      attr := crt.Stack[crt.Dump_Header];
    END;
    crt.DrawHeader (hwnd, size, attr);
    win.SetHeaderByStr (hwnd, store);
    IF msg.ID = eve.Redraw THEN crt.UpdateRect(size) END;

  | eve.Mouse_Pressed, eve.Mouse_Dbl, eve.Mouse_Moved:
    ASSERT(win.GetRelMouse(hwnd, msg, x, y));
    IF msg.ID = eve.Mouse_Pressed THEN
      CASE crt.GetControl (size, x, y, dlt.WND_CTRL_SET{dlt.WinCtrl_Close}) OF
      | dlt.WinCtrl_Close:
        eve.AddToTail(hwnd, eve.Hide, 0);
        RETURN;
      ELSE
        WITH size DO
          IF std.CheckFrame(size, x1+x, y1+y) THEN
            std.Move_Resize_mou(hwnd, 14+dt.Types[p^.Mode].s_len+1, 0, x1+x, y1+y);
            RETURN;
          END;
        END;
      END;
    END;
    height := size.y2 - size.y1;
    IF (y>0) AND (y < height) THEN
      WITH p^  DO
        len := dt.Types[p^.Mode].s_len;
        IF Mode # dt.st_mixed THEN
          INC(len);
        END;
      END;
      IF (x >= 13) THEN
        DEC(x, 13);
        IF (x <= cells*len - 1) THEN
          curr := (y-1)*cells + (x DIV len);
          addr := p^.addr + curr * dt.Types[p^.Mode].len;
          IF (addr >= p^.min) AND (addr + dt.Types[p^.Mode].len - 1 <= p^.max) THEN
            p^.curr := curr;
            eve.AddToTail(hwnd, eve.Redraw, 0);
            IF msg.ID = eve.Mouse_Dbl THEN
              eve.AddToTail(hwnd, eve.KbHit, key.Enter);
            END;
          END;
        END;
(*
      ELSIF (x > 1) & (x < 10) & (msg.ID # eve.Mouse_Moved) THEN
        fmt.print(dv.DumpAddrStr, exp.Fmt_board_addr, p^.addr+(y-1)*cells*dt.Types[p^.Mode].len);
        eve.AddToTail(hwnd, eve.DoAction, ORD(act.ChangeOrigin));
*)
      END;
    END;

  | eve.R_Mouse:
    ASSERT(win.GetMouse(msg, i, y));
    IF NOT std.CheckFrame(size, i, y) THEN
      x := i-size.x1;
      IF x < 12 THEN INC(x, 12-x); END;
      INC(x, size.x1);
      msg.par := x*10000H + y;
      eve.AddToTail(hwnd, eve.Mouse_Pressed, msg.par);
      eve.Flush;
      SetStackContextList;
      puw.PopupWindow(i, y, hwnd, StackContext);
    END;

  | eve.QueryKbHit:
    CASE msg.par OF
    | key.Enter:
      act.ConfirmQuery;
    ELSE
      std.DefaultProc(hwnd, msg);
    END;

  | eve.QueryItem:
    std.GetItem (msg.par, main, item);
    ASSERT(main);
    CASE item OF
    | DumpAt, GoHome, CodeAt, Break:
      act.ConfirmQuery;
    ELSE
      std.DefaultProc(hwnd, msg);
    END;

  | eve.ContextItem:
    std.GetItem (msg.par, main, item);
    ASSERT(main);
    CASE item OF
    | Break:
      <* IF DEST_XDS THEN *>
      IF NOT dbr.CreateNewAccessBreak(Events.Write, p^.addr+p^.curr*dt.Types[p^.Mode].len, dt.Types[p^.Mode].len) THEN
        eve.AddToTail(std.ErrorMsg, eve.Rise, 0);
      END;
      <* ELSIF DEST_K26 THEN *>
      eve.AddToTail(hwnd, eve.DoAction, ORD(act.ConditionBreak));
      <* END *>
    | GoHome:
      eve.AddToTail(hwnd, eve.DoAction, ORD(act.GotoExec));
    | CodeAt:
      addr := 0;
      ok := mem.Get(p^.addr+p^.curr*dt.Types[p^.Mode].len, sys.ADR(addr), 4);
      IF ok THEN
        IF mod.SetNewPosByAddr(addr) THEN
          eve.AddToTail(std.Wnds[std.MainWindow].hwnd, eve.Rise, 0);
        ELSE
          std.ErrorNo(mes.Incorrect_address);
        END;
      END;
    | DumpAt:
      addr := 0;
      ok := mem.Get(p^.addr+p^.curr*dt.Types[p^.Mode].len, sys.ADR(addr), 4);

      IF ok THEN
        fmt.print(dv.DumpAddrStr, 'A:%$8X', addr);
        IF InitDumpWindow(win.Invalid_H) THEN
          eve.AddToTail(hwnd, eve.Redraw, 0);
        ELSE
          eve.AddToTail(std.ErrorMsg, eve.Rise, 0);
        END;
      END;
    ELSE
      std.DefaultProc(hwnd, msg);
    END;


  | eve.QueryAction:
   <* PUSH *>
   <* WOFF902+ *>
    CASE VAL(act.ACTION, msg.par MOD 100H) OF
   <* POP *>
    | act.Stack:
      act.ConfirmQueryByCond(TRUE);
      RETURN;

    | act.ContextMenu, act.ChangeTypes:
      act.ConfirmQuery;

   <* IF DEST_XDS THEN *>
    | act.Access:
      act.ConfirmQuery;
   <* END *>

    ELSE
      std.DefaultProc(hwnd, msg);
    END;

  | eve.DoAction:
   <* PUSH *>
   <* WOFF902+ *>
    CASE VAL(act.ACTION, msg.par MOD 100H) OF
   <* POP *>
    | act.ContextMenu:
      SetStackContextList;
      WITH size DO
        puw.PopupWindow(x1+(x2-x1) DIV 2, y1+(y2-y1) DIV 2, hwnd, StackContext);
      END;

    | act.GotoExec:
      p^.addr := p^.addr_first;
      p^.curr := 0;
      eve.AddToTail(hwnd, eve.Redraw, 0);

    | act.ChangeTypes:
      WITH p^ DO
        addr := addr + curr*dt.Types[Mode].len;
        curr := 0;
        Mode := act.ChangeTypesID;
      END;
      eve.AddToTail(hwnd, eve.Redraw, 0);

    | act.ChangeOrigin:
      std.OpenUniversalDialog(mes.InputAddress, ChangeOrigin, dv.DumpAddrStr);

   <* IF DEST_XDS THEN *>
    | act.Access:
      fmt.print(buf,'A:%$8X', p^.addr + p^.curr * dt.Types[p^.Mode].len);
      dbr.SetAccessAttr(buf, dt.Types[p^.Mode].len);
      act.ExecuteAction (act.Access, act.mode_silent);
      RETURN;
   <* END *>

    ELSE
      std.DefaultProc(hwnd, msg);
    END;

  | eve.KbHit:
    CASE msg.par OF
    | key.CtrlW:
      std.Move_Resize_kbd(hwnd, 14+dt.Types[p^.Mode].s_len+1, 2);
    | key.Enter:
      p := win.GetAMPtr(hwnd);
      fmt.print(buf,'Memory at %$8X', p^.addr+p^.curr*dt.Types[p^.Mode].len);
      std.IniInputDialog(ChangeStack, FALSE, buf);
    | key.AltHome:
      p := win.GetAMPtr(hwnd);
      p^.addr := p^.addr_first;
      p^.curr := 0;
      eve.AddToTail(hwnd, eve.Redraw, 0);
    | key.PgUp, key.PgDn:
      size := win.GetWindowSize(hwnd);
      IF cells = 1 THEN
        IF msg.par = key.PgUp THEN
          IF p^.addr - dt.Types[p^.Mode].len >= p^.min THEN
            DEC(p^.addr, height*dt.Types[p^.Mode].len);
            IF p^.addr < p^.min THEN
              IF p^.curr > (p^.min - p^.addr) DIV dt.Types[p^.Mode].len THEN
                DEC(p^.curr, (p^.min - p^.addr) DIV dt.Types[p^.Mode].len)
              END;
              p^.addr := p^.min;
            END;
          ELSE
            p^.curr := 0;
          END;
        ELSE
          IF p^.addr + height*dt.Types[p^.Mode].len <= p^.max THEN
            IF p^.addr + ( height + p^.curr)*dt.Types[p^.Mode].len <= p^.max THEN
              INC(p^.addr, height*dt.Types[p^.Mode].len);
            ELSE
              p^.addr := p^.max + 1 - height*dt.Types[p^.Mode].len;
              p^.curr := height - 1;
            END;
          ELSE
            p^.curr := std.Min(height - 1, (p^.max - p^.addr) DIV dt.Types[p^.Mode].len);
          END;
        END;
        eve.AddToTail(hwnd, eve.Redraw, 0);
      END;
    | key.CtrlHome:
      p := win.GetAMPtr(hwnd);
      p^.curr := 0;
      eve.AddToTail(hwnd, eve.Redraw, 0);
    | key.CtrlEnd:
      IF width > 12 THEN
        p^.curr := std.Min(cells*height - 1, (p^.max - p^.addr) DIV dt.Types[p^.Mode].len);
        IF p^.addr + (p^.curr)*dt.Types[p^.Mode].len <= p^.max THEN
          IF (p^.max - (p^.addr + (p^.curr)*dt.Types[p^.Mode].len) < dt.Types[p^.Mode].len-1) THEN
            IF p^.curr > 0 THEN
              DEC(p^.curr)
            ELSE
              IF p^.addr - dt.Types[p^.Mode].len >= p^.addr_first THEN
                DEC(p^.addr, dt.Types[p^.Mode].len);
              END;
            END;
          END;
        END;
        eve.AddToTail(hwnd, eve.Redraw, 0);
      END;
    | key.Up, key.Down:
      IF width > 12 THEN
        WITH p^ DO
          IF msg.par = key.Down THEN
            IF addr + (curr+cells)*dt.Types[Mode].len <= max THEN
              IF max - (addr + (curr+cells)*dt.Types[Mode].len) >= dt.Types[p^.Mode].len-1 THEN
                IF (curr + 1) = height THEN
                  INC(addr, cells*(dt.Types[Mode].len))
                ELSE
                  INC(curr, cells)
                END;
              END;
            END;
          ELSE
            IF curr < cells THEN
              IF addr >= min + dt.Types[Mode].len THEN
                DEC (addr, cells*(dt.Types[Mode].len));
                IF addr < min THEN
                  IF curr > (min - addr) DIV dt.Types[Mode].len THEN
                    DEC (curr, (min - addr) DIV dt.Types[Mode].len)
                  END;
                  addr := min;
                END;
              ELSE
                p^.curr := 0;
              END;
            ELSE
              DEC (curr, cells);
            END;
          END;
        END;
        eve.AddToTail(hwnd, eve.Redraw, 0);
      END;
    ELSE
      std.DefaultProc(hwnd, msg)
    END;
  ELSE
    std.DefaultProc(hwnd, msg)
  END;
END StackHandler;

TYPE
  SET_REG = SET OF REG_STATE;
  REG_ARRAY=ARRAY [0..kt.ShownRegs-1] OF RECORD
                                            reg_no : CARDINAL;
                                            val    : CARDINAL;
                                            state  : SET_REG;
                                            Index_W: CARDINAL;
                                            Index_R: CARDINAL;
                                          END;

VAR
  R_Array: REG_ARRAY;
  R_curr : CARDINAL;
  RegsReady: BOOLEAN;

PROCEDURE IniRegisters;
VAR
  i: CARDINAL;
BEGIN
  ASSERT(NOT RegsReady);
  FOR i:=0 TO kt.ShownRegs-1 DO
    WITH R_Array[i] DO
      state := SET_REG{};
      IF kex.Loaded THEN
        ASSERT(mem.GetReg(reg_no, val));
       <* IF TARGET_VAX THEN *>
        IF opt.TraceRegisters THEN
          IF reg_no < 2000 THEN
            IF Index_R = 0 THEN ASSERT(kme.SetRegTrace(Read, reg_no, Index_R)); END;
            IF Index_W = 0 THEN ASSERT(kme.SetRegTrace(Write, reg_no, Index_W)); END;
          END;
        ELSE
          Index_W := 0;
          Index_R := 0;
        END;
       <* ELSE *>
        Index_W := 0;
        Index_R := 0;
       <* END *>
      ELSE
        val := 0;
        Index_W := 0;
        Index_R := 0;
      END;
    END;
  END;
END IniRegisters;


PROCEDURE ClearRegisters;
VAR
  i: CARDINAL;
BEGIN
  RegsReady := FALSE;
  FOR i:=0 TO kt.ShownRegs-1 DO
    WITH R_Array[i] DO

      reg_no := kt.Registers[i].reg_no;
      state := SET_REG{};
      IF kex.Loaded THEN
        ASSERT(mem.GetReg (reg_no, val));

<* IF DEST_K26 THEN *>
      IF Index_R # 0 THEN
        ASSERT(kme.RemoveRegTrace(Index_R));
      END;
      IF Index_W # 0 THEN
        ASSERT(kme.RemoveRegTrace(Index_W));
      END;
<* END *>

      ELSE
        val := 0;
        Index_W := 0;
        Index_R := 0;
      END;
    END;
  END;
END ClearRegisters;

PROCEDURE GetRegOldValue(no: CARDINAL): CARDINAL;
VAR
  i: CARDINAL;
BEGIN
  FOR i := 0 TO HIGH(R_Array) DO
    WITH R_Array[i] DO
      IF reg_no = no THEN
        RETURN R_Array[reg_no].val;
      END;
    END;
  END;
END GetRegOldValue;

PROCEDURE ChangeReg(no: CARDINAL; st: REG_STATE);
VAR
  i: CARDINAL;
BEGIN
  FOR i := 0 TO HIGH(R_Array) DO
    WITH R_Array[i] DO
      IF reg_no = no THEN
        ASSERT(mem.GetReg(reg_no, val));
        INCL(state, st);
        RETURN;
      END;
    END;
  END;
END ChangeReg;


(* Поиск регистра по собственному индексу диалога *)
PROCEDURE FindRegisterByIndex (Index: CARDINAL; VAR RegNo: CARDINAL): BOOLEAN;
VAR
  i: CARDINAL;
BEGIN
  FOR i := 0 TO HIGH(R_Array) DO
    WITH R_Array[i] DO
      IF (Index_W = Index) OR (Index_R = Index) THEN
        RegNo := reg_no;
        RETURN TRUE;
      END;
    END;
  END;
  RETURN FALSE;
END FindRegisterByIndex;


PROCEDURE ClearRegsState;
VAR
  i: CARDINAL;
BEGIN
  FOR i:=0 TO kt.ShownRegs-1 DO
    WITH R_Array[i] DO
      ASSERT(mem.GetReg(reg_no, val));
      state := SET_REG{};
    END;
  END;
END ClearRegsState;

(* Оттображает изменения регистров во внутренней структуре *)
PROCEDURE RefreshRegs;
VAR
  i      : CARDINAL;
  new_val: kt.REG_VALUE;
BEGIN
  FOR i:=0 TO kt.ShownRegs-1 DO
    WITH R_Array[i] DO
      ASSERT(mem.GetReg(reg_no, new_val));
      IF CARDINAL(new_val) # val THEN
        val := CARDINAL(new_val);
        INCL(state, Write);
      END;
    END;
  END;
END RefreshRegs;

PROCEDURE ChangeRON(hwnd: win.HWND):BOOLEAN;
VAR
  val: kt.ADDRESS;
  RegName: xs.String;
  buf: xs.String;
BEGIN
  IF NOT exp.GetCardValue(mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, std.NewValue, val) THEN
    std.SetErrorNo(exp.error);
    RETURN FALSE;
  ELSE
    IF NOT mem.SetReg(R_Array[R_curr].reg_no, val) THEN
      IF NOT exe.GetRegName (R_Array[R_curr].reg_no, RegName) THEN COPY('', RegName); END;
      pro.GetMsg(mes.WriteErrorRegister, buf);
      std.SetErrorMsg (buf, RegName);
      RETURN FALSE;
    END;
    R_Array[R_curr].val := val;
    INCL(R_Array[R_curr].state, Write);
    IF kt.EIP = R_Array[R_curr].reg_no THEN
      dex.PointExecLine (mem.GetIP());
    ELSE
      dw.RecalcWatches;
    END;
    eve.AddToTail(eve.AllWindows, eve.Redraw, 1);
    eve.AddToTail(hwnd, eve.Hide, 0);
    eve.Flush;
    RETURN TRUE;
  END;
END ChangeRON;


PROCEDURE RegProc(hwnd: win.HWND; msg: eve.MSG);

<* IF TARGET_x86 THEN *>

CONST
  FN = 8;

TYPE
  FF = ARRAY [0..FN-1] OF CARDINAL;
  FC = ARRAY [0..FN-1] OF CHAR;

CONST
  fl_pos = FF { 11,  10,  09,  07,  06,  04,  02,  00};
  fl_chr = FC {'O', 'D', 'I', 'S', 'Z', 'A', 'P', 'C'};

  SeparateRegs = 2;

<* ELSIF TARGET_VAX THEN *>

TYPE
  FF = ARRAY [0..7] OF CARDINAL;

CONST
  fl_pos = FF {0,1,2,3,4,5,6,7};

  SeparateRegs = 1;

<* ELSIF TARGET_M68K THEN *>

CONST
  FN = 5;

TYPE
  FF = ARRAY [0..FN-1] OF CARDINAL;
  FC = ARRAY [0..FN-1] OF CHAR;

CONST
  fl_pos = FF {  4,   3,   2,   1,   0};
  fl_chr = FC {'S', 'Z', 'A', 'P', 'C'};

  SeparateRegs = 1;
<* END *>

VAR
  i      : CARDINAL;
  x, y   : CARDINAL;
  xi, yi : CARDINAL;
  val_str: ARRAY [0..31] OF CHAR;
  attr   : crt.ATTR;
  size   : crt.SZ;
  buf    : xs.String;
  Flags  : BITSET;

  RegContextList: act.CONTEXT_LIST;

CONST
  ContextItem_ViewAs = 2;

VAR
  name: xs.String;
  main: BOOLEAN;
  item: CARDINAL;


  PROCEDURE SetRegistersContextList;
  BEGIN
    RegContextList[0] := act.CONTEXT{ act.do_action, act.Dump };
    IF std.MakeSubmenuViewAs (RegContextList[ContextItem_ViewAs]) THEN
      RegContextList[1] := act.CONTEXT{ act.separate };
      RegContextList[ContextItem_ViewAs+1] := act.EMPTY_CONTEXT;
    ELSE
      RegContextList[1] := act.EMPTY_CONTEXT;
    END;
  END SetRegistersContextList;


BEGIN
  CASE msg.ID OF
  | eve.Hide:
    std.DefaultProc(hwnd, msg);
    ClearRegisters;

  | eve.Mouse_Pressed, eve.Mouse_Dbl, eve.Mouse_Moved:
    ASSERT(win.GetRelMouse(hwnd, msg, x, y));
    size := win.GetWindowSize(hwnd);
    IF msg.ID = eve.Mouse_Pressed THEN
      CASE crt.GetControl (size, x, y, dlt.WND_CTRL_SET{dlt.WinCtrl_Close}) OF
      | dlt.WinCtrl_Close:
        eve.AddToTail(hwnd, eve.Hide, 0);
        RETURN;
      ELSE
        WITH size DO
          IF std.CheckFrame(size, x1+x, y1+y) THEN
            std.Move_Resize_mou(hwnd, x2-x1+1, 0, x1+x, y1+y);
            RETURN;
          END;
        END;
      END;
    END;
    FOR i := 0 TO kt.UserRegs-1 DO
      xi := 1 + (i MOD 2)*14;
      yi := 1 + (i DIV 2);
      IF (xi <= x) AND (x < xi+14) AND (y = yi) THEN
        R_curr := i;
        eve.AddToTail(hwnd, eve.Redraw, 0);
        IF msg.ID = eve.Mouse_Dbl THEN
          eve.AddToTail(hwnd, eve.KbHit, key.Enter);
        END;
        RETURN;
      END;
    END;
   <* IF TARGET_VAX THEN *>
    FOR i := kt.UserRegs TO kt.ShownRegs-2 DO
      xi := 1 + (i MOD 2)*14;
      yi := 1 + (i DIV 2) + 1;
      IF (xi <= x) AND (x < xi+14) AND (y = yi) THEN
        R_curr := i;
        eve.AddToTail(hwnd, eve.Redraw, 0);
        IF msg.ID = eve.Mouse_Dbl THEN
          eve.AddToTail(hwnd, eve.KbHit, key.Enter);
        END;
        RETURN;
      END;
    END;
    IF (1 <= x) AND (x <= 14) AND (y = (kt.ShownRegs DIV 2) + 3) THEN
      R_curr := kt.ShownRegs - 1;
      eve.AddToTail(hwnd, eve.Redraw, 0);
      IF msg.ID = eve.Mouse_Dbl THEN
        eve.AddToTail(hwnd, eve.KbHit, key.Enter);
      END;
      RETURN;
    END;
  <* ELSE *>
    FOR i := kt.UserRegs TO kt.ShownRegs-1 DO
      yi := (kt.UserRegs DIV 2) + 2 + i - kt.UserRegs;
      IF (1 <= x) AND (x <= 14) AND (y = yi) THEN
        R_curr := i;
        eve.AddToTail(hwnd, eve.Redraw, 0);
        IF msg.ID = eve.Mouse_Dbl THEN
          eve.AddToTail(hwnd, eve.KbHit, key.Enter);
        END;
        RETURN;
      END;
    END;
  <* END  *>

  | eve.R_Mouse:
    size := win.GetWindowSize(hwnd);
    ASSERT(win.GetMouse(msg, x, y));
    IF NOT std.CheckFrame(size, x, y) THEN
      eve.AddToTail(hwnd, eve.Mouse_Pressed, msg.par);
      eve.Flush;
      SetRegistersContextList;
      puw.PopupWindow(x, y, hwnd, RegContextList);
    END;

  | eve.Paint, eve.Redraw:
    crt.FillWindow(hwnd, ' ', crt.Reg[crt.Reg_Background]);
    FOR i:=0 TO kt.UserRegs-1 DO
      fmt.print(val_str, '%$8X', R_Array[i].val);
      crt.SetPos( 1 + (i MOD 2)*14,  (i DIV 2) + 1 );
      fmt.print(buf, " %-13.13s", kt.Registers[i].name);
      IF (i = R_curr) AND (hwnd = win.ActiveWindow) THEN
        crt.WrStr(hwnd, buf, crt.Reg[crt.Reg_Current]);
      ELSE
        crt.WrStr(hwnd, buf, crt.Reg[crt.Reg_Normal]);
      END;
      crt.SetPos( 6 + (i MOD 2)*14, (i DIV 2) + 1);
     <* IF TARGET_VAX THEN *>
      IF Write IN R_Array[i].state THEN
        IF Read  IN R_Array[i].state THEN
          attr := crt.Reg[crt.Reg_Both]
        ELSE
          attr := crt.Reg[crt.Reg_Written]
        END;
      ELSIF Read  IN R_Array[i].state THEN
        attr := crt.Reg[crt.Reg_Readed]
      ELSE
        IF (i = R_curr) AND (hwnd = win.ActiveWindow) THEN
          attr := crt.Reg[crt.Reg_Current];
        ELSE
          attr := crt.Reg[crt.Reg_Normal];
        END;
      END;
      crt.WrStr(hwnd, val_str, attr);
     <* ELSE *>
      IF Write IN R_Array[i].state THEN
        attr := crt.Reg[crt.Reg_Written]
      ELSE
        IF (i = R_curr) AND (hwnd = win.ActiveWindow) THEN
          attr := crt.Reg[crt.Reg_Current];
        ELSE
          attr := crt.Reg[crt.Reg_Normal];
        END;
      END;
      crt.WrStr(hwnd, val_str, attr);
      IF (i = R_curr) AND (hwnd = win.ActiveWindow) THEN
        crt.HilitePart(hwnd, (i DIV 2)+1, (i MOD 2)*14+1, (i MOD 2)*14+15, crt.Bg(crt.Reg[crt.Reg_Current]));
      END;
     <* END *>
    END;
   <* IF TARGET_VAX THEN *>
    FOR i:=kt.UserRegs TO kt.ShownRegs-2 DO
      fmt.print(val_str, '%$8X', R_Array[i].val);
      crt.SetPos( 1 + (i MOD 2)*14,  (i DIV 2) + 1 + 1);
      fmt.print(buf, " %-13.13s", kt.Registers[i].name);
      IF (i = R_curr) AND (hwnd = win.ActiveWindow) THEN
        crt.WrStr(hwnd, buf, crt.Reg[crt.Reg_Current]);
      ELSE
        crt.WrStr(hwnd, buf, crt.Reg[crt.Reg_Normal]);
      END;
      crt.SetPos( 6 + (i MOD 2)*14, (i DIV 2) + 1 + 1);
      IF Write IN R_Array[i].state THEN
        IF Read  IN R_Array[i].state THEN
          attr := crt.Reg[crt.Reg_Both]
        ELSE
          attr := crt.Reg[crt.Reg_Written]
        END;
      ELSIF Read  IN R_Array[i].state THEN
        attr := crt.Reg[crt.Reg_Readed]
      ELSE
        IF (i = R_curr) AND (hwnd = win.ActiveWindow) THEN
          attr := crt.Reg[crt.Reg_Current];
        ELSE
          attr := crt.Reg[crt.Reg_Normal];
        END;
      END;
      crt.WrStr(hwnd, val_str, attr);
    END;
    crt.SetPos(0, (kt.ShownRegs DIV 2) + 2);
    crt.WrStr(hwnd, "─────────────────────────────────────────", crt.Reg[crt.Reg_Frame]);
    fmt.print(val_str, '%$8X', R_Array[kt.ShownRegs - 1].val);
    crt.SetPos( 1 ,  (kt.ShownRegs DIV 2) + 3);
    fmt.print(buf, " %-13.13s", kt.Registers[kt.ShownRegs - 1].name);
    IF (kt.ShownRegs - 1 = R_curr) AND (hwnd = win.ActiveWindow) THEN
      crt.WrStr(hwnd, buf, crt.Reg[crt.Reg_Current]);
    ELSE
      crt.WrStr(hwnd, buf, crt.Reg[crt.Reg_Normal]);
    END;
    crt.SetPos( 6 , (kt.ShownRegs DIV 2) + 3);
    IF Write IN R_Array[kt.ShownRegs - 1].state THEN
      IF Read  IN R_Array[kt.ShownRegs - 1].state THEN
        attr := crt.Reg[crt.Reg_Both]
      ELSE
        attr := crt.Reg[crt.Reg_Written]
      END;
    ELSIF Read  IN R_Array[kt.ShownRegs - 1].state THEN
      attr := crt.Reg[crt.Reg_Readed]
    ELSE
      IF (kt.ShownRegs - 1 = R_curr) AND (hwnd = win.ActiveWindow) THEN
        attr := crt.Reg[crt.Reg_Current];
      ELSE
        attr := crt.Reg[crt.Reg_Normal];
      END;
    END;
    crt.WrStr(hwnd, val_str, attr);

    Flags := BITSET(R_Array[kt.ShownRegs-1].val);
    crt.SetPos( 20, (kt.ShownRegs DIV 2) + 3 );
    attr := crt.Reg[crt.Reg_Normal];
    crt.WrStr(hwnd, 'CVZNTIFD', attr);
    FOR i := 0 TO HIGH(fl_pos) DO
      crt.SetPos( 20+i , (kt.ShownRegs DIV 2) + 4 );
      IF fl_pos[i] IN Flags THEN
        crt.WrStr(hwnd, '+', attr);
      ELSE
        crt.WrStr(hwnd, '-', attr);
      END;
    END;
   <* ELSE *>
    FOR i:=kt.UserRegs TO kt.ShownRegs-1 DO
      crt.SetPos(1, (kt.UserRegs DIV 2) + 2 + i - kt.UserRegs);
      fmt.print(buf, " %-13.13s", kt.Registers[i].name);
      fmt.print(val_str, '%$8X', R_Array[i].val);
      IF (i = R_curr) AND (hwnd = win.ActiveWindow) THEN
        crt.WrStr(hwnd, buf, crt.Reg[crt.Reg_Current]);
      ELSE
        crt.WrStr(hwnd, buf, crt.Reg[crt.Reg_Normal]);
      END;
      crt.SetPos( 6 , (kt.UserRegs DIV 2) + 2 + i - kt.UserRegs);
      IF Write IN R_Array[i].state THEN
        attr := crt.Reg[crt.Reg_Written]
      ELSE
        IF (i = R_curr) AND (hwnd = win.ActiveWindow) THEN
          attr := crt.Reg[crt.Reg_Current];
        ELSE
          attr := crt.Reg[crt.Reg_Normal];
        END;
      END;
      crt.WrStr(hwnd, val_str, attr);
      IF (i = R_curr) AND (hwnd = win.ActiveWindow) THEN
        crt.HilitePart(hwnd, (kt.UserRegs DIV 2)+2+i-kt.UserRegs, 1, 15, crt.Bg(crt.Reg[crt.Reg_Current]));
      END;
    END;
    Flags := BITSET(R_Array[kt.UserRegs+1].val);
    crt.SetPos( 20, (kt.UserRegs DIV 2) + 2 );
    attr := crt.Reg[crt.Reg_Normal];
    crt.WrStr(hwnd, fl_chr, attr);
    FOR i := 0 TO HIGH(fl_pos)  DO
      crt.SetPos (20+i, (kt.UserRegs DIV 2) + 3 );
      IF fl_pos[i] IN Flags THEN
        crt.WrStr(hwnd, '+', attr);
      ELSE
        crt.WrStr(hwnd, '-', attr);
      END;
    END;
   <* END  *>
    crt.SetPos(0, (kt.UserRegs DIV 2) + 1);
    crt.WrStr(hwnd, "─────────────────────────────────────────", crt.Reg[crt.Reg_Frame]);
    size := win.GetWindowSize(hwnd);
    attr := crt.Reg[crt.Reg_Frame];
    IF hwnd = win.ActiveWindow THEN
      crt.DrawFrame(hwnd, size, crt.Double, attr);
      crt.DrawControls(hwnd, size, attr, dlt.WND_CTRL_SET{dlt.WinCtrl_Close});
      attr := crt.Reg[crt.Reg_ActiveHeader];
    ELSE
      crt.DrawFrame(hwnd, size, crt.Single, attr);
      crt.DrawControls(hwnd, size, attr, dlt.WND_CTRL_SET{dlt.WinCtrl_Close});
      attr := crt.Reg[crt.Reg_Header];
    END;
    crt.DrawHeader (hwnd, size, attr);
    IF msg.ID = eve.Redraw THEN crt.UpdateRect(size) END;

  | eve.QueryItem:
    std.GetItem (msg.par, main, item);
    IF main THEN
      ASSERT(item = ContextItem_ViewAs);
      act.ConfirmQueryByCond (lst.EquNamesNo() > 0);
    ELSE
      act.ConfirmQueryByCond (item < lst.EquNamesNo());
    END;

  | eve.ContextItem:
    std.GetItem (msg.par, main, item);
    ASSERT(NOT main);
    lst.GetName (item, name);
    lst.GetEquName (name, buf);
    fmt.print (dv.VarName, '%s<@%s>', buf, kt.Registers[R_curr].name);
    act.ExecuteAction (act.Examine, act.mode_silent);

  | eve.QueryAction:
    CASE VAL(act.ACTION, msg.par MOD 100H) OF
    | act.ContextMenu:
      act.ConfirmQuery;
    | act.AddWatch:
      act.ConfirmQuery;
    | act.Dump:
      act.ConfirmQuery;
   <* IF DEST_XDS THEN *>
    | act.Access:
      act.ConfirmQuery;
   <* END *>
    ELSE
      std.DefaultProc(hwnd, msg);
    END;

  | eve.DoAction:
    CASE VAL(act.ACTION, msg.par MOD 100H) OF
    | act.ContextMenu:
      SetRegistersContextList;
      size := win.GetWindowSize(hwnd);
      WITH size DO
        puw.PopupWindow(x1+(x2-x1) DIV 2, y1+(y2-y1) DIV 2, hwnd, RegContextList);
      END;
    | act.AddWatch:
      fmt.print(dv.expr, '@%s', kt.Registers[R_curr].name);
      act.ExecuteAction (act.AddWatch, act.mode_silent);
    | act.Dump:
      fmt.print(dv.DumpAddrStr, '@%s', kt.Registers[R_curr].name);
      act.ExecuteAction (act.Dump, act.mode_silent);
   <* IF DEST_XDS THEN *>
    | act.Access:
      fmt.print(buf, '@%s', kt.Registers[R_curr].name);
      dbr.SetAccessAttr(buf, 1);
      act.ExecuteAction (act.Access, act.mode_silent);
   <* END *>
    ELSE
      std.DefaultProc(hwnd, msg);
    END;

  | eve.KbHit:
    CASE msg.par OF
    | key.Enter:
        IF kex.Loaded THEN
          fmt.print(buf, '%s', kt.Registers[R_curr].name);
          fmt.print(std.NewValue, exp.Fmt_RegWindow, R_Array[R_curr].val);
          std.IniInputDialog(ChangeRON, TRUE, buf);
        ELSE
          crt.Beep;
        END;
    | key.Tab  :
        IF R_curr < kt.ShownRegs-1 THEN
          INC(R_curr);
        ELSE
          R_curr := 0;
        END;
        eve.AddToTail(hwnd, eve.Redraw, 0);
    | key.Left :
        IF R_curr < kt.ShownRegs-SeparateRegs THEN
          IF ODD(R_curr) THEN
            DEC(R_curr);
            eve.AddToTail(hwnd, eve.Redraw, 0);
          END;
        END;
    | key.Right:
        IF R_curr < kt.ShownRegs-SeparateRegs THEN
          IF NOT ODD(R_curr) THEN
            INC(R_curr);
            eve.AddToTail(hwnd, eve.Redraw, 0);
          END;
        END;

    | key.Down:
<* IF TARGET_VAX THEN *>
<* PUSH *>
<* WOFF311+ *>
<* WOFF313+ *>
<* WOFF900+ *>
<* WOFF902+ *>
<* END *>
        IF R_curr < kt.ShownRegs-SeparateRegs-1 THEN
          IF R_curr < kt.ShownRegs-2 THEN
            INC(R_curr, 2);
            eve.AddToTail(hwnd, eve.Redraw, 0);
          END;
        ELSIF R_curr >= kt.ShownRegs-SeparateRegs THEN
          IF R_curr < kt.ShownRegs-1 THEN
            INC(R_curr);
            eve.AddToTail(hwnd, eve.Redraw, 0);
          END;
        END;
<* IF TARGET_VAX THEN *>
<* POP *>
<* END *>

    | key.Up:
        IF R_curr <= kt.ShownRegs-SeparateRegs THEN
          IF R_curr >= 2 THEN
            DEC(R_curr, 2);
            eve.AddToTail(hwnd, eve.Redraw, 0);
          END;
        ELSE
          DEC(R_curr);
          eve.AddToTail(hwnd, eve.Redraw, 0);
        END;
    ELSE
      std.DefaultProc(hwnd, msg);
    END;
  ELSE
    std.DefaultProc(hwnd, msg);
  END;
END RegProc;

PROCEDURE InitRegWindow(show: BOOLEAN);
BEGIN
  WITH std.Wnds[std.RegWindow] DO
    IF hwnd = win.Invalid_H THEN
      hwnd := win.RegisterWindow(RegProc,0);
      ASSERT(hwnd # win.Invalid_H);
      win.SetWindowSize(hwnd, size);
      win.SetMovable(hwnd);
      win.SetResizable(hwnd, FALSE, TRUE);
      win.SetSwitchable(hwnd);
      win.SetHeader(hwnd, mes.Registers);
    END;
    R_curr := 0;
    IF NOT win.Visible(hwnd) & NOT RegsReady THEN
      IniRegisters;
    END;
    IF show THEN
      eve.AddToTail(hwnd, eve.Rise, 0);
    END;
  END;
END InitRegWindow;


<* IF DEST_XDS AND (TARGET_OS = "WINNT") THEN *>

TYPE
  LIST_EXT_FLOAT_REGISTERS = RECORD
                               hex_mode: BOOLEAN;
                             END;

  PLIST_EXT_FLOAT_REGISTERS = POINTER TO LIST_EXT_FLOAT_REGISTERS;

  REGISTER_STATUS = ( Zero, Valid, Special, Empty );

  REGISTER_NAME = ARRAY [0..9] OF CHAR;

  REGISTER_NAMES = ARRAY REGISTER_STATUS OF REGISTER_NAME;


CONST
  RegisterNames = REGISTER_NAMES{ 'Zero', 'Valid', 'Special', 'Empty' };

  first_reg_line = 7;



PROCEDURE ChangeFloatReg (hwnd: win.HWND): BOOLEAN;
VAR
  value: LONGLONGREAL;
  p: std.PLIST;
  FloatRegs: kt.FLOATING_REGISTER_CACHE;
BEGIN
  IF exp.GetRealValue (mod.Curr^.Pos.ComN, mod.Curr^.Pos.ModN, std.NewValue, value) THEN
    p := win.GetAMPtr(std.Wnds[std.FloatRegWindow].hwnd);
    -- программа должна быть загружена
    IF NOT mem.GetFloatRegs (FloatRegs) THEN
      std.SetErrorMsg ("Cannot read floating registers");
      RETURN FALSE;
    END;
    FloatRegs.RegisterData[p^.curr-first_reg_line] := value;
    IF mem.SetFloatRegs(FloatRegs) THEN
      eve.AddToTail(eve.AllWindows, eve.Redraw, 1);
      eve.AddToTail(hwnd, eve.Hide, 0);
      eve.Flush;
      RETURN TRUE;
    ELSE
      std.SetErrorMsg ("Cannot write to floating registers");
      RETURN FALSE;
    END;
  ELSE
    std.SetErrorNo(exp.error);
    RETURN FALSE;
  END;
END ChangeFloatReg;



PROCEDURE FloatRegistersHandler (hwnd: win.HWND; msg: eve.MSG);
VAR
  p   : std.PLIST;
  pext: PLIST_EXT_FLOAT_REGISTERS;
  size: crt.SZ;

  FloatRegs: kt.FLOATING_REGISTER_CACHE;


  PROCEDURE InitData;
  BEGIN
    p := win.GetAMPtr(hwnd);
    size := win.GetWindowSize(hwnd);
    -- поля + разделитель + флажки + регистры
    p^.N := 6+1+kt.FloatRegs;
    pext := PLIST_EXT_FLOAT_REGISTERS(p^.ext);
    IF kex.Loaded THEN
      ASSERT(mem.GetFloatRegs (FloatRegs));
    END;
  END InitData;



  PROCEDURE register_status (reg_no: CARDINAL): REGISTER_STATUS;
  VAR
    top, k: CARDINAL;
    status : REGISTER_STATUS;
  BEGIN
    top := FloatRegs.StatusWord DIV 2048 MOD 8;
    k := (top+reg_no) MOD 8;
    IF k*2 IN BITSET(FloatRegs.TagWord) THEN
      IF k*2+1 IN BITSET(FloatRegs.TagWord) THEN
        status := Empty;
      ELSE
        status := Zero;
      END;
    ELSE
      IF k*2+1 IN BITSET(FloatRegs.TagWord) THEN
        status := Special;
      ELSE
        status := Valid;
      END;
    END;
    RETURN status;
  END register_status;



  PROCEDURE write_line (num: CARDINAL);

    PROCEDURE trunc (value: sys.CARD32): sys.CARD16;
    BEGIN
      RETURN value MOD 010000H;
    END trunc;

    PROCEDURE check_flag (s, k: CARDINAL; VAR flag: ARRAY OF CHAR);
    BEGIN
      IF k IN BITSET(s) THEN
        COPY('1', flag);
      ELSE
        COPY('0', flag);
      END;
    END check_flag;


    PROCEDURE check_hex_mode (reg_no: CARDINAL; VAR s: ARRAY OF CHAR);
    VAR
      parr: POINTER TO ARRAY [0..SIZE(LONGLONGREAL)-1] OF sys.CARD8;
      k   : CARDINAL;
      tmp : xs.String;
    BEGIN
      IF pext^.hex_mode THEN
        fmt.print (s, '  %c ', CHR(263C));
        parr := sys.ADR(FloatRegs.RegisterData[reg_no]);
        FOR k := HIGH(parr^) TO 0 BY -1 DO
          fmt.print (tmp, "%$2X", parr^[k]);
          xs.Append (tmp, s);
        END;
      ELSE
        COPY("", s);
      END;
    END check_hex_mode;

  VAR
    name  : xs.String;
    result: xs.String;
    state : xs.String;
    line  : xs.String;
    top   : CARDINAL;
    reg_no: CARDINAL;
    status: REGISTER_STATUS;
  BEGIN
    WITH p^ DO
      IF num < 6 THEN
        -- status and control word
        IF NOT kex.Loaded THEN
          COPY("?", result);
        END;
        CASE num OF
        | 0 : COPY("Control word", name);
              IF kex.Loaded THEN fmt.print (result, "%$4X", trunc(FloatRegs.ControlWord)); END;
        | 1 : COPY("Status word", name);
              IF kex.Loaded THEN fmt.print (result, "%$4X", trunc(FloatRegs.StatusWord)); END;
        | 2 : COPY("Tag word", name);
              IF kex.Loaded THEN fmt.print (result, "%$4X", trunc(FloatRegs.TagWord)); END;
        | 3 : COPY("Error offset", name);
              IF kex.Loaded THEN fmt.print (result, "%$8X", FloatRegs.ErrorOffset); END;
        | 4 : COPY("Data offset", name);
              IF kex.Loaded THEN fmt.print (result, "%$8X", FloatRegs.DataOffset); END;
        | 5 : COPY("Cr0Npx state", name);
              IF kex.Loaded THEN fmt.print (result, "%$4X", trunc(FloatRegs.Cr0NpxState)); END;
        END;
        fmt.print (line, '%-12s %s', name, result);
        crt.SetPos(2, num-frame+1);
        crt.WrStrFromPos(hwnd, line, Colors^[crt.List_Line], pos);
        IF kex.Loaded THEN
          -- флажки
         <* PUSH *>
         <* WOFF902+ *>
          CASE num OF
          | 0 : COPY("ie", name); check_flag (FloatRegs.StatusWord, 0, state);
          | 1 : COPY("de", name); check_flag (FloatRegs.StatusWord, 1, state);
          | 2 : COPY("ze", name); check_flag (FloatRegs.StatusWord, 2, state);
          | 3 : COPY("oe", name); check_flag (FloatRegs.StatusWord, 3, state);
          | 4 : COPY("ue", name); check_flag (FloatRegs.StatusWord, 4, state);
          | 5 : COPY("pe", name); check_flag (FloatRegs.StatusWord, 5, state);
          END;
          fmt.print (line, "%c %s=%s", CHR(263C), name, state);
          crt.SetPos(24, num-frame+1);
          crt.WrStrFromPos(hwnd, line, Colors^[crt.List_Line], pos);
          CASE num OF
          | 0 : COPY("sf", name); check_flag (FloatRegs.StatusWord, 6, state);
          | 1 : COPY("es", name); check_flag (FloatRegs.StatusWord, 7, state);
          | 2 : COPY("c0", name); check_flag (FloatRegs.StatusWord, 8, state);
          | 3 : COPY("c1", name); check_flag (FloatRegs.StatusWord, 9, state);
          | 4 : COPY("c2", name); check_flag (FloatRegs.StatusWord, 10, state);
          | 5 : COPY("c3", name); check_flag (FloatRegs.StatusWord, 14, state);
          END;
          fmt.print (line, "%c %s=%s", CHR(263C), name, state);
          crt.SetPos(31, num-frame+1);
          crt.WrStrFromPos(hwnd, line, Colors^[crt.List_Line], pos);
          CASE num OF
          | 0 : COPY("im", name); check_flag (FloatRegs.ControlWord, 0, state);
          | 1 : COPY("dm", name); check_flag (FloatRegs.ControlWord, 1, state);
          | 2 : COPY("zm", name); check_flag (FloatRegs.ControlWord, 2, state);
          | 3 : COPY("om", name); check_flag (FloatRegs.ControlWord, 3, state);
          | 4 : COPY("um", name); check_flag (FloatRegs.ControlWord, 4, state);
          | 5 : COPY("pm", name); check_flag (FloatRegs.ControlWord, 5, state);
          END;
          fmt.print (line, "%c %s=%s", CHR(263C), name, state);
          crt.SetPos(38, num-frame+1);
          crt.WrStrFromPos(hwnd, line, Colors^[crt.List_Line], pos);
          crt.SetPos(45, num-frame+1);
          CASE num OF
          | 0 : COPY("st", name);
                top := FloatRegs.StatusWord DIV 2048 MOD 8;
                fmt.print (state, "%d", top);
          | 1 : COPY("ic", name); check_flag (FloatRegs.ControlWord, 12, state);
          | 2 : COPY("pc", name); COPY("?", state);
                IF 8 IN BITSET(FloatRegs.ControlWord) THEN
                  IF 9 IN BITSET(FloatRegs.ControlWord) THEN
                    COPY('extended', state);
                  ELSE
                    COPY('reserved', state);
                  END;
                ELSE
                  IF 9 IN BITSET(FloatRegs.ControlWord) THEN
                    COPY('double', state);
                  ELSE
                    COPY('single', state);
                  END;
                END;
          | 3 : COPY("rc", name);
                IF 10 IN BITSET(FloatRegs.ControlWord) THEN
                  IF 11 IN BITSET(FloatRegs.ControlWord) THEN
                    COPY('truncate', state);
                  ELSE
                    COPY('down', state);
                  END;
                ELSE
                  IF 11 IN BITSET(FloatRegs.ControlWord) THEN
                    COPY('up', state);
                  ELSE
                    COPY('nearest', state);
                  END;
                END;
          ELSE
            fmt.print (line, "%c", CHR(263C));
            crt.WrStrFromPos(hwnd, line, Colors^[crt.List_Line], pos);
            RETURN;
          END;
          fmt.print (line, "%c %s=%s", CHR(263C), name, state);
          crt.WrStrFromPos(hwnd, line, Colors^[crt.List_Line], pos);
         <* POP *>
        END;
      ELSIF num = first_reg_line-1 THEN
        crt.SetPos (1, num-frame+1);
        crt.WrNChar (hwnd, size.x2, '─', Colors^[crt.List_Line]);
      ELSE
        COPY("", line);
        IF kex.Loaded THEN
          reg_no := num-first_reg_line;
          status := register_status (reg_no);
          CASE status OF
          | Zero:
            check_hex_mode (reg_no, name);
            fmt.print (line, 'ST(%d) %-28s%s', reg_no, "0.0", name);
          | Valid, Special:
            r2s.to_float (FloatRegs.RegisterData[reg_no], 17, 1, 17, 'E', FALSE, FALSE, result);
            check_hex_mode (reg_no, name);
            fmt.print (line, 'ST(%d) %-28s%s', reg_no, result, name);
          | Empty:
            fmt.print (line, 'ST(%d)', reg_no);
          END;
        END;
        crt.SetPos(2, num-frame+1);
        crt.WrStrFromPos(hwnd, line, Colors^[crt.List_Line], pos);
      END;
    END;
  END write_line;


VAR
  i, len: CARDINAL;
  last  : CARDINAL;
  name  : xs.String;
  reg_no: CARDINAL;
  status: REGISTER_STATUS;

BEGIN
  InitData;
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
    | act.FloatRegisters:
      act.ConfirmQueryByCond (kex.Loaded);
    ELSE
      std.ListBox (hwnd, msg);
    END;

  | eve.KbHit:
    CASE msg.par OF
    | key.Enter:
      IF kex.Loaded AND (p^.curr >= first_reg_line) THEN
        reg_no := p^.curr - first_reg_line;
        fmt.print(name, 'ST(%s)', reg_no);
        status := register_status (reg_no);
        CASE status OF
        | Zero, Valid:
          r2s.to_float (FloatRegs.RegisterData[reg_no], 17, 1, 17, 'E', FALSE, FALSE, std.NewValue);
          std.IniInputDialog (ChangeFloatReg, TRUE, RegisterNames[status]);
          RETURN;
        ELSE
        END;
      END;
      crt.Beep;
    ELSE
      std.ListBox(hwnd, msg);
    END;

  ELSE
    std.ListBox(hwnd, msg);
  END;
END FloatRegistersHandler;


PROCEDURE tglHexMode (check: BOOLEAN): BOOLEAN;
VAR
  p   : std.PLIST;
  pext: PLIST_EXT_FLOAT_REGISTERS;
BEGIN
  p := win.GetAMPtr(std.Wnds[std.FloatRegWindow].hwnd);
  pext := PLIST_EXT_FLOAT_REGISTERS(p^.ext);
  RETURN std.tglOption (pext^.hex_mode, check);
END tglHexMode;


PROCEDURE InitFloatRegWindow(show: BOOLEAN);
VAR
  p: std.PLIST;
  pext: PLIST_EXT_FLOAT_REGISTERS;
BEGIN
  WITH std.Wnds[std.FloatRegWindow] DO
    IF hwnd = win.Invalid_H THEN
      hwnd := win.RegisterWindow(FloatRegistersHandler, SIZE(std.LIST)+SIZE(PLIST_EXT_FLOAT_REGISTERS));
      ASSERT(hwnd # win.Invalid_H);
      win.SetWindowSize(hwnd, size);
      win.SetMovable(hwnd);
      win.SetResizable(hwnd, TRUE, TRUE);
      win.SetSwitchable(hwnd);
      win.SetHeaderByStr(hwnd, "Float registers");
      p := win.GetAMPtr(hwnd);
      p^ := std.EmptyList;
      WITH p^ DO
        Colors     := sys.ADR(crt.List);
        Frame      := crt.Double;
        locator    := NIL;
        actions[0] := act.CONTEXT{ act.toggler, 'Hex mode', tglHexMode };
        actions[1] := act.EMPTY_CONTEXT;
        ext        := sys.ADDADR(sys.ADR(p^), SIZE(std.LIST));
      END;
      pext := PLIST_EXT_FLOAT_REGISTERS(p^.ext);
      pext^.hex_mode := FALSE;
    END;
    IF show THEN
      eve.AddToTail(hwnd, eve.Rise, 0);
    END;
  END;
END InitFloatRegWindow;


PROCEDURE InitFloatRegDialog (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.FloatRegisters);
  IF NOT kex.Loaded THEN
    RETURN FALSE;
  END;
  IF mode # act.mode_check THEN
    InitFloatRegWindow (TRUE);
  END;
  RETURN TRUE;
END InitFloatRegDialog;

<* END *>


PROCEDURE InitRegDialog(action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.Registers);
  IF NOT kex.Loaded THEN RETURN FALSE; END;
  IF mode # act.mode_check THEN
    InitRegWindow(TRUE);
  END;
  RETURN TRUE;
END InitRegDialog;



PROCEDURE RiseDumpWindow (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.Dump);
  IF NOT kex.Loaded THEN RETURN FALSE; END;
  IF mode = act.mode_silent THEN
    IF InitDumpWindow (win.Invalid_H) THEN
      eve.AddToTail (win.ActiveWindow, eve.Redraw, 0);
    ELSE
      eve.AddToTail (std.ErrorMsg, eve.Rise, 0);
    END;
  ELSIF mode # act.mode_check THEN
    std.OpenUniversalDialog (mes.InputAddress, InitDumpWindow, dv.DumpAddrStr);
  END;
  RETURN TRUE;
END RiseDumpWindow;


PROCEDURE ResetStack;
VAR
  p: PDUMP;
  start, len: CARDINAL;
  access: kt.ATTRIBS;
BEGIN
  WITH std.Wnds[std.StackWindow] DO
    IF (hwnd = win.Invalid_H) THEN RETURN END;
    p := win.GetAMPtr(hwnd);
  END;
  WITH p^ DO
    addr_first := mem.GetSP();
    addr := addr_first;
    curr := 0;
    IF kex.Loaded AND mem.GetSegmentInfo (addr_first, start, len, access) THEN
     <* IF DEST_K26 THEN *>
      write:= (kt.write IN access);
     <* ELSE *>
      write:= TRUE;
     <* END *>
    ELSE
      start := addr_first;
      len   := 1;
      write := FALSE;
    END;
    min := addr_first;
    max := start + len - 1;
  END;
END ResetStack;


PROCEDURE InitStackWindow(show: BOOLEAN);
VAR
  p: PDUMP;
BEGIN
  WITH std.Wnds[std.StackWindow] DO
    IF hwnd = win.Invalid_H THEN
      hwnd := win.RegisterWindow(StackHandler, SIZE(DUMP));
      ASSERT(hwnd # win.Invalid_H);
      win.SetWindowSize(hwnd, size);
      win.SetMovable   (hwnd);
      win.SetSwitchable(hwnd);
      win.SetResizable (hwnd, FALSE, TRUE);
      win.SetHeader(hwnd, mes.Stack);
      p := win.GetAMPtr(hwnd);
      WITH p^ DO
        Mode := dt.st_dword;
        addr_first := 0;
        addr  := addr_first;
        curr  := 0;
        write := FALSE;
        min   := addr_first;
        max   := min;
      END;
    END;
    IF show THEN
      ResetStack;
      eve.AddToTail(hwnd, eve.Rise, 0);
    END;
  END;
END InitStackWindow;


PROCEDURE InitStack(action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.Stack);
  IF NOT kex.Loaded THEN RETURN FALSE; END;
  InitStackWindow (mode # act.mode_check);
  IF mode = act.mode_check THEN
    RETURN std.QueryAction(std.Wnds[std.StackWindow].hwnd, act.Stack);
  ELSE
    RETURN TRUE;
  END;
END InitStack;

PROCEDURE dummyChangeOrigin (action: act.ACTION; mode: act.ACTION_MODE): BOOLEAN;
BEGIN
  ASSERT(action = act.ChangeOrigin);
  RETURN mode # act.mode_check;
END dummyChangeOrigin;

PROCEDURE FindOpenedDumpWindow (hwnd: win.HWND): win.HWND;
BEGIN
  RETURN win.FindOpened (hwnd, DumpHandler);
END FindOpenedDumpWindow;

PROCEDURE GetDumpAttr (hwnd: win.HWND; VAR dump: DUMP);
VAR
  p: PDUMP;
BEGIN
  ASSERT( win.GetHandler (hwnd) = DumpHandler );
  p := win.GetAMPtr(hwnd);
  dump := p^;
END GetDumpAttr;


BEGIN
  std.Wnds[std.RegWindow  ].init := InitRegWindow;
  std.Wnds[std.StackWindow].init := InitStackWindow;

  act.IniAction(act.Stack,     InitStack);
  act.IniAction(act.Dump,      RiseDumpWindow);
  act.IniAction(act.ChangeOrigin, dummyChangeOrigin);
  act.IniAction(act.Registers, InitRegDialog);

 <* IF DEST_XDS THEN *>
  SaveDumpToFileHwnd := win.Invalid_H;
 <* IF TARGET_OS = "WINNT" THEN *>
  std.Wnds[std.FloatRegWindow].init := InitFloatRegWindow;
  act.IniAction(act.FloatRegisters, InitFloatRegDialog);
 <* END *>
 <* END *>

  ActiveDumpWindow := win.Invalid_H;
  DumpWindow  := win.Invalid_H;
  DumpDialog  := win.Invalid_H;

  ClearRegisters;
END Dlg_Mem.
