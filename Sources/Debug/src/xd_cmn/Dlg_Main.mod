IMPLEMENTATION MODULE Dlg_Main;

IMPORT Dialog;  -- Don't erase it!

IMPORT opt := Options;
IMPORT txt := Texts;
IMPORT key := Keys;
IMPORT fil := File;
IMPORT xs  := xStr;


IMPORT dt  := DI_Types;
IMPORT tls := DI_Tools;

IMPORT kt  := KrnTypes;
IMPORT kexe:= KrnExec;

IMPORT exe := ExeMain;
IMPORT mem := Exe_Mem;

IMPORT dmm := Dlg_Mem;
IMPORT cfg := DlgCfg;
IMPORT act := Dlg_Acts;
IMPORT dv  := Dlg_Vars;
IMPORT eve := DlgEvent;
IMPORT mod := DlgMods;
IMPORT dex := Dlg_Exec;
IMPORT crt := CRT;
IMPORT dlr := DlgReact;

<* IF DEST_XDS THEN *>

IMPORT Publics; -- Don't erase it!
IMPORT Dlg_ODS; -- Don't erase it!

<* IF (TARGET_OS = "WINNT") THEN *>

<* IF DEFINED (xd_debug) AND xd_debug THEN *>
IMPORT Dlg_TD;
<* END *>

<* END *>


<* ELSIF DEST_K26 THEN *>

IMPORT mdl := DlgModel;

<* END *>

IMPORT std := Dlg_Std;

<* IF TARGET_x86 THEN *>
IMPORT dbg := Krn_Dbg;
<* END *>


PROCEDURE CreateWindows;
VAR
  i: std.WINDOW;
  n: CARDINAL;
BEGIN
  FOR i := MIN(std.WINDOW) TO MAX(std.WINDOW) DO
    WITH std.Wnds[i] DO
      WITH size DO
        IF x2 >= opt.X THEN
          n := x2-opt.X;
          x2 := opt.X-1;
          IF x1 > n THEN
            DEC(x1, n+1);
          ELSE
            x1 := 0;
          END;
        END;
        IF y2 >= opt.Y THEN
          n := y2-opt.Y;
          y2 := opt.Y-1;
          IF y1+1 > n THEN
            DEC(y1, n+1);
          ELSE
            y1 := 1;
          END;
        END;
      END;
      IF visible_at_start THEN
        init(TRUE);
      END;
    END;
  END;
END CreateWindows;

PROCEDURE Ini;
BEGIN
  exe.ExecComp := dt.Invalid_Component;
  exe.ExecMod := dt.Invalid_Module;
  exe.ExecLine := MAX(CARDINAL);
  dv.FirstTime := TRUE;
  act.ExecuteAction (act.Pulldown, act.mode_silent);
  CreateWindows;
  std.InitErrorMsg;
END Ini;


PROCEDURE IniFromPack;
VAR
  pc: CARDINAL;
  ProgramName :  xs.String;
BEGIN
 <* IF TARGET_x86 THEN *>
  dbg.SwitchToDebugger;
 <* END *>
  IF dv.FirstTime THEN
    CreateWindows;
    act.ExecuteAction (act.Pulldown, act.mode_silent);
    eve.AddToTail(std.Wnds[std.MainWindow].hwnd, eve.Rise, 0);
  ELSE
    eve.AddToTail(eve.AllWindows, eve.Redraw, 1);
  END;

  IF dv.FirstTime THEN
    fil.ExtractFileName (opt.prog_name,  ProgramName);
    fil.RemoveExtension(ProgramName);
    cfg.LoadConfig (ProgramName);
    eve.Flush (); -- rise windows after load config
    IF NOT kexe.ProgramContextOk THEN
      mod.SetNewPos (0, 1, 1);
    END;
  END;

  ASSERT(kexe.Loaded);
  pc := mem.GetIP();
  dex.PointExecLine(pc);

  IF dv.FirstTime THEN dv.FirstTime := FALSE; END;

  std.InitErrorMsg;
  eve.AddToTail(std.Wnds[std.MainWindow].hwnd, eve.Rise, 0);

  dex.SetRTShandler;
END IniFromPack;


PROCEDURE SaveDebugSessionFile;
VAR
  ProgramName: xs.String;
BEGIN
  IF opt.SaveOpt THEN
    IF opt.prog_name # '' THEN
      COPY (opt.prog_name, ProgramName);
      fil.RemoveExtension (ProgramName);
      cfg.SaveConfig (ProgramName);
    END;
  END;
END SaveDebugSessionFile;


PROCEDURE Start();

  MODULE AutoSaveRestore;

  IMPORT txt;
  IMPORT opt;
  IMPORT crt;
  IMPORT dlr;

 <* IF DEST_K26 THEN *>
  IMPORT mdl;
 <* END *>

  VAR
    nf, re, em: txt.ErrorProc;
  BEGIN
    opt.in_dialog := TRUE;
    nf := txt.NotFound;
    re := txt.ReadError;
    em := txt.FileEmpty;
    txt.NotFound  := txt.dummyNotFound;
    txt.ReadError := txt.dummyReadError;
    txt.FileEmpty := txt.dummyFileEmpty;
   <* IF DEST_K26 THEN *>
    mdl.Ini;
   <* END *>
    crt.Ini;
    dlr.Ini;
  FINALLY
    dlr.Exi;
    crt.Exi;
   <* IF DEST_K26 THEN *>
    mdl.Exi;
   <* END *>
    txt.NotFound  := nf;
    txt.ReadError := re;
    txt.FileEmpty := em;
    opt.in_dialog := FALSE;
  END AutoSaveRestore;

VAR
  ProgramName: xs.String;
  success: BOOLEAN;
  IP, tmp : kt.ADDRESS;
  access: kt.ATTRIBS;
BEGIN
  Ini;

  IF opt.prog_name = '' THEN
    cfg.LoadConfig('');
    eve.Flush (); -- rise windows after load config
    act.ExecuteAction (act.Load, act.mode_loud);
  ELSE
    fil.ExtractFileName (opt.prog_name, ProgramName);
    fil.RemoveExtension(ProgramName);
    cfg.LoadConfig (ProgramName);
    eve.Flush (); -- rise windows after load config
    IP := kt.NIL_ADDRESS; -- only to kill warning message "possibly used before definition"
    IF dex.Load(opt.prog_name) AND kexe.Loaded THEN
      IP := mem.GetIP ();
      success := mem.GetSegmentInfo (IP, tmp, tmp, access);
    ELSE
      success := FALSE;
    END;
    IF success THEN
      dex.PointExecLine (IP);
      eve.AddToTail (std.Wnds[std.MainWindow].hwnd, eve.Rise, 0);
    ELSE
      crt.Refresh;
      crt.Update;
      eve.AddToTail (dv.MainPulldown, eve.Rise, 0);
    END;
  END;

  eve.MainLoop;

  SaveDebugSessionFile;
END Start;


PROCEDURE StartFromPack(str: ARRAY OF CHAR);

  MODULE AutoSaveRestore;

  IMPORT txt;
  IMPORT opt;
  IMPORT crt;
  IMPORT dlr;

 <* IF DEST_K26 THEN *>
  IMPORT mdl;
 <* END *>

  VAR
    nf, re, em: txt.ErrorProc;
  BEGIN
    opt.in_dialog := TRUE;
    nf := txt.NotFound;
    re := txt.ReadError;
    em := txt.FileEmpty;
    txt.NotFound  := txt.dummyNotFound;
    txt.ReadError := txt.dummyReadError;
    txt.FileEmpty := txt.dummyFileEmpty;
   <* IF DEST_K26 THEN *>
    mdl.Ini;
   <* END *>
    crt.Ini;
    dlr.Ini;
  FINALLY
    dlr.Exi;
    crt.Exi;
   <* IF DEST_K26 THEN *>
    mdl.Exi;
   <* END *>
    txt.NotFound  := nf;
    txt.ReadError := re;
    txt.FileEmpty := em;
    opt.in_dialog := FALSE;
  END AutoSaveRestore;

BEGIN
  IniFromPack;

  IF str # '' THEN
    std.Notify(str);
  END;

  eve.MainLoop;

  SaveDebugSessionFile;

  dmm.ClearRegisters;
END StartFromPack;


BEGIN
END Dlg_Main.
