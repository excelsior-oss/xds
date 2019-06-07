-- Главный модуль компоненты xstrip
-- Позволяет удалить/восстановить отладочную информацию

MODULE xStrip;

<* Storage+ *>
<* ALIGNMENT = "1" *>

IMPORT sys := SYSTEM;
IMPORT arg := ProgEnv;
IMPORT rf  := RndFile;
IMPORT xfp := xFilePos;
IMPORT ioc := IOChan;
IMPORT xio := XIOChan;
IMPORT cc  := ChanConsts;
IMPORT fs  := FileSys;
IMPORT tc  := TimeConv;
IMPORT fmt := FormStr;

IMPORT xStr;
IMPORT fil := File;

IMPORT Printf;

<* IF env_target = "x86nt" THEN *>

IMPORT win := Windows;
IMPORT wc  := Win32IOChan;

TYPE
 <* PUSH *>
 <* ALIGNMENT = "4" *>
  FILE_TIMES = RECORD
                 result         : BOOLEAN;
                 CreationTime   : win.FILETIME;
                 LastAccessTime : win.FILETIME;
                 LastWriteTime  : win.FILETIME;
               END;
  <* POP *>

VAR
  FileTimes: FILE_TIMES;

PROCEDURE StoreFileTime (file: rf.ChanId);
VAR
  hFile: win.HANDLE;
BEGIN
  hFile := wc.GetSysHandle(file);
  WITH FileTimes DO
    result := win.GetFileTime (hFile, CreationTime, LastAccessTime, LastWriteTime);
    IF NOT result THEN
      Printf.printf("Error %d while getting file time.", win.GetLastError);
    END;
  END;
END StoreFileTime;


PROCEDURE RestoreFileTime (file: rf.ChanId);
VAR
  hFile: win.HANDLE;
BEGIN
  hFile := wc.GetSysHandle(file);
  WITH FileTimes DO
    IF result AND NOT win.SetFileTime (hFile, CreationTime, LastAccessTime, LastWriteTime) THEN
      Printf.printf("Error %d while setting file time.", win.GetLastError);
    END;
  END;
END RestoreFileTime;


<* ELSIF env_target = "x86os2" THEN *>

IMPORT OS2;
IMPORT oc := OS2IOChan;

TYPE
 <* PUSH *>
 <* ALIGNMENT = "4" *>
  FILE_TIMES = RECORD
                 result     : BOOLEAN;
                 FileStatus : OS2.FILESTATUS3;
               END;
 <* POP *>

VAR
  FileTimes: FILE_TIMES;

PROCEDURE StoreFileTime (file: rf.ChanId);
VAR
  hFile: OS2.HFILE;
  rc: CARDINAL;
BEGIN
  hFile := oc.GetSysHandle(file);
  WITH FileTimes DO
    rc := OS2.DosQueryFileInfo (hFile, OS2.FIL_STANDARD, sys.ADR(FileStatus), SIZE(FileStatus));
    result := rc = 0;
    IF NOT result THEN
      Printf.printf("Error %d while getting file time.", rc);
    END;
  END;
END StoreFileTime;


PROCEDURE RestoreFileTime (file: rf.ChanId);
VAR
  hFile: OS2.HFILE;
  rc: CARDINAL;
BEGIN
  hFile := oc.GetSysHandle(file);
  WITH FileTimes DO
    IF result THEN
      rc := OS2.DosSetFileInfo (hFile, OS2.FIL_STANDARD, sys.ADR(FileStatus), SIZE(FileStatus));
      IF rc # 0 THEN
        Printf.printf("Error %d while setting file time.", rc);
      END;
    END;
  END;
END RestoreFileTime;

<* END *>


TYPE
  DI_ENTRY = RECORD
               name: ARRAY [0..3] OF CHAR;
               offs: CARDINAL;
             END;

VAR
  strip   : BOOLEAN;
  modify  : BOOLEAN;
  quiet   : BOOLEAN;
  save    : BOOLEAN;
  exe_name: xStr.String;
  exe_size: CARDINAL;
  hdr_pos : CARDINAL;
  hdr_addr: CARDINAL;
  hdr_size: CARDINAL;
  di_name : xStr.String;
  di_type : xStr.String;
  di_size : CARDINAL;
  di_exe  : xStr.String;


PROCEDURE printf (s: ARRAY OF CHAR; SEQ arg: sys.BYTE);
BEGIN
  IF NOT quiet THEN
    Printf.printf (s, arg);
  END;
END printf;


CONST
  IDENTKEY  = "XDS Debug Info Stripper";
  VERSION   = "1.0";
  COPYRIGHT = "1997-2001 Excelsior";


PROCEDURE Copyright;
BEGIN
  printf("\n%s, Version %s\n(c) %s\n\n", IDENTKEY, VERSION, COPYRIGHT);
END Copyright;


PROCEDURE Help;
BEGIN
  printf("Usage:   xstrip [ ('-'|'/') options ] [ program ]\n\n");
  printf('Options: s [=<debug_info>] strip debug info to file\n');
  printf('         a [=<debug_info>] add debug info to program\n');
  printf("         n                 do not save debug info\n");
  printf("         t                 preserve timestamp\n");
  printf("         q                 quiet mode\n\n");
  printf('         <debug_info> is name of file contains debug info\n\n');
  printf("Default: strip debug info  - on\n");
  printf("         program extension - '.exe'\n");
  printf("         debug info name   - program'.xst'\n");
  printf("         \n");
  HALT (0);
END Help;


PROCEDURE Options (opt-: ARRAY OF CHAR);
BEGIN
  CASE CAP(opt[1]) OF
  | 'A':
    strip := FALSE;
    IF opt[2] = '=' THEN
      xStr.Extract (opt, 3, LENGTH(opt), di_name);
    ELSIF opt[2] # 0C THEN
      Help;
    END;
  | 'S':
    strip := TRUE;
    IF opt[2] = '=' THEN
      xStr.Extract (opt, 3, LENGTH(opt), di_name);
    ELSIF opt[2] # 0C THEN
      Help;
    END;
  | 'T':
    modify := FALSE;
  | 'Q':
    quiet := TRUE;
  | 'N':
    save := FALSE;
  ELSE
    Help;
  END;
END Options;


PROCEDURE ParseCommandLine;
VAR
  k, i: CARDINAL;
  a: xStr.String;
BEGIN
  k := arg.ArgNumber();
  IF k = 0 THEN Help; END;
  i := 0;
  LOOP
    IF (i = k) THEN EXIT; END;
    arg.GetArg(i, a);
    IF (a[0] = '/') OR (a[0] = '-') THEN
      Options(a);
    ELSE
      EXIT;
    END;
    INC(i);
  END;
  IF strip AND (i = k) THEN Help; END;
  IF i < k THEN
    arg.GetArg (i, exe_name);
    fil.AddExtension (exe_name, 'exe');
  END;
  IF di_name = '' THEN
    IF exe_name = '' THEN Help; END;
    COPY(exe_name, di_name);
    fil.RemoveExtension (di_name);
  END;
  fil.AddExtension (di_name, 'xst');
END ParseCommandLine;


PROCEDURE Init;
BEGIN
  strip    := TRUE;
  modify   := TRUE;
  quiet    := FALSE;
  save     := TRUE;
  exe_name := '';
  di_name  := '';
  hdr_pos  := 0;
  hdr_addr := 0;
  hdr_size := 0;
END Init;


CONST
  e_ExecutableFileNotOpened                    = 1;
  e_FileTooLong                                = 2;
  e_FileStructureFault                         = 3;
  e_FileReadError                              = 4;
  e_FileWriteError                             = 5;
  e_FileAccessDenied                           = 6;
  e_FileNotFound                               = 7;
  e_IsNot_XDS_StripFile                        = 8;
  e_ExecutableSizeMismatch                     = 9;

  w_DebugInfoNotDetected                       = 1000;
  w_FileStructureFaultOrNotDebugInfo           = 1001;
  w_ExecutableNamesMismatch                    = 1002;
  w_ExecutableSizeMismatch                     = 1003;


PROCEDURE Error (error_code: CARDINAL);
VAR
  s: xStr.String;
BEGIN
  CASE error_code OF
  | e_ExecutableFileNotOpened:
    COPY('Executable file not opened', s);
  | e_FileTooLong:
    COPY('Executable file too long', s);
  | e_FileStructureFault:
    COPY('File structure fault', s);
  | e_FileReadError:
    COPY('File read error', s);
  | e_FileWriteError:
    COPY('File write error', s);
  | e_FileAccessDenied:
    COPY('File access denied', s);
  | e_FileNotFound:
    COPY('File not found', s);
  | e_IsNot_XDS_StripFile:
    COPY('File is not XDS strip file', s);
  | e_ExecutableSizeMismatch:
    COPY('Executable size mismatch', s);
  ELSE
    printf(s, 'n.%u', error_code);
  END;
  printf('Error: %s.\n', s);
  HALT (error_code);
END Error;


PROCEDURE Warning (warning_code: CARDINAL);
VAR
  s: xStr.String;
BEGIN
  CASE warning_code OF
  | w_DebugInfoNotDetected:
    COPY('Debug info not detected', s);
  | w_FileStructureFaultOrNotDebugInfo:
    COPY('File structure fault or not debug info', s);
  | w_ExecutableNamesMismatch:
    COPY('Executable names mismatch', s);
  | w_ExecutableSizeMismatch:
    COPY('Executable sizes mismatch', s);
  ELSE
    printf(s, 'n.%u', warning_code);
  END;
  printf('Warning: %s.\n', s);
END Warning;



PROCEDURE GetDebugInfo (): BOOLEAN;
VAR
  file : rf.ChanId;
  res  : rf.OpenResults;
  ps   : CARDINAL;
  fp   : xfp.FilePos;
  ent  : DI_ENTRY;

  PROCEDURE read (VAR buf: ARRAY OF sys.LOC): BOOLEAN;
  VAR
    x: CARDINAL;
  BEGIN
    x:=SIZE(buf);
    ioc.RawRead(file, sys.ADR(buf), x, x);
    RETURN x=SIZE(buf);
  END read;

BEGIN
  rf.OpenOld (file, exe_name, rf.raw+rf.read, res);
  IF res # rf.opened THEN Error(e_FileNotFound); END;

  IF NOT modify THEN StoreFileTime (file); END;

  fp := rf.EndPos(file);
  IF NOT xfp.PosToCard(ps, fp) THEN Error(e_FileTooLong); END;
  IF ps <= 8 THEN Error(e_FileStructureFault);  END;
  xfp.CardToPos(fp, ps-8);
  rf.SetPos(file, fp);
  IF NOT read(ent) THEN Error(e_FileReadError); END;

  IF ent.offs > ps THEN
    Warning(w_FileStructureFaultOrNotDebugInfo);
    RETURN FALSE;
  END;

  COPY(exe_name, di_exe);
  COPY(ent.name, di_type);
  di_size := ent.offs;
  exe_size := ps-ent.offs;
  xfp.CardToPos (fp, exe_size);
  rf.SetPos (file, fp);
  IF NOT read(ent) THEN
    Warning(w_FileStructureFaultOrNotDebugInfo);
    RETURN FALSE;
  END;

  IF ent.name # di_type THEN
    Warning(w_FileStructureFaultOrNotDebugInfo);
    RETURN FALSE;
  END;

  rf.Close(file);
  RETURN TRUE;
END GetDebugInfo;


<* IF env_target = "x86nt" THEN *>

PROCEDURE GetHeaderDebugInfo (full_name-: ARRAY OF CHAR);
TYPE
  A = ARRAY [0..15] OF CARDINAL;

VAR
  hMapFile      : win.HANDLE;
  lpFile        : win.PVOID;
  plpFile       : POINTER TO win.USHORT;
  pfh           : win.PIMAGE_FILE_HEADER;
  poh           : win.PIMAGE_OPTIONAL_HEADER;
  hfile         : win.HFILE;
  ReOpenBuff    : win.OFSTRUCT;
  nSections     : CARDINAL;
  ImageHdrOffset: CARDINAL;
  pA            : POINTER TO A;
  pCARDINAL     : POINTER TO CARDINAL;

  PROCEDURE CloseHandles;
  BEGIN
    IF hfile # 0 THEN win._lclose (hfile); END;
    IF lpFile # NIL THEN win.UnmapViewOfFile (lpFile); END;
    IF hMapFile # NIL THEN win.CloseHandle (hMapFile); END;
  END CloseHandles;

  MODULE Exit; IMPORT CloseHandles; BEGIN FINALLY CloseHandles; END Exit;

BEGIN
  hdr_pos := 0;
  hdr_addr := 0;
  hdr_size := 0;
  hMapFile := NIL;
  lpFile := NIL;
  hfile := win.OpenFile (full_name, ReOpenBuff, win.OF_READ);
  IF ReOpenBuff.nErrCode # 0 THEN RETURN; END;
  hMapFile := win.CreateFileMapping (win.HANDLE(hfile), NIL, win.PAGE_READONLY, 0, 0, NIL);
  lpFile := win.MapViewOfFile (hMapFile, win.FILE_MAP_READ, 0, 0, 0);
  plpFile := sys.ADDRESS(lpFile);
  IF plpFile^ # win.IMAGE_DOS_SIGNATURE THEN RETURN; END;
  pA := sys.ADDRESS(lpFile);
  ImageHdrOffset := pA^[15]+SIZE(CARDINAL);
  pCARDINAL := sys.ADDADR(lpFile, ImageHdrOffset-SIZE(CARDINAL));
  IF pCARDINAL^ # win.IMAGE_NT_SIGNATURE THEN RETURN; END;
  pfh := win.PIMAGE_FILE_HEADER(sys.ADDADR(lpFile, ImageHdrOffset));
  IF pfh^.SizeOfOptionalHeader = 0 THEN RETURN; END;
  nSections := pfh^.NumberOfSections;
  IF nSections = 0 THEN RETURN; END;
  poh := win.PIMAGE_OPTIONAL_HEADER(sys.ADDADR(pfh, SIZE(win.IMAGE_FILE_HEADER)));
  hdr_pos := sys.DIFADR(sys.ADR(poh^.DataDirectory[win.IMAGE_DIRECTORY_ENTRY_DEBUG]), lpFile);
  hdr_addr := poh^.DataDirectory[win.IMAGE_DIRECTORY_ENTRY_DEBUG].VirtualAddress;
  hdr_size := poh^.DataDirectory[win.IMAGE_DIRECTORY_ENTRY_DEBUG].Size;
EXCEPT
  RETURN;
END GetHeaderDebugInfo;

<* ELSIF env_target = "x86os2" THEN *>

PROCEDURE GetHeaderDebugInfo (full_name-: ARRAY OF CHAR);
TYPE
  LX_HEADER = RECORD
    signature: ARRAY [0..1] OF CHAR;
    border     : sys.CARD8;      (* byte ordering                             *)
    worder     : sys.CARD8;      (* word ordering                             *)
    lefl       : sys.CARD32;     (* Linear EXE Format Level                   *)
    cpu        : sys.CARD16;     (* Module CPU type                           *)
    ostype     : sys.CARD16;     (* Module OS type                            *)
    mver       : sys.CARD32;     (* Version of LX module                      *)
    mflags     : BITSET;         (* Flag bits for the module                  *)
    mpages     : sys.CARD32;     (* Number of pages in module                 *)
    eipobj     : sys.CARD32;     (* The Object number to which the Entry      *)
                                 (* Address is relative                       *)
    eip        : sys.CARD32;     (*  Entry address of module                  *)
    espobj     : sys.CARD32;     (* The Object number to  which the ESP is    *)
                                 (*  relative                                 *)
    esp        : sys.CARD32;     (* Starting stack address of module          *)
    psize      : sys.CARD32;     (* The size of one page for this system      *)
    poffshift  : sys.CARD32;     (* The shift left  bits  for  page offsets   *)
    fupssize   : sys.CARD32;     (* Total  size  of  the  fixup information   *)
                                 (* in bytes                                  *)
    fupschsum  : sys.CARD32;     (* Checksum for fixup information            *)
    lderssize  : sys.CARD32;     (* Size  of  memory  resident tables         *)
    lderchsum  : sys.CARD32;     (* Checksum   for  loader  section           *)
    objtoffs   : sys.CARD32;     (* Object Table offset                       *)
    objinmod   : sys.CARD32;     (* Obect Table Count                        *)
    objptoffs  : sys.CARD32;     (* Object Page Table offset                  *)
    objipoff   : sys.CARD32;     (* Object  Iterated  Pages  offset           *)
    restoffs   : sys.CARD32;     (* Resource Table offset                     *)
    restentr   : sys.CARD32;     (* Number  of entries  in Resource Table     *)
    resntoffs  : sys.CARD32;     (* Resident Name Table offset                *)
    enttoffs   : sys.CARD32;     (* Entry Table offset                        *)
    moddoffs   : sys.CARD32;     (* Module  Format Directives Table offset    *)
    moddirvs   : sys.CARD32;     (* Number of Module Format Directives in     *)
                                 (* the Table                                 *)
    fupptoffs  : sys.CARD32;     (* Fixup Page Table offset                   *)
    fuptroffs  : sys.CARD32;     (* Fixup Record Table Offset                 *)
    impmodoffs : sys.CARD32;     (* Import  Module Name Table offset          *)
    impmodentr : sys.CARD32;     (* The number of entries in the Import       *)
                                 (* Module Name Table                         *)
    impprtoffs : sys.CARD32;     (* Import Procedure  Name Table offset       *)
    ppchkoffs  : sys.CARD32;     (* Per-Page  Checksum  Table offset          *)
    dpoffs     : sys.CARD32;     (* Data Pages Offset                         *)
    numprelp   : sys.CARD32;     (* Number of Preload pages for this module   *)
    nonrestoffs: sys.CARD32;     (* Non-Resident  Name  Table offset          *)
    nonrestlen : sys.CARD32;     (* Number of bytes in the Non-resident       *)
                                 (* name table                                *)
    nrestchksum: sys.CARD32;     (* Non-Resident  Name  Table Checksum        *)
    autodsobj  : sys.CARD32;     (* The Auto Data  Segment Object number      *)
    debinfoffs : sys.CARD32;     (* Debug Information offset                  *)
    debinflen  : sys.CARD32;     (* Debug Information length                  *)
    ninstpp    : sys.CARD32;     (* Instance  pages  in  preload section      *)
    ninstdem   : sys.CARD32;     (* Instance  pages  in  demand section       *)
    heapsize   : sys.CARD32;     (* Heap size added to the Auto DS Object     *)
  END;


VAR
  file: rf.ChanId;
  res : rf.OpenResults;
  hdr : LX_HEADER;

  PROCEDURE fseek(pos: CARDINAL);
  VAR
    fp: rf.FilePos;
  BEGIN
    xfp.CardToPos(fp, pos);
    rf.SetPos(file, fp);
  END fseek;

  PROCEDURE read(VAR buf: ARRAY OF sys.LOC): BOOLEAN;
    VAR x: CARDINAL;
  BEGIN
    x:=SIZE(buf);
    ioc.RawRead(file, sys.ADR(buf), x, x);
    RETURN x=SIZE(buf);
  END read;

BEGIN
  hdr_pos := 0;
  hdr_addr := 0;
  hdr_size := 0;

  rf.OpenOld (file, full_name, rf.raw, res);
  IF res # rf.opened THEN RETURN; END;

  fseek(03CH);
  IF NOT read(hdr_pos) THEN RETURN; END;
  fseek(hdr_pos);
  IF NOT read(hdr) THEN hdr_pos := 0; RETURN; END;
  IF hdr.signature # 'LX' THEN RETURN; END;

  INC(hdr_pos, sys.DIFADR(sys.ADR(hdr.debinfoffs),sys.ADR(hdr)));
  hdr_addr := hdr.debinfoffs;
  hdr_size := hdr.debinflen;
EXCEPT
  RETURN;
END GetHeaderDebugInfo;

<* END *>

PROCEDURE WriteDebugInfo;
VAR
  fexe: ioc.ChanId;
  f_di: ioc.ChanId;
  res : cc.OpenResults;
  len : CARDINAL;
  read: CARDINAL;
  fp  : xfp.FilePos;
  buf : ARRAY [1..0FFFFH] OF sys.CARD8;
  done: BOOLEAN;
  key : xStr.String;

BEGIN
  GetHeaderDebugInfo (di_exe);

  IF save THEN
    rf.OpenClean (f_di, di_name, rf.raw+rf.write+rf.old, res);
    IF res # cc.opened THEN Error(e_FileWriteError); END;

    fmt.print(key, '%s, %s', IDENTKEY, VERSION);
    len := LENGTH(key)+1;
    ioc.RawWrite (f_di, sys.ADR(key), len);

    len := LENGTH(di_exe)+1;
    ioc.RawWrite (f_di, sys.ADR(len), SIZE(len));
    ioc.RawWrite (f_di, sys.ADR(di_exe), len);
    ioc.RawWrite (f_di, sys.ADR(exe_size), SIZE(exe_size));
    ioc.RawWrite (f_di, sys.ADR(hdr_pos), SIZE(hdr_pos));
    ioc.RawWrite (f_di, sys.ADR(hdr_addr), SIZE(hdr_addr));
    ioc.RawWrite (f_di, sys.ADR(hdr_size), SIZE(hdr_size));
    ioc.RawWrite (f_di, sys.ADR(di_size), SIZE(di_size));

    rf.OpenOld (fexe, di_exe, rf.raw+rf.read, res);
    IF res # rf.opened THEN Error(e_FileNotFound); END;
    xfp.CardToPos(fp, exe_size);
    rf.SetPos(fexe, fp);

    len := SIZE(buf);
    REPEAT
      IF len > di_size THEN len := di_size; END;
      ioc.RawRead(fexe, sys.ADR(buf), len, read);
      IF len # read THEN
        rf.Close(f_di);
        rf.Close(fexe);
        fs.Remove (di_name, done);
        Error(e_FileReadError);
      END;
      ioc.RawWrite (f_di, sys.ADR(buf), len);
      DEC(di_size, len);
    UNTIL di_size = 0;

    rf.Close(f_di);
    rf.Close(fexe);
  END;

  rf.OpenOld (fexe, di_exe, rf.raw+rf.write, res);
  IF res # rf.opened THEN Error(e_FileAccessDenied); END;
  xfp.CardToPos(fp, exe_size);
  rf.SetPos(fexe, fp);
  xio.Truncate(fexe);
  IF hdr_pos # 0 THEN
    xfp.CardToPos(fp, hdr_pos);
    rf.SetPos(fexe, fp);
    read := 0;
    ioc.RawWrite (fexe, sys.ADR(read), SIZE(read));
    ioc.RawWrite (fexe, sys.ADR(read), SIZE(read));
  END;

  IF NOT modify THEN RestoreFileTime (fexe); END;

  rf.Close(fexe);
END WriteDebugInfo;


PROCEDURE ReadDebugInfo (): BOOLEAN;
VAR
  fexe: ioc.ChanId;
  f_di: ioc.ChanId;
  res : cc.OpenResults;
  len : CARDINAL;
  read: CARDINAL;
  size: CARDINAL;
  fp  : xfp.FilePos;
  ent : DI_ENTRY;
  str : xStr.String;
  key : xStr.String;

BEGIN
  rf.OpenOld (f_di, di_name, rf.raw+rf.read, res);
  IF res # cc.opened THEN Error(e_FileNotFound); END;

  fmt.print(key, '%s, %s', IDENTKEY, VERSION);
  len := LENGTH(key)+1;
  ioc.RawRead (f_di, sys.ADR(str), len, read);
  IF len # read THEN Error(e_FileReadError); END;
  IF key # str THEN Error(e_IsNot_XDS_StripFile); END;

  len := 0;
  ioc.RawRead (f_di, sys.ADR(len), SIZE(len), read);
  IF SIZE(len) # read THEN Error(e_FileReadError); END;
  ioc.RawRead (f_di, sys.ADR(di_exe), len, read);
  IF len # read THEN Error(e_FileReadError); END;
  IF LENGTH(di_exe) # len-1 THEN Error(e_FileStructureFault); END;
  IF exe_name = '' THEN
    COPY(di_exe, exe_name);
  ELSIF exe_name # di_exe THEN
    Warning(w_ExecutableNamesMismatch);
  END;

  rf.OpenOld (fexe, exe_name, rf.raw, res);
  IF res # rf.opened THEN Error(e_FileNotFound); END;

  IF NOT modify THEN StoreFileTime (fexe); END;

  fp := rf.EndPos(fexe);
  IF NOT xfp.PosToCard(exe_size, fp) THEN Error(e_FileTooLong); END;
  rf.Close(fexe);

  size := 0;
  ioc.RawRead (f_di, sys.ADR(size), SIZE(size), read);
  IF SIZE(size) # read THEN Error(e_FileReadError); END;
  IF exe_size # size THEN Error(e_ExecutableSizeMismatch); END;

  ioc.RawRead (f_di, sys.ADR(hdr_pos), SIZE(hdr_pos), read);
  IF SIZE(hdr_pos) # read THEN Error(e_FileReadError); END;

  ioc.RawRead (f_di, sys.ADR(hdr_addr), SIZE(hdr_addr), read);
  IF SIZE(hdr_addr) # read THEN Error(e_FileReadError); END;

  ioc.RawRead (f_di, sys.ADR(hdr_size), SIZE(hdr_size), read);
  IF SIZE(hdr_size) # read THEN Error(e_FileReadError); END;

  ioc.RawRead (f_di, sys.ADR(di_size), SIZE(di_size), read);
  IF SIZE(di_size) # read THEN Error(e_FileReadError); END;

  INC(len, LENGTH(key)+1);
  INC(len, 4+4+4+4+4+4+di_size);
  fp := rf.EndPos(f_di);
  IF NOT xfp.PosToCard(size, fp) THEN Error(e_FileTooLong); END;
  IF len # size THEN Error(e_FileStructureFault); END;

  ioc.RawRead (f_di, sys.ADR(ent), SIZE(ent), read);
  IF SIZE(ent) # read THEN Error(e_FileReadError); END;
  COPY(ent.name, di_type);

  rf.Close(f_di);
  RETURN TRUE;
END ReadDebugInfo;


PROCEDURE PutDebugInfo;
VAR
  fexe: ioc.ChanId;
  f_di: ioc.ChanId;
  res : cc.OpenResults;
  len : CARDINAL;
  read: CARDINAL;
  fp  : xfp.FilePos;
  buf : ARRAY [1..0FFFFH] OF sys.CARD8;
  key : xStr.String;

BEGIN
  rf.OpenOld (f_di, di_name, rf.raw+rf.read, res);
  IF res # cc.opened THEN Error(e_FileNotFound); END;

  fmt.print(key, '%s, %s', IDENTKEY, VERSION);
  len := LENGTH(key)+1;
  xfp.CardToPos(fp, len+4+LENGTH(di_name)+1+4+4+4+4+4);
  rf.SetPos(f_di, fp);

  rf.OpenOld (fexe, exe_name, rf.raw+rf.write, res);
  IF res # rf.opened THEN Error(e_FileNotFound); END;
  xfp.CardToPos(fp, exe_size);
  rf.SetPos(fexe, fp);

  len := SIZE(buf);
  REPEAT
    IF len > di_size THEN len := di_size; END;
    ioc.RawRead(f_di, sys.ADR(buf), len, read);
    IF len # read THEN
      rf.Close(f_di);
      rf.SetPos(fexe, fp);
      xio.Truncate(fexe);
      rf.Close(fexe);
      Error(e_FileStructureFault);
    END;
    ioc.RawWrite (fexe, sys.ADR(buf), len);
    DEC(di_size, len);
  UNTIL di_size = 0;

  rf.Close(f_di);

  IF hdr_pos # 0 THEN
    xfp.CardToPos(fp, hdr_pos);
    rf.SetPos(fexe, fp);
    ioc.RawWrite (fexe, sys.ADR(hdr_addr), SIZE(hdr_addr));
    ioc.RawWrite (fexe, sys.ADR(hdr_size), SIZE(hdr_size));
  END;

 <* IF env_target = "x86os2" THEN *>
  fp := rf.EndPos(fexe);
  rf.SetPos(fexe, fp);
  xio.Truncate(fexe);
 <* END *>

  IF NOT modify THEN RestoreFileTime (fexe); END;

  rf.Close(fexe);
END PutDebugInfo;


PROCEDURE DoAction;
BEGIN
  IF strip THEN
    IF GetDebugInfo() THEN
      printf('\nAction            strip debug info\n');
      printf('\nExecutable name   %s\n', exe_name);
      printf('           size   %u (0x%X)\n', exe_size, exe_size);
      printf('\nDebug info name   %s\n', di_name);
      printf('           type   %s\n', di_type);
      printf('           size   %u (0x%X)\n', di_size, di_size);
      WriteDebugInfo;
    END;
  ELSE
    IF ReadDebugInfo() THEN
      printf('\nAction            add debug info\n');
      printf('\nDebug info name   %s\n', di_name);
      printf('           from   %s\n', di_exe);
      printf('           type   %s\n', di_type);
      printf('           size   %u (0x%X)\n', di_size, di_size);
      printf('\nExecutable name   %s\n', exe_name);
      PutDebugInfo;
    END;
  END;
EXCEPT
  Error(e_FileStructureFault);
END DoAction;


BEGIN
  Copyright;
  Init;
  ParseCommandLine;
  DoAction;
END xStrip.
