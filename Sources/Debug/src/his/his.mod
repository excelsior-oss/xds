-- Главный модуль компоненты his
-- Визуализация посмертного стека программы

MODULE His;

<* Storage+ *>

IMPORT sys := SYSTEM;
IMPORT ioc := IOChan;
IMPORT cc  := IOConsts;
IMPORT fio := SeqFile;
IMPORT tio := TextIO;
IMPORT fmt := FormStr;
IMPORT fn  := FileName;
IMPORT arg := ProgEnv;
IMPORT prn := Printf;
IMPORT seq := SeqFile;
IMPORT pe  := ProgExec;

IMPORT xs  := xStr;
IMPORT fil := File;
IMPORT txt := Texts;
IMPORT tls := DI_Tools;
IMPORT bld := DI_Build;
IMPORT dt  := DI_Types;
IMPORT dri := DI_Read;
IMPORT exe := ScanExe;
IMPORT dcd := Decode;

<* IF XDSIDE THEN *>
IMPORT ide := xShell;
<* END *>


VAR
  LogFile     : BOOLEAN;         -- Write log file?
  LogFileName : xs.String;       -- Log file name
  log_file    : ioc.ChanId;      -- Log file
  log_open    : BOOLEAN;         -- Alredy open?


PROCEDURE printf (s-: ARRAY OF CHAR; SEQ arg: sys.BYTE);
VAR
  t: xs.String;
BEGIN
  IF log_open THEN
    fmt.print (t, s, arg);
    tio.WriteString (log_file, t);
  ELSE
    prn.printf (s, arg);
  END;
END printf;

CONST
  PRODUCT   = "XDS History formatter"; (* Продукт   *)
  VERSION   = "2.0";                   (* Версия    *)
  COPYRIGHT = "1996-2001 Excelsior";   (* Copyright *)


PROCEDURE Copyright;
BEGIN
  printf("\n%s, Version %s\n", PRODUCT, VERSION);
  printf("(c) %s\n\n", COPYRIGHT);
END Copyright;


CONST
  err_file_name = "errinfo.$$$"; -- Default error info file name
  his_data_extension = '$$$';
  his_log_extension  = 'log';

VAR
  ErrInfoName: xs.String;     -- Error info file name


PROCEDURE Help;
BEGIN
  printf("Usage:   his [ ('-'|'/') options ] <errinfo file>\n");
  printf('\nOptions:  L[=<filename>]  write log to file\n');
  printf('\t  B[=<filename>]  write XDS debugger control file\n');
  printf('\t  D[=<cmd>]       execute XDS debugger\n');
  printf('\t  S               silent debugger mode\n');
  printf('\t  C               do not write comments\n');
  printf('\t  F               show full path to source file\n');
  HALT (0);
END Help;


VAR
  xdFile     : BOOLEAN;       -- Write XDS debugger control file?
  xdFileName : xs.String;     -- xd file name
  xdText     : txt.TEXT;      -- xd control file text
  xdExec     : BOOLEAN;       -- Execute XDS debugger?
  xdExecCmd  : xs.String;     -- Debugger command line
  xdComment  : BOOLEAN;       -- Write comments?
  xdSilent   : BOOLEAN;       -- Silent debbuger mode?
  FullPath   : BOOLEAN;       -- Full path


PROCEDURE Options (opt-: ARRAY OF CHAR);
BEGIN
  CASE CAP(opt[1]) OF
  | 'L' :
    LogFile := TRUE;
    IF opt[2] = '=' THEN
      xs.Extract(opt, 3, LENGTH(opt), LogFileName);
    ELSIF opt[2] # 0C THEN
      Help;
    END;
  | 'B' :
    xdFile := TRUE;
    IF opt[2] = '=' THEN
      xs.Extract(opt, 3, LENGTH(opt), xdFileName);
    ELSIF opt[2] # 0C THEN
      Help;
    END;
    txt.Close (xdText);
    txt.New (xdText);
  | 'D' :
    IF opt[2] = '=' THEN
      xs.Extract(opt, 3, LENGTH(opt), xdExecCmd);
    ELSIF opt[2] # 0C THEN
      Help;
    END;
    xdExec := TRUE;
    xdFile := TRUE;
    txt.Close (xdText);
    txt.New (xdText);
  | 'S' :
    IF opt[2] # 0C THEN Help; END;
    xdSilent := TRUE;
  | 'F' :
    IF opt[2] # 0C THEN Help; END;
    FullPath := TRUE;
  | 'C' :
    IF opt[2] # 0C THEN Help; END;
    xdComment := FALSE;
  ELSE
    Help;
  END;
END Options;


PROCEDURE ParseCommandLine;
VAR
  k: CARDINAL;
  i: CARDINAL;
  a: xs.String;
BEGIN
  k := arg.ArgNumber();
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
  IF i = k THEN
    COPY(err_file_name, ErrInfoName);
  ELSE
    arg.GetArg(i, ErrInfoName);
  END;
END ParseCommandLine;


PROCEDURE Comment (comment-: ARRAY OF CHAR; SEQ args: sys.BYTE);
VAR
  s: xs.String;
BEGIN
  IF xdComment THEN
    fmt.print (s, comment, args);
    xs.Insert ('; ', 0, s);
    txt.AddLine (xdText, s);
  END;
END Comment;


PROCEDURE Command (command-: ARRAY OF CHAR; SEQ args: sys.BYTE);
VAR
  s: xs.String;
BEGIN
  fmt.print (s, command, args);
  xs.Insert ('  ', 0, s);
  txt.AddLine (xdText, s);
END Command;


PROCEDURE Label (label-: ARRAY OF CHAR);
BEGIN
  txt.AddLine (xdText, label);
END Label;


PROCEDURE PrepareData;
VAR
  res: seq.OpenResults;
BEGIN
  fil.AddExtension (ErrInfoName, his_data_extension);
  IF LogFile THEN
    IF LogFileName = '' THEN
      IF ErrInfoName = '' THEN
        COPY ('his', LogFileName);
      ELSE
        COPY (ErrInfoName, LogFileName);
      END;
      fil.ChangeExtension (LogFileName, his_log_extension);
    ELSE
      fil.AddExtension (LogFileName, his_log_extension);
    END;
    seq.OpenWrite (log_file, LogFileName, seq.write+seq.text+seq.old, res);
    log_open := res = seq.opened;
    IF log_open THEN
      Copyright;
    ELSE
      printf('Can not open log file %s.\n', LogFileName);
      LogFile := FALSE;
    END;
  END;
  IF xdFile THEN
    IF xdFileName = '' THEN
      IF ErrInfoName = '' THEN
        COPY ('his', xdFileName);
      ELSE
        COPY (ErrInfoName, xdFileName);
      END;
      fil.ChangeExtension (xdFileName, 'xd');
    ELSE
      fil.AddExtension (xdFileName, 'xd');
    END;
    txt.SetName (xdText, xdFileName);
    IF xdSilent THEN
      Comment ("Disable executed control file statements are logged\n");
      Command ("MODE BAT-");
    END;
    Comment ("\n; %s, Version %s", PRODUCT, VERSION);
    Comment ("(c) %s", COPYRIGHT);
    Comment ('\n; XDS Debugger control file\n');
  END;
END PrepareData;


PROCEDURE Init;
BEGIN
  LogFile     := FALSE;
  LogFileName := '';
  log_open    := FALSE;
  xdFile      := FALSE;
  xdFileName  := '';
  xdText      := txt.nil;
  xdComment   := TRUE;
  xdSilent    := FALSE;
  xdExec      := FALSE;
  xdExecCmd   := '';
  FullPath    := FALSE;
END Init;


PROCEDURE wsep;
BEGIN
  printf ("-------------------------------------------------------------------------------\n");
END wsep;


TYPE
  TEXT_buf = POINTER TO ARRAY OF CHAR;

  TEXT = RECORD
    name   : TEXT_buf;
    pos    : CARDINAL;
    buffer : TEXT_buf;
    ok     : BOOLEAN;
  END;

VAR
  err_txt : TEXT;

VAR
  cid : ioc.ChanId;

PROCEDURE OpenText(VAR t: TEXT; name-: ARRAY OF CHAR);

CONST
  Flags = fio.text;

VAR
  res : fio.OpenResults;

BEGIN
  fio.OpenRead(cid, name, Flags, res);
  IF res # fio.opened THEN
    t.ok := FALSE;
    printf("Can not read file '%s' \n",name);
  ELSE
    t.ok := TRUE;
  END;
END OpenText;


PROCEDURE truncFileName (VAR s: ARRAY OF CHAR; l: CARDINAL);
VAR
  ln  : CARDINAL;
  len : CARDINAL;
  head: xs.String;
  tail: xs.String;
BEGIN
  IF NOT FullPath THEN
    fil.ExtractFileName (s, tail);
    COPY (tail, s);
  END;
  ln := LENGTH(s);
  IF (l < 12) OR (ln <= l) THEN RETURN; END;
  len := l DIV 2 - 2;
  xs.Extract (s, 0, len, head);
  xs.Extract (s, ln-len-1, ln, tail);
  COPY ('...', s);
  xs.Insert (head, 0, s);
  xs.Append (tail, s);
END truncFileName;


PROCEDURE UnmangleName (VAR name: ARRAY OF CHAR);
VAR
  tmp: xs.String;
BEGIN
  IF dcd.Demangle_Debug (name, tmp) THEN
    COPY (tmp, name);
  END;
EXCEPT
  RETURN;
END UnmangleName;


VAR
  shell: BOOLEAN;


PROCEDURE ReadErrFile();
TYPE
  CHAR_SET = SET OF CHAR;

VAR
  str   : xs.String;
  mname : xs.String;
  proc_name: xs.String;
  comp_name: xs.String;
  module_name: xs.String;
  ext: xs.String;
  mname_ptr : xs.txt_ptr;
  mn: POINTER TO ARRAY [0..255] OF CHAR;
  addr, pos: CARDINAL;
  res: cc.ReadResults;
  comno, modno: CARDINAL;
  save_comno, save_modno: CARDINAL;
  line: CARDINAL;
  save_line: CARDINAL;
  proc: dt.OBJECT;
  Component: dt.COMPONENT;
  Flag, Java: BOOLEAN;
  Break: BOOLEAN;
  RTSmsg: xs.String;

BEGIN
  OpenText(err_txt, ErrInfoName);
  IF err_txt.ok THEN
    Break := FALSE;
    save_comno := dt.Invalid_Component;
    save_modno := dt.Invalid_Module;
    save_line := MAX(CARDINAL);
    tio.ReadString(cid, str);
    tio.SkipLine(cid);
    IF NOT shell THEN
      printf("%s\n", str);
      wsep;
    END;
    Flag := str[0] # '#';
    Java := NOT Flag AND (str[1] = 'J');
    IF xdFile THEN
      Comment ('1. Load the program\n');
      IF Flag THEN
        Command ("LOAD %s\n", str);
      ELSE
        Command ("LOAD <Program>\n");
      END;
    END;
    IF Flag THEN
      tio.ReadString(cid, str);
      tio.SkipLine(cid);
      IF NOT shell THEN
        printf("%s\n", str);
        wsep;
      END;
      Java := str[1] = 'J';
    END;
    COPY(str, RTSmsg);
    IF NOT shell THEN
      IF Java THEN
        printf("Source file                     LINE  OFFSET  METHOD           COMPONENT\n",0);
      ELSE
        printf("Source file                     LINE  OFFSET  PROCEDURE        COMPONENT\n",0);
      END;
      wsep;
    END;
    LOOP
      tio.ReadString(cid, str);
      res := ioc.ReadResult(cid);
      IF (res = cc.allRight) THEN
        tio.SkipLine(cid);
        pos  := 0;
        addr := 0;
        WHILE str[pos] IN CHAR_SET{'0'..'9', 'A'..'F', 'a'..'f'} DO
          IF str[pos] IN CHAR_SET{'0'..'9'} THEN
            addr := addr * 16 + (ORD(str[pos])-ORD('0'));
          ELSE
            addr := addr * 16 + (ORD(CAP(str[pos]))-ORD('A')+10);
          END;
          INC(pos);
        END;
        WHILE str[pos] = ' ' DO INC(pos); END;

        WHILE str[pos] IN CHAR_SET{'0'..'9', 'A'..'F', 'a'..'f'} DO
          INC(pos);
        END;
        WHILE str[pos] = ' ' DO INC(pos); END;

        mn := sys.ADR(str[pos]);

        fn.GetName(mn^, module_name);
        comp_name := module_name;
        xs.Append('$', comp_name);
        fn.GetExt(mn^, ext);
        xs.Append('.', module_name);
        xs.Append(ext, module_name);

        Flag := FALSE;
        IF NOT tls.FindComponentByName(comp_name, comno) THEN
          Component := dt.EmptyComponent;
          WITH Component DO
          (*  Name := bld.AddName( comp_name);  *)
            DI   := dt.EmptyDebugInfo;
            raw  := NIL;
          END;
          bld.AddComponent(Component);
          comno := dt.Components.Count-1;
          dt.Components.Components^[comno].Name:= bld.AddName(comp_name);
          IF exe.OpenExe(mn^, comno, dt.Components.Components^[comno]) THEN
            IF dri.ReadModules(comno, dt.Components.Components^[comno]) = 0 THEN
              Flag := TRUE;
            ELSE
              printf("Unknown debug info format in %s\n", module_name);
            END;
          ELSE
            printf("Can't process file %s\n", module_name);
          END;
        ELSE
          IF dri.CheckDebugInfoVersion (dt.Components.Components^[comno].EI) THEN
            Flag := TRUE;
          ELSE
            printf("Unknown debug info format in %s\n", module_name);
          END;
        END;

        IF NOT tls.FindModInCompByAddr(comno, addr, modno) THEN
          IF (comno # dt.Invalid_Component)  THEN
            printf("Can't recognize module\n");
          END;
        ELSIF Flag THEN
          dri.BuildForMod(modno-1, comno, dt.Components.Components^[comno]);
          IF NOT tls.SourceName(comno, modno, mname) THEN
            IF tls.ModName(comno, modno, mname_ptr) AND (mname_ptr^ # '') THEN
              COPY(mname_ptr^, mname);
            ELSE
              mname := 'UNKNOWN';
            END;
          END;
          IF NOT tls.SourceByAddrInMod(comno, modno, addr, line) THEN
            line := 0;
          END;
          proc := tls.FindProcByAddr(comno, modno, addr);
          IF tls.IsObjectValid (proc) THEN
            tls.ObjectName (proc, proc_name);
            IF Java THEN
              UnmangleName (proc_name);
            END;
          ELSE
            COPY('******', proc_name);
          END;
          IF NOT shell THEN
            truncFileName(mname, 30);
            printf('%-30s %5u  %$6X  %-16s %-s\n', mname, line, addr, proc_name, module_name);
          <* IF XDSIDE THEN *>
          ELSE
            fmt.image (str, pos, '  near procedure "%s" (offset=%8$XH)', proc_name, addr);
            ide.Error(ide.MSG_NOTICE, 0, 1, line, mname, str);
          <* END *>
          END;
          IF xdFile AND NOT Break THEN
            save_comno := comno;
            save_modno := modno;
            save_line := line;
            Comment ('2. Establish breakpoint-counter\n');
            ASSERT(tls.ComName(save_comno, mname_ptr));
            Command ("MODULE %s", mname_ptr^);
            ASSERT(tls.ModName(save_comno, save_modno, mname_ptr));
            Command ("BREAK Counter, LINE, %s, %i, .\n", mname_ptr^, save_line);
            Break := TRUE;
          END;
        END;
      ELSIF (res = cc.endOfInput) THEN fio.Close(cid); EXIT;
      ELSE
        printf("\nWrong format of file '%s'\n", ErrInfoName);
        HALT;
      END;
    END;
    IF xdFile THEN
      IF Break THEN
        Comment ('3. Initiate program execution at the first time\n');
        Command ('START\n');
        Comment ('4. When program has finished, establish delayed sticky breakpoint\n');
        ASSERT(tls.ComName(save_comno, mname_ptr));
        Command ("MODULE %s", mname_ptr^);
        ASSERT(tls.ModName(save_comno, save_modno, mname_ptr));
        Command ("BREAK Break, LINE, %s, %i, Switch_To_Dialog, @PASS(Counter)-1\n", mname_ptr^, save_line);
        Comment ('5. Delete auxiliary breakpoint\n');
        Command ("DEL Counter\n");
        Comment ('6. Initiate program execution for the second time\n');
        Command ('RESTART');
        Command ('START\n');
        Command ('QUIT\n');
        Comment ('7. Upon the breakpoint hit, switch to the dialog mode\n');
        Label ('Switch_To_Dialog');
      ELSE
        Comment ('2. Neither of modules with debug info not found\n');
      END;
      Command ('DIALOG %s', RTSmsg);
      Command ('STOP');
    END;
  END;
END ReadErrFile;


PROCEDURE WriteDebuggerControlFile;
VAR
  xd_file: ioc.ChanId;
  xd_open: BOOLEAN;
  res : seq.OpenResults;
  i, l: CARDINAL;
  line: txt.txt_ptr;
  s   : xs.String;
BEGIN
  l := txt.LastLine(xdText);
  txt.GetName (xdText, line);
  COPY(line^, s);
  seq.OpenWrite (xd_file, s, seq.write+seq.text+seq.old, res);
  xd_open := res = seq.opened;
  IF xd_open THEN
    FOR i := 0 TO l-1 DO
      txt.GetLine (xdText, i, line);
      fmt.print(s, "%s\n", line^);
      tio.WriteString (xd_file, s);
    END;
    seq.Close (xd_file);
  ELSE
    printf("Can not open file '%s'.\n", xdFileName);
  END;
  txt.Close (xdText);
END WriteDebuggerControlFile;


PROCEDURE ExecuteDebbuger;
VAR
  s: xs.String;
  code: CARDINAL;
BEGIN
  IF xdExecCmd = '' THEN
    COPY('xd.exe', xdExecCmd);
    IF arg.StringLength ('XDSDIR') # 0 THEN
      xs.Insert ('\BIN\', 0, xdExecCmd);
      arg.String ('XDSDIR', s);
      xs.Insert (s, 0, xdExecCmd);
    END;
  END;
  printf ("\nExecute debugger %s for %s...\n\n", xdExecCmd, xdFileName);
  fmt.print (s, '/b %s', xdFileName);
  IF pe.Execute (xdExecCmd, s, code) THEN
    IF code # 0 THEN
      printf ("Debugger exeit code - %d\n", code);
    END;
  ELSE
    printf ("Cannot start debugger\n");
  END;
END ExecuteDebbuger;


BEGIN
<* IF XDSIDE THEN *>
  shell := ide.Start();
  IF shell THEN ide.TurnSortingOff END;
<* ELSE *>
  shell := FALSE;
<* END *>

  Copyright;
  Init;
  ParseCommandLine;
  PrepareData;
  ReadErrFile;

  IF xdFile THEN
    WriteDebuggerControlFile;
    IF xdExec THEN
      ExecuteDebbuger;
    END;
  END;
END His.
