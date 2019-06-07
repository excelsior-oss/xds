MODULE H2DMsg;

(*  ----------------------------------------------------------------------  *)

IMPORT
  adt,
  clock:= SysClock,
  conv := TimeConv,
  sys  := SYSTEM,
  io   := Printf,
  lstr := LongStrs,
  file := H2DFile,
  WholeStr,
  ConvTypes;

(*  ----------------------------------------------------------------------  *)

CONST
  comment_char * = '%';
  version * = " v1.31.0 ";

CONST
  all_files_exist = 150;

CONST
  progress_delay = 1;
  msgs_num = 200;
  no_messages = 'Message %d (message text is not available)';
  mergeMarker = '*';
  shift = LENGTH(mergeMarker) + 2;

(*  ----------------------------------------------------------------------  *)

TYPE
 INT = sys.INT32;

 Statistics * = POINTER TO StatisticsDesc;
 StatisticsDesc * = RECORD (adt.NamedElementDesc)
   lines  * : INT;
   errors   : INT;
   warnings : INT;
   messages : adt.List; (* of adt.NamedElement *)
 END;

(*  ----------------------------------------------------------------------  *)

VAR
  WasError * : BOOLEAN; 	      (* error flag for all modules *)
  progress * : BOOLEAN;

  Line * , Pos * : INT;
  File * : lstr.String;


  global_lines_num: INT;
  global_files_num: INT;
  global_errors_num: INT;
  global_warnings_num: INT;

  msgs: ARRAY msgs_num OF lstr.String;

  begin_time: clock.DateTime;
  progress_time: clock.DateTime;
  progress_line_len: INT;

TYPE
  Level = POINTER TO LevelDesc;
  LevelDesc = RECORD(adt.ElementDesc)
    includeLevel, mergeLevel: INT;
  END;

VAR
  logFile: file.FILE;
  logFileExist: BOOLEAN;
  levelStack: adt.Stack;
  currLevel: Level;

  dirFile: file.FILE;
  dirFileExist: BOOLEAN;

(*  ----------------------------------------------------------------------  *)

PROCEDURE Copyright * ();
BEGIN
  io.printf( "H2D%s(c) 1996-1997 xTech Ltd.\n", version );
END Copyright;

(*  ----------------------------------------------------------------------  *)

PROCEDURE Help * ();
BEGIN
  io.printf( "Usage:\n");
  io.printf( "  Make project:\n");
  io.printf( "    H2D =p  FILENAME\n");
  io.printf( "  Translate:\n");
  io.printf( "    H2D { FILENAME }\n");
  HALT;
END Help;

(*  ----------------------------------------------------------------------  *)

PROCEDURE NewStatistics * (VAR s: Statistics; name-: ARRAY OF CHAR);
BEGIN
  NEW(s); s.SetName(name);
  s.lines    := 0;
  s.errors   := 0;
  s.warnings := 0;
  adt.NewList(s.messages);
END NewStatistics;

(*  ----------------------------------------------------------------------  *)

PROCEDURE (d: Statistics) ShowHeader * ();
VAR str: lstr.String;
BEGIN
  IF d.name^ # '' THEN
    IF progress THEN
      str:= NIL;
      lstr.Fill(str, ' ', progress_line_len);
      io.printf('%s\r', str^);
    END;
    io.printf('File %s\n', d.name^);
  END;
END ShowHeader;

(*  ----------------------------------------------------------------------  *)

PROCEDURE (d: Statistics) ShowFooter * ();
VAR e: adt.Element;
    str: lstr.String;
BEGIN
  IF progress THEN
    str:= NIL;
    lstr.Fill(str, ' ', progress_line_len);
    io.printf('%s\r', str^);
  END;
  d.messages.FindFirst(e);
  WHILE e # NIL DO
    io.printf('%s\n', e(adt.NamedElement).name^);
    d.messages.FindNext(e);
  END;
  IF d.name^ # '' THEN
    io.printf('errors %d, warnings %d, lines %d.\n', d.errors, d.warnings, d.lines);
    INC(global_files_num);
    global_lines_num   := global_lines_num    + d.lines;
    global_warnings_num:= global_warnings_num + d.warnings;
  END;
  global_errors_num  := global_errors_num   + d.errors;
END ShowFooter;

(*  ----------------------------------------------------------------------  *)

PROCEDURE (d: Statistics) Error * (number: INT; name-: ARRAY OF CHAR;
                                   line, pos: INT; SEQ x: sys.BYTE);
VAR buf: ARRAY 512 OF CHAR;
    msg: ARRAY 512 OF CHAR;
    ne: adt.NamedElement;
BEGIN
  WasError := TRUE;
  INC(d.errors);
  IF msgs[number] # NIL THEN
    io.sprintf(msg, msgs[number]^, x);
  ELSE
    io.sprintf(msg, no_messages, number);
  END;
  io.sprintf(buf, 'Error [ %s %d:%d ] ** %s', name, line, pos, msg);
  NEW(ne); ne.SetName(buf);
  d.messages.Insert(ne);
END Error;

(*  ----------------------------------------------------------------------  *)

PROCEDURE (d: Statistics) Warning * (number: INT; name-: ARRAY OF CHAR;
                                     line, pos: INT; SEQ x: sys.BYTE);
VAR buf: ARRAY 512 OF CHAR;
    msg: ARRAY 512 OF CHAR;
    ne: adt.NamedElement;
BEGIN
  INC(d.warnings);
  IF msgs[number] # NIL THEN
    io.sprintf(msg, msgs[number]^, x);
  ELSE
    io.sprintf(msg, no_messages, number);
  END;
  io.sprintf(buf, 'Warning [ %s %d:%d ] ** %s', name, line, pos, msg);
  NEW(ne); ne.SetName(buf);
  d.messages.Insert(ne);
END Warning;

(*  ----------------------------------------------------------------------  *)

PROCEDURE GlobalStatistics * ();
VAR curTime: clock.DateTime;
    total_time: INT;
BEGIN
  clock.GetClock( curTime );
  total_time:= sys.VAL(INT, conv.SubDateSecs(curTime, begin_time));
  IF (global_files_num = 0) & (global_errors_num = 0) THEN
    IF msgs[all_files_exist] # NIL THEN
      io.printf('%s', msgs[all_files_exist]^);
    ELSE
      io.printf(no_messages, all_files_exist);
    END;
  ELSE
    io.printf('-----------------------------------------------------\n');
    io.printf('\rFiles %d, lines %d, ', global_files_num, global_lines_num);
    IF global_errors_num = 0 THEN
      io.printf('no errors, ');
    ELSE
      io.printf('errors %d, ', global_errors_num);
    END;
    IF global_warnings_num = 0 THEN
      io.printf('no warnings, ');
    ELSE
      io.printf('warnings %d, ', global_warnings_num);
    END;
    IF (total_time DIV 3600) > 0 THEN
      io.printf('time: %d:%d:%d.\n', total_time DIV 3600, (total_time MOD 3600) DIV 60, (total_time MOD 3600) MOD 60);
    ELSE
      io.printf('time %d:%d.\n', total_time DIV 60, total_time MOD 60);
    END;
  END;
END GlobalStatistics;

(*  ----------------------------------------------------------------------  *)

PROCEDURE Error * ( number: INT; SEQ x: sys.BYTE );
BEGIN
  WasError := TRUE;
  IF File = NIL THEN
    io.printf('Error [ ] ** ');
    IF msgs[number] # NIL THEN
      io.printf(msgs[number]^, x);
    ELSE
      io.printf(no_messages, number);
    END;
    io.printf('\n');
  ELSE
    io.printf('Error [ %s %d:%d ] ** ', File^, Line, Pos);
    IF msgs[number] # NIL THEN
      io.printf(msgs[number]^, x);
    ELSE
      io.printf(no_messages, number);
    END;
    io.printf('\n');
  END;
END Error;

(*  ----------------------------------------------------------------------  *)

PROCEDURE Warning * (number: INT; SEQ x: sys.BYTE);
BEGIN
  IF File = NIL THEN
    io.printf('Warning [ ] ** ');
    IF msgs[number] # NIL THEN
      io.printf(msgs[number]^, x);
    ELSE
      io.printf(no_messages, number);
    END;
    io.printf('\n');
  ELSE
    io.printf('Warning [ %s %d:%d ] ** ', File^, Line, Pos);
    IF msgs[number] # NIL THEN
      io.printf(msgs[number]^, x);
    ELSE
      io.printf(no_messages, number);
    END;
    io.printf('\n');
  END;
END Warning;

(*  ----------------------------------------------------------------------  *)

PROCEDURE Progress * (name-: ARRAY OF CHAR; line: INT);
VAR curTime: clock.DateTime;
    buf: ARRAY 200 OF CHAR;
    str: lstr.String;
    len: INT;
BEGIN
  IF progress THEN
    clock.GetClock(curTime);
    IF conv.SubDateSecs(curTime, progress_time) > progress_delay THEN
      str:= NIL;
      io.sprintf(buf, 'File %s, line %d', name, line);
      len:= lstr.Length(buf);
      lstr.Fill(str, ' ', progress_line_len - len);
      lstr.Insert(buf, 0, str);
      progress_line_len:= len;
      io.printf('%s\r', str^);
      clock.GetClock( progress_time );
    END;
  END;
END Progress;

(*  ----------------------------------------------------------------------  *)
PROCEDURE writeLogFile(name-: ARRAY OF CHAR; merge: BOOLEAN);
VAR str: lstr.String;
BEGIN
  IF merge THEN
    file.SpaceStr(str, currLevel.mergeLevel);
    file.WrStrLn(logFile, str^);
    file.WrStr(logFile, mergeMarker);
    file.SpaceStr(str, currLevel.includeLevel - currLevel.mergeLevel - LENGTH(mergeMarker));
    file.WrStr(logFile, str^);
    file.WrStr(logFile, name);
  ELSE
    file.SpaceStr(str, currLevel.includeLevel);
    file.WrStrLn(logFile, str^);
    file.WrStr(logFile, name);
  END;
END writeLogFile;

PROCEDURE OpenLogFile * (wholename-, ext-: ARRAY OF CHAR; stat: Statistics);
VAR name, logname: lstr.String;
BEGIN
  file.GetName(wholename, name);
  file.CreateName('', name^, ext, logname);
  IF file.Open(logname^, file.crmode, logFile) THEN
    adt.NewStack(levelStack);
    NEW(currLevel);
    currLevel.includeLevel:= 0;
    currLevel.mergeLevel:= shift - LENGTH(mergeMarker);
    levelStack.Push(currLevel);
    writeLogFile(wholename, FALSE);
    currLevel.includeLevel:= shift;
    logFileExist:= TRUE;
  ELSE
    stat.Warning(6, wholename, 0, 0, logname^);
  END;
  lstr.Deallocate(name); lstr.Deallocate(logname);
END OpenLogFile;

PROCEDURE WriteLogFile * (name-: ARRAY OF CHAR; merge: BOOLEAN);
VAR e: adt.Element;
    l: Level;
BEGIN
  IF logFileExist THEN
    IF name = "" THEN
      levelStack.Pop(e);
      levelStack.Pop(e);
      IF e # NIL THEN
        currLevel:= e(Level);
        levelStack.Push(e);
      END;
    ELSE
      writeLogFile(name, merge);
      NEW(l);
      l.includeLevel:= currLevel.includeLevel + shift;
      IF merge THEN
        l.mergeLevel:= currLevel.mergeLevel;
      ELSE
        l.mergeLevel:= l.includeLevel - LENGTH(mergeMarker);
      END;
      levelStack.Push(l);
      currLevel:= l;
    END;
  END;
END WriteLogFile;

PROCEDURE CloseLogFile * ();
BEGIN
  IF logFileExist THEN
    file.Close(logFile);
    logFileExist:= FALSE;
  END;
END CloseLogFile;

(*  ----------------------------------------------------------------------  *)
PROCEDURE OpenDirFile * (wholename-, ext-: ARRAY OF CHAR);
VAR name, dirname: lstr.String;
BEGIN
  file.GetName(wholename, name);
  file.CreateName('', name^, ext, dirname);
  IF file.Open(dirname^, file.crmode, dirFile) THEN
    dirFileExist:= TRUE;
  ELSE
    Warning(6, dirname^);
  END;
  lstr.Deallocate(name); lstr.Deallocate(dirname);
END OpenDirFile;

PROCEDURE WriteDirFile * (str-: ARRAY OF CHAR);
BEGIN
  IF dirFileExist THEN file.WrStrLn(dirFile, str) END;
END WriteDirFile;

PROCEDURE CloseDirFile * ();
BEGIN
  IF dirFileExist THEN
    file.Close(dirFile);
    dirFileExist:= FALSE;
  END;
END CloseDirFile;

(*  ----------------------------------------------------------------------  *)

PROCEDURE ReadMsgs();
VAR
  f: file.FILE;
  i: INT;
  str, name: lstr.String;
  num: INT;
  line: INT;
  (*----------------------------*)
  PROCEDURE get_msg();
  VAR i: INT;
      numstr: lstr.String;
      res: ConvTypes.ConvResults;
  BEGIN
    i:= 0;
    WHILE (str[i] >= '0') & (str[i] <= '9') DO  INC(i)  END;
    numstr:= NIL;
    lstr.Extract(str^, 0, i, numstr);
    lstr.Delete(str^, 0, i);
    file.CutSpace(str^);
    WholeStr.StrToInt(numstr^, num, res); lstr.Deallocate(numstr);
    IF (res # ConvTypes.strAllRight) THEN
      (* error *)
      WasError:= TRUE;
      io.printf("\nError [ h2d.msg %d:0 ] ** Wrong message format.\n", line);
    ELSIF num > msgs_num THEN
      (* error *)
      WasError:= TRUE;
      io.printf("\nError [ h2d.msg %d:0 ] ** Incorrect message number.\n", line);
    ELSE
      lstr.Assign(str^, msgs[num]);
    END;
  END get_msg;
  (*----------------------------*)


BEGIN
  str:= NIL; name:= NIL;
  FOR i:= 0 TO msgs_num -1 DO
    msgs[i]:= NIL;
  END;
  file.CreateName('', file.ProgramName^, 'msg', name);
  IF file.Open( name^, file.rdmode, f ) THEN
    file.RdStr(f, str); line:= 1;
    WHILE str # NIL DO
      IF (lstr.Length(str^) > 0) & (str[0] # comment_char) THEN
	get_msg(); IF WasError THEN RETURN END;
      END;
      file.RdStr(f, str); INC(line);
    END;
  ELSE
    (* error *)
    io.printf("\nError [ ] ** Can't open file '%s'.\n", name^);
    WasError:= TRUE;
  END;
  lstr.Deallocate(str); lstr.Deallocate(name);
END ReadMsgs;

(*  ----------------------------------------------------------------------  *)

BEGIN
  clock.GetClock(begin_time);

  global_lines_num:= 0;
  global_files_num:= 0;
  global_errors_num:= 0;
  global_warnings_num:= 0;

  WasError := FALSE;

  progress:= FALSE;
  progress_line_len:= 0;
  clock.GetClock( progress_time );

  logFile:= NIL;
  logFileExist:= FALSE;

  Copyright();
  ReadMsgs();

END H2DMsg.
