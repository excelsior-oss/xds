(** Compiler to shell interface *)

(* *FSA 24.09.2002: Out() и вывод чисел для __GEN_C__ надо доделывать, если
                    кому-то это потребуется - там зачем-то куча кода дублируется..
*)

<*-IOVERFLOW*>
<*-COVERFLOW*>
<*-CHECKRANGE*>
<*+M2EXTENSIONS*>

IMPLEMENTATION MODULE xShell;

IMPORT SYSTEM;
IMPORT FormStr;
<* IF env_target="x86os2" THEN *>
IMPORT xrtsOS, xOS2;
<* ELSIF env_target="x86nt" THEN *>
IMPORT xWin32,
       IOChan,
       StdChans;
<* ELSIF env_target="x86linux" THEN *>
IMPORT stdio, stdlib;
<* ELSIF __GEN_C__ THEN *>
IMPORT xPOSIX;
<* END *>

CONST varName = "__XDS_SHELL__";
      HEADER  = 1C;

<* IF env_target#"x86os2" THEN *> (*==================================================*)

  <* IF env_target="x86nt" THEN *>
VAR
  handle: IOChan.ChanId;

PROCEDURE Flush;
BEGIN
--  IF xWin32.FlushFileBuffers(handle) THEN END;
END Flush;


PROCEDURE Out (s-: ARRAY OF CHAR; l: INTEGER);
VAR a: SYSTEM.ADDRESS;
BEGIN
  a := SYSTEM.ADR (s);
  IOChan.TextWrite(handle, a, l);
END Out;


PROCEDURE Start (): BOOLEAN;
VAR
  ch: CHAR;
BEGIN
  RETURN xWin32.GetEnvironmentVariable(SYSTEM.ADR(varName), SYSTEM.ADR(ch), 1) # 0;
END Start;


PROCEDURE Finish;
BEGIN
END Finish;


PROCEDURE Init;
BEGIN
  handle := StdChans.StdOutChan();
END Init;

  <* ELSIF env_target="x86linux" THEN *>


PROCEDURE Flush;
BEGIN
  stdio.fflush(stdio.stdout^);
END Flush;


PROCEDURE Out (s-: ARRAY OF CHAR; l: INTEGER);
VAR n: CARDINAL;
    a: SYSTEM.ADDRESS;
BEGIN
  a := SYSTEM.ADR (s);
  n := stdio.fwrite (a, 1, l, stdio.stdout^);
END Out;


PROCEDURE Start (): BOOLEAN;
BEGIN
  RETURN stdlib.getenv(varName) # NIL;
END Start;


PROCEDURE Finish;
BEGIN
END Finish;


PROCEDURE Init;
BEGIN
END Init;


  <* ELSIF __GEN_C__ THEN *>

PROCEDURE Flush;
BEGIN
  xPOSIX.fflush(xPOSIX.stdout^);
END Flush;

    <* IF TARGET_FAMILY <> "UNIX" THEN *>

VAR
  fd : SYSTEM.int;

PROCEDURE Out (s-: ARRAY OF CHAR; l: INTEGER);
VAR n: CARDINAL;
    a: SYSTEM.ADDRESS;
    oldmode: SYSTEM.INT;
BEGIN
  Flush;
  a := SYSTEM.ADR (s);
  SYSTEM.CODE ("oldmode = setmode(fd, O_BINARY);");
  n := xPOSIX.write(fd,a,l);
  SYSTEM.CODE ("setmode(fd, oldmode);");
END Out;

    <* ELSE *>

PROCEDURE Out (s-: ARRAY OF CHAR; l: INTEGER);
VAR n: CARDINAL;
    a: SYSTEM.ADDRESS;
BEGIN
  a := SYSTEM.ADR (s);
  n := xPOSIX.fwrite (a, 1, l, xPOSIX.stdout^);
END Out;

    <* END *>

PROCEDURE Start (): BOOLEAN;
BEGIN
  RETURN xPOSIX.getenv(varName) # NIL
END Start;


PROCEDURE Finish;
BEGIN
END Finish;


PROCEDURE Init;
BEGIN
  <* IF TARGET_FAMILY <> "UNIX" THEN *>
    fd := xPOSIX.fileno(xPOSIX.stdout^);
  <* END *>
END Init;

  <* END *>

(*------------------------------------------------------------*)

PROCEDURE OutChar (c: CHAR);
VAR arr: ARRAY [0..1] OF CHAR;
BEGIN
  arr[0] := c;
  Out(arr, 1);
END OutChar;


PROCEDURE OutText (s-: ARRAY OF CHAR);
VAR l: INTEGER;
BEGIN
  l := LENGTH(s);
  ASSERT(s[l]=0C);
  Out (s, l+1);
END OutText;


PROCEDURE OutHeader (c: CHAR);
BEGIN
  Flush;
  OutChar (HEADER);
  OutChar (c);
END OutHeader;


PROCEDURE OutNum (n: INTEGER);
VAR sz: ARRAY [0..31] OF CHAR;
BEGIN
  FormStr.print (sz, "%u_", n);
  Out (sz, LENGTH(sz));
END OutNum;



(*-------------------------------------------------------------*)

PROCEDURE String (s-: ARRAY OF CHAR);
BEGIN
  OutHeader ('S');
  OutText   (s);
END String;


PROCEDURE StartJob (s-: ARRAY OF CHAR;
                    progress_limit: INTEGER);
BEGIN
  OutHeader ('J');
  OutNum    (progress_limit);
  OutText   (s);
END StartJob;


PROCEDURE Comment (s-: ARRAY OF CHAR);
BEGIN
  OutHeader ('M');
  OutText   (s);
END Comment;


PROCEDURE Caption (s-: ARRAY OF CHAR);
BEGIN
  OutHeader('C');
  OutText  (s);
  StartJob ("", 0);
  Comment  ("");
END Caption;


PROCEDURE Progress (comment_progress, progress: INTEGER);
BEGIN
  OutHeader ('P');
  OutNum (comment_progress);
  OutNum (progress);
END Progress;


PROCEDURE StartFileList;
BEGIN
  OutHeader ('F');
END StartFileList;


PROCEDURE EndFileList;
BEGIN
  OutHeader ('X');
END EndFileList;


PROCEDURE TurnSortingOn;
BEGIN
  OutHeader ('m');
  OutChar   ('S');
END TurnSortingOn;


PROCEDURE TurnSortingOff;
BEGIN
  OutHeader ('m');
  OutChar   ('s');
END TurnSortingOff;


PROCEDURE AppendFile (name-: ARRAY OF CHAR);
  <* IF env_target="x86nt" THEN *>
VAR fullname: ARRAY [0..511] OF CHAR;
    filePart: SYSTEM.ADDRESS;
  <* END *>
BEGIN
  <* IF env_target="x86nt" THEN *>
  xWin32.GetFullPathName (name, SIZE (fullname), fullname, filePart);
  <* END *>
  OutHeader ('f');
  <* IF env_target="x86nt" THEN *>
  OutText (fullname);
  <* ELSE *>
  OutText (name);
  <* END *>
END AppendFile;


PROCEDURE Error (err_class: CHAR;
                    err_no: INTEGER;
                      x, y: INTEGER;
                    fname-,
                        s-: ARRAY OF CHAR);
VAR
  <* IF env_target="x86nt" THEN *>
    fullname: ARRAY [0..511] OF CHAR;
    filePart: SYSTEM.ADDRESS;
  <* END *>
BEGIN
  <* IF env_target="x86nt" THEN *>
  IF fname <> ""  THEN
    xWin32.GetFullPathName (fname, SIZE (fullname), fullname, filePart);
  END;
  <* END *>
  OutHeader ('E');
  OutNum(err_no);
  OutNum(y);
  OutNum(x);
  OutChar(err_class);
  <* IF env_target="x86nt" THEN *>
  IF fname <> ""  THEN
    OutText (fullname);
  ELSE
    OutText (fname);
  END;
  <* ELSE *>
  OutText (fname);
  <* END *>
  OutText (s);
END Error;


PROCEDURE ErrorIC (err_class: CHAR;
                      err_no: INTEGER;
                        x, y: INTEGER;
                      fname-,
                          s-,
                         ic-: ARRAY OF CHAR);
BEGIN
  Error (err_class, err_no, x, y, fname, s);
  OutText (ic);
END ErrorIC;


<* ELSE *> (*==== env_target="x86os2" =================================================*)

CONST (* Number of bit in usChanged *)
  CHG_ENDMAKE      = 0;
  CHG_SENDERROR    = 1;
  CHG_SENDCAPTION  = 2;
  CHG_SENDCOMMENT  = 3;
  CHG_SENDSTARTJOB = 4;
  CHG_SENDPROGRESS = 5;
  CHG_STARTFILES   = 6;
  CHG_APPENDFILE   = 7;
  CHG_ENDFILES     = 8;
  CHG_SORT_OFF     = 9;
  CHG_SORT_ON      = 10;

TYPE
  Str_100 = ARRAY [0..99] OF CHAR;
  Str_256 = ARRAY [0..255] OF CHAR;

  COMMONMEM = RECORD
    ulChanged  : BITSET;        -- CHF_  flags
    ----------------------- SendError
    ulErrClass : SYSTEM.CARD32; -- MSG_  constant
    ulErrNum   : SYSTEM.CARD32; -- error number
    lPos, lLine: SYSTEM.INT32;  -- File position (pos/line) /* MSG_ERROR, MSG_WARNING */
    szFilename : Str_256;       -- Full(!) filename         /* MSG_ERROR, MSG_WARNING) */
    szBody     : Str_256;       -- Error text
    ----------------------- SendCaption
    szCaption  : Str_256;
    ----------------------- SendComment
    szComment  : Str_256;
    ----------------------- SendStartJob
    lProgressLimit    : SYSTEM.INT32;
    szProgressComment : Str_100;
    ----------------------- SendProgress
    lProgress        : SYSTEM.INT32;
    lCommentProgress : SYSTEM.INT32;
    ----------------------- AppendFile
    szFile : ARRAY [0..xOS2.CCHMAXPATH-1] OF CHAR;
  END;

VAR pcm : POINTER TO COMMONMEM;

PROCEDURE ["SysCall"] / DosGetNamedSharedMem
( VAR pb : SYSTEM.ADDRESS;
  name   : ARRAY OF CHAR;
  flags  : BITSET
)        : xOS2.APIRET;

PROCEDURE ["SysCall"] / DosSleep
( msec : SYSTEM.CARD32
)      : xOS2.APIRET;

PROCEDURE isOkMem(): BOOLEAN;
  VAR ul1 : CARDINAL; ul2: BITSET;
BEGIN
  IF pcm = NIL THEN RETURN FALSE END;
  IF DosSleep(0) # 0 THEN RETURN FALSE END;
  ul1 := SIZE(pcm^);
  IF xOS2.DosQueryMem(pcm, ul1, ul2) # xOS2.ok THEN
    pcm := NIL;
    RETURN FALSE
  END;
  RETURN TRUE
END isOkMem;

PROCEDURE ["C"] EndMake;
BEGIN
  IF isOkMem() THEN  INCL(pcm^.ulChanged, CHG_ENDMAKE) END;
END EndMake;

PROCEDURE _strncp(src-: ARRAY OF CHAR; VAR dst: ARRAY OF CHAR);
BEGIN
  COPY(src, dst);
  dst[HIGH(dst)] := 0C;
END _strncp;

PROCEDURE Error (err_class: CHAR; err_no: INTEGER;
                      x, y: INTEGER;
                fname-, s-: ARRAY OF CHAR);
BEGIN
  LOOP
    IF NOT isOkMem() THEN RETURN END;
    IF NOT (CHG_SENDERROR IN pcm^.ulChanged) THEN EXIT END;
  END;
  pcm^.ulErrClass := ORD(err_class);
  pcm^.ulErrNum   := SYSTEM.CAST(SYSTEM.CARD32, err_no);
  pcm^.lPos  := x;
  pcm^.lLine := y;
  _strncp(fname, pcm^.szFilename);
  _strncp(s, pcm^.szBody);
  INCL(pcm^.ulChanged, CHG_SENDERROR);
END Error;

PROCEDURE String (s-: ARRAY OF CHAR);
  VAR len: CARDINAL;
BEGIN
  IF isOkMem() THEN
    IF xOS2.DosWrite (xOS2.STDOUT, s, LENGTH(s), len) # xOS2.ok THEN END;
  END;
END String;

PROCEDURE Caption (s-: ARRAY OF CHAR);
BEGIN
  IF isOkMem() THEN
    _strncp(s, pcm^.szCaption);
    INCL(pcm^.ulChanged, CHG_SENDCAPTION);
  END;
END Caption;

PROCEDURE Comment (s-: ARRAY OF CHAR);
BEGIN
  IF isOkMem() THEN
    _strncp(s, pcm^.szComment);
    INCL(pcm^.ulChanged, CHG_SENDCOMMENT);
  END
END Comment;

PROCEDURE StartJob (comment-: ARRAY OF CHAR; progress_limit: INTEGER);
BEGIN
  IF isOkMem() THEN
    _strncp(comment, pcm^.szProgressComment);
    pcm^.lProgressLimit   := progress_limit;
    pcm^.lProgress        := 0;
    pcm^.lCommentProgress := 0;
    pcm^.ulChanged := pcm^.ulChanged + {CHG_SENDSTARTJOB, CHG_SENDPROGRESS};
  END;
END StartJob;

PROCEDURE Progress (comment_progress, progress: INTEGER);
BEGIN
  IF isOkMem() THEN
    IF progress < 0 THEN pcm^.lProgress := -progress;
    ELSE                 pcm^.lProgress := pcm^.lProgress + progress;
    END;
    pcm^.lCommentProgress := comment_progress;
    INCL(pcm^.ulChanged, CHG_SENDPROGRESS);
  END;
END Progress;

PROCEDURE StartFileList;
BEGIN
  IF isOkMem() THEN
    INCL(pcm^.ulChanged, CHG_STARTFILES);
  END;
END StartFileList;

PROCEDURE AppendFile (szFile-: ARRAY OF CHAR);
BEGIN
  LOOP
    IF NOT isOkMem() THEN RETURN END;
    IF pcm^.ulChanged * {CHG_STARTFILES, CHG_APPENDFILE} = {} THEN
      EXIT
    END;
  END;
  _strncp(szFile, pcm^.szFile);
  INCL(pcm^.ulChanged, CHG_APPENDFILE);
END AppendFile;

PROCEDURE EndFileList;
BEGIN
  LOOP
    IF NOT isOkMem() THEN RETURN END;
    IF pcm^.ulChanged * {CHG_STARTFILES, CHG_APPENDFILE} = {} THEN
      EXIT
    END;
  END;
  INCL(pcm^.ulChanged, CHG_ENDFILES);
END EndFileList;

PROCEDURE TurnSortingOn;
BEGIN
  LOOP
    IF NOT isOkMem() THEN RETURN END;
    IF NOT (CHG_SENDERROR IN pcm^.ulChanged) THEN EXIT END;
  END;
  EXCL(pcm^.ulChanged, CHG_SORT_OFF);
(*
  INCL(pcm^.ulChanged, CHG_SORT_ON);
*)
END TurnSortingOn;

PROCEDURE TurnSortingOff;
BEGIN
  LOOP
    IF NOT isOkMem() THEN RETURN END;
    IF NOT (CHG_SENDERROR IN pcm^.ulChanged) THEN EXIT END;
  END;
  EXCL(pcm^.ulChanged, CHG_SORT_ON);
  INCL(pcm^.ulChanged, CHG_SORT_OFF);
END TurnSortingOff;

PROCEDURE Start() : BOOLEAN;
VAR
  p: SYSTEM.ADDRESS;
BEGIN
  IF (xOS2.DosScanEnv(varName, p) # 0) OR 
     (DosGetNamedSharedMem (pcm, "\\SHAREMEM\\XDSSHMEM.XDS",
                              xOS2.PAG_READ+xOS2.PAG_WRITE) # xOS2.ok)
  THEN
    pcm := NIL;
    RETURN FALSE;
  END;
  xrtsOS.X2C_atexit (EndMake);
  RETURN TRUE;
END Start;

PROCEDURE Finish;
BEGIN
END Finish;

PROCEDURE Init;
BEGIN
  pcm := NIL;
END Init;

<* END *>

BEGIN
  Init;
END xShell.

