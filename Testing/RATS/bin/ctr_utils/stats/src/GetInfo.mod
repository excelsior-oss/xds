<* +M2EXTENSIONS *>
MODULE GetInfo;

IMPORT sys := SYSTEM;
IMPORT fmt := FormStr;
IMPORT icc := IOConsts;
IMPORT ioc := IOChan;
IMPORT seq := SeqFile;
IMPORT tio := TextIO;
IMPORT arg := ProgEnv;
IMPORT ws  := WholeStr;
IMPORT sio := STextIO;
IMPORT str := Strings;

FROM Printf IMPORT printf;


TYPE
  STRING   = ARRAY [0..1023] OF CHAR;
  TCounter = sys.CARD32;

VAR
  reportname: STRING;
  n_line    : CARDINAL;

  -- Options
  show_total_info: BOOLEAN;


PROCEDURE Error (message-: ARRAY OF CHAR; SEQ args: sys.BYTE);
BEGIN
  printf ("Error (%s, %d): ", reportname, n_line);
  printf (message, args);
  printf ("\n");
  HALT (1);
END Error;


--------------------------------------------------------------------------------
PROCEDURE Inc (VAR cnt: TCounter; num: TCounter);
BEGIN
  IF cnt > (MAX(TCounter) - num) THEN
    cnt := MAX(TCounter);
  ELSE
    INC(cnt, num);
  END;
END Inc;


--------------------------------------------------------------------------------
TYPE
  TKeyWord = RECORD
    count   : TCounter;
    total   : TCounter;
    keyword : ARRAY [0..32] OF CHAR;
    message : ARRAY [0..64] OF CHAR;
    required: BOOLEAN;
  END;

  TErrorKeyWords = ARRAY [0..4] OF TKeyWord;

CONST
  KeyWordsSeparator = ";";
  RequiredKeyWordCount = 4;

  Info = TErrorKeyWords { 
           TKeyWord {0, 0, "instr=", "instr= %d", TRUE}
         , TKeyWord {0, 0, "time=",  "time= %d",  TRUE}
         , TKeyWord {0, 0, "code=",  "code= %d",  TRUE}
         , TKeyWord {0, 0, "data=",  "data= %d",  TRUE}
         , TKeyWord {0, 0, "tests=", "tests= %d", FALSE}
         };


VAR
  info: TErrorKeyWords;


PROCEDURE FindKeywords (line-: STRING; VAR keywords: ARRAY OF TKeyWord);
VAR
  i, count: CARDINAL;

  patternFound: BOOLEAN;
  posOfPattern: CARDINAL;

  endFound: BOOLEAN;
  startPos, endPos: CARDINAL;
  
  tmp: STRING; 
  res: ws.ConvResults;

  notFoundRequiredKeyCount: CARDINAL; 
  notFoundOtherKeyCount: CARDINAL; 
BEGIN
  notFoundRequiredKeyCount := 0;
  notFoundOtherKeyCount    := 0;

  FOR i := 0 TO HIGH (keywords) DO
    str.FindNext (keywords[i].keyword, line, 0, patternFound, posOfPattern);
    IF patternFound THEN
      startPos := posOfPattern+LENGTH(keywords[i].keyword);
      str.FindNext (KeyWordsSeparator, line, startPos, endFound, endPos);
      IF endFound THEN
        str.Extract (line, startPos, endPos-startPos, tmp);
        ws.StrToCard (tmp, count, res);
        IF res # ws.strAllRight THEN
          Error ("Invalid number '%s'\n", tmp);
        END;
      ELSE
        Error ("Invalid format: '%s'\n", line);
      END;
      Inc (keywords[i].count, count);
    ELSIF keywords[i].required THEN
      INC(notFoundRequiredKeyCount);
    ELSE
      INC(notFoundOtherKeyCount);
    END;
  END;

  IF (notFoundRequiredKeyCount = 0) & (notFoundOtherKeyCount > 0) THEN
    FOR i := 0 TO HIGH (keywords) DO
      IF NOT keywords[i].required THEN
        Inc (keywords[4].count, 1);
      END;
    END;
  ELSIF (notFoundRequiredKeyCount > 0) & (notFoundRequiredKeyCount < RequiredKeyWordCount) THEN
    Error ("Invalid format: '%s'\n", line);
  END;
END FindKeywords;



--------------------------------------------------------------------------------
PROCEDURE ParseReportFile ( freport: ioc.ChanId
                          ; VAR keywords: ARRAY OF TKeyWord );
VAR line: STRING;
BEGIN
  LOOP
    tio.ReadString (freport, line);
    IF (ioc.ReadResult (freport) # icc.endOfLine) AND
       (ioc.ReadResult (freport) # icc.allRight)
    THEN
      EXIT;
    END;
    FindKeywords (line, keywords);
    tio.SkipLine (freport);
    INC (n_line);
  END;
END ParseReportFile;


--------------------------------------------------------------------------------
--                             Show Results
--------------------------------------------------------------------------------
PROCEDURE SumCount (keywords-: ARRAY OF TKeyWord): CARDINAL;
VAR i, sum: CARDINAL;
BEGIN
  sum := 0;
  FOR i := 0 TO HIGH (keywords) DO
    INC(sum, keywords[i].count);
  END;
  RETURN sum;
END SumCount;


--------------------------------------------------------------------------------
PROCEDURE PrintResults (keywords-: ARRAY OF TKeyWord);
VAR
  i: CARDINAL;
  tmp: STRING; 
BEGIN
  FOR i := 0 TO HIGH (keywords) DO
    fmt.print (tmp, keywords[i].message, keywords[i].count);
    IF show_total_info THEN  
      IF keywords[i].count = MAX(TCounter) THEN
        printf ("  Total %s (overflow)\n", tmp);
      ELSE
        printf ("  Total %s\n", tmp);
      END;
    ELSE
      printf (" %s;", tmp);
    END;
  END;
  printf ("\n");
END PrintResults;


--------------------------------------------------------------------------------
PROCEDURE ShowReport ();
VAR sum: CARDINAL;
    i: CARDINAL;
BEGIN
  IF NOT show_total_info THEN  
    printf ("%s:\n   ", reportname);
  END;
  PrintResults (info);
END ShowReport;


--------------------------------------------------------------------------------
--                         Command Line Argument
--------------------------------------------------------------------------------

PROCEDURE Help ();
BEGIN
  printf ("Usage: getinfo [options] <full-path-to-info-file>\n\n");
  printf ("Options:\n");
  printf ("  -t   Show total information\n");
  HALT (1);
END Help;


--------------------------------------------------------------------------------
PROCEDURE ParseOption (VAR argInx: CARDINAL; argValue-: ARRAY OF CHAR);
BEGIN
  IF argValue = "-t" THEN
    show_total_info := TRUE;
  ELSE
    Error ("unrecognized option '%s'", argValue);
  END;
END ParseOption;


--------------------------------------------------------------------------------
PROCEDURE ParseArguments ();
VAR argValue: STRING;
    argNumber, i: CARDINAL;
BEGIN
  argNumber := arg.ArgNumber ();
  IF argNumber = 0 THEN
    Help ();
  END;

  i := 0;
  WHILE i < argNumber DO
    arg.GetArg (i, argValue);
    IF (argValue[0] = "-") THEN
      ParseOption (i, argValue);
    ELSE
      COPY (argValue, reportname);
    END;
    INC (i);
  END; 
END ParseArguments;


--------------------------------------------------------------------------------
VAR
  freport: ioc.ChanId;
  res    : seq.OpenResults;

BEGIN
  n_line := 0;
  reportname[0] := 0C;
  show_total_info := FALSE;
  info := Info;

  ParseArguments ();

  seq.OpenRead (freport, reportname, seq.read + seq.old + seq.text, res);
  IF res # seq.opened THEN
    Error ("Info file %s not found\n", reportname);
  END;
  ParseReportFile (freport, info);
  seq.Close (freport);

  ShowReport ();
END GetInfo.
