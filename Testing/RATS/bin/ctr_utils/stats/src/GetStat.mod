<* +M2EXTENSIONS *>
MODULE GetStat;

IMPORT sys := SYSTEM;
IMPORT fmt := FormStr;
IMPORT icc := IOConsts;
IMPORT ioc := IOChan;
IMPORT seq := SeqFile;
IMPORT tio := TextIO;
IMPORT arg := ProgEnv;
IMPORT ws  := WholeStr;
IMPORT str := Strings;

FROM Printf IMPORT printf;


TYPE
  STRING = ARRAY [0..1023] OF CHAR;

VAR
  reportFileName: STRING;
  countFileName : STRING;
  n_line: CARDINAL;

  -- Options
  show_total_info: BOOLEAN;
  show_error_only: BOOLEAN;


--------------------------------------------------------------------------------
PROCEDURE Error (message-: ARRAY OF CHAR; SEQ args: sys.BYTE);
BEGIN
  printf ("Error (%s, %d): ", reportFileName, n_line);
  printf (message, args);
  printf ("\n");
  HALT (1);
END Error;


--------------------------------------------------------------------------------
TYPE
  TKeyWord = RECORD
    base   : INTEGER;   
    keyword: ARRAY [0..32] OF CHAR;
    count  : CARDINAL;
    message: ARRAY [0..64] OF CHAR;
    visible: BOOLEAN;
  END;

  TErrorKeyWords = ARRAY [0..5] OF TKeyWord;
  TTestKeyWords  = ARRAY [0..1] OF TKeyWord;
  TTestCountKeyWords = ARRAY [0..1] OF TKeyWord;

--------------------------------------------------------------------------------
CONST
  Fails = TErrorKeyWords { -- rep-files key words
    TKeyWord {-1, "compilation.", 0, "compilation error(s): %d", TRUE}
  , TKeyWord {-1, "linking."    , 0, "linking error(s)    : %d", TRUE}
  , TKeyWord {-1, "execution."  , 0, "execution error(s)  : %d", TRUE}
  , TKeyWord { 0, "expected compilation error.", 0, "expected compilation error(s): %d", FALSE}
  , TKeyWord { 2, "expected execution error."  , 0, "expected execution error(s)  : %d", FALSE}
  , TKeyWord { 2, "wrong out."                 , 0, "wrong out error(s)           : %d", FALSE}
  };

--------------------------------------------------------------------------------
CONST
  Total = TErrorKeyWords { 
    TKeyWord {-1, "compilation error(s): ", 0, "compilation error(s): %d", TRUE}
  , TKeyWord {-1, "linking error(s)    : ", 0, "linking error(s)    : %d", TRUE}
  , TKeyWord {-1, "execution error(s)  : ", 0, "execution error(s)  : %d", TRUE}
  , TKeyWord { 0, "expected compilation error(s): ", 0, "expected compilation error(s): %d", FALSE}
  , TKeyWord { 2, "expected execution error(s)  : ", 0, "expected execution error(s)  : %d", FALSE}
  , TKeyWord { 2, "wrong out error(s)           : ", 0, "wrong out error(s)           : %d", FALSE}
  };

--------------------------------------------------------------------------------
CONST
  Tests = TTestKeyWords { 
    TKeyWord {-1, "              --- Ok! ---" ,     0, "Passed test(s): %d", FALSE}
  , TKeyWord {-1, "              *** Failed! ***" , 0, "Failed test(s): %d", TRUE}
  };

--------------------------------------------------------------------------------
CONST
  TestCount = TTestCountKeyWords { -- cnt-files key words
    TKeyWord {-1, "Total tests: ",  0, "Total  test(s): %d", TRUE}
  , TKeyWord {-1, "Total errors: ", 0, "Total error(s): %d", TRUE}
  };

  TotalTestCount = TTestCountKeyWords { 
    TKeyWord {-1, "Total  test(s): ", 0, "Total  test(s): %d", TRUE}
  , TKeyWord {-1, "Total error(s): ", 0, "Total error(s): %d", TRUE}
  };

--------------------------------------------------------------------------------
VAR
  fails: TErrorKeyWords;
  total: TErrorKeyWords;
  tests: TTestKeyWords;
  count: TTestCountKeyWords;


  --------------------------------------------------------------------------------
PROCEDURE FindKeywords (line-: STRING; VAR keywords: ARRAY OF TKeyWord; get_count: BOOLEAN);
VAR i, count: CARDINAL;
    patternFound: BOOLEAN;
    posOfPattern: CARDINAL;
    tmp: STRING; 
    res: ws.ConvResults;
BEGIN
  FOR i := 0 TO HIGH (keywords) DO
    str.FindNext(keywords[i].keyword, line, 0, patternFound, posOfPattern);
    IF patternFound THEN
      IF get_count THEN
        str.Extract(line, posOfPattern + LENGTH(keywords[i].keyword), LENGTH(line), tmp);
        ws.StrToCard (tmp, count, res);
        IF res # ws.strAllRight THEN
          Error ("Invalid total number '%s'\n", tmp);
        END;
      ELSE
        count := 1;
      END;
      INC(keywords[i].count, count);
      RETURN;
    END;
  END;
END FindKeywords;



--------------------------------------------------------------------------------
PROCEDURE ParseReportFile ( file: ioc.ChanId
                          ; VAR keywords: ARRAY OF TKeyWord
                          ; get_count: BOOLEAN );
VAR line: STRING;
BEGIN
  n_line := 0;
  LOOP
    tio.ReadString(file, line);
    IF (ioc.ReadResult(file) # icc.endOfLine)
     & (ioc.ReadResult(file) # icc.allRight)
    THEN
      EXIT;
    END;
    FindKeywords(line, keywords, get_count);
    tio.SkipLine(file);
    INC (n_line);
  END;
END ParseReportFile;


--------------------------------------------------------------------------------
--                             Show Results
--------------------------------------------------------------------------------
PROCEDURE CalcComplexCount (VAR keywords: ARRAY OF TKeyWord);
VAR i: CARDINAL;
    base: INTEGER;
BEGIN
  FOR i := 0 TO HIGH (keywords) DO
    base := keywords[i].base;
    IF base # -1 THEN
      INC( keywords[base].count, keywords[i].count);
    END;
  END;
END CalcComplexCount;


--------------------------------------------------------------------------------
PROCEDURE SumCount (keywords-: ARRAY OF TKeyWord): CARDINAL;
VAR i, sum: CARDINAL;
    base: INTEGER;
BEGIN
  sum := 0;
  FOR i := 0 TO HIGH (keywords) DO
    base := keywords[i].base;
    IF (base = -1) OR (keywords[base].count = 0) THEN -- may be 'CalcComplexCount' wasn't be called
      INC(sum, keywords[i].count);
    END;
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
    IF keywords[i].visible THEN
      fmt.print (tmp, keywords[i].message, keywords[i].count);
      printf ("        %s\n", tmp);
    END;
  END;
END PrintResults;


--------------------------------------------------------------------------------
PROCEDURE ShowReport ();
VAR sum: CARDINAL;
BEGIN
  CalcComplexCount(total);
  CalcComplexCount(fails);
  IF show_total_info THEN  
    sum := SumCount(tests);
    printf("Total  groups: %d\n", sum);
    printf("Failed groups: %d\n", tests[1].count);
    printf("Total  test(s): %d\n", count[0].count);
    sum := SumCount(total);
    IF sum < count[1].count THEN
      sum := count[1].count;
    END;
    printf("Total error(s): %d\n", sum);
    PrintResults(total);
  ELSE   
    sum := SumCount(fails);
    IF sum < count[1].count THEN
      sum := count[1].count;
    END;
    IF sum = 0 THEN
      IF NOT show_error_only THEN  
        printf("%s%s\n", reportFileName, Tests[0].keyword);
        printf("  Total  test(s): %d\n", count[0].count);
        printf("  Total error(s): %d\n", sum);
      END;
    ELSE             
      printf("%s%s\n", reportFileName, Tests[1].keyword);
      printf("  Total  test(s): %d\n", count[0].count);
      printf("  Total error(s): %d\n", sum);
      PrintResults(fails);
    END;
  END;
END ShowReport;


--------------------------------------------------------------------------------
--                         Command Line Argument
--------------------------------------------------------------------------------

PROCEDURE Help ();
BEGIN
  printf ("Usage: getstat [options] <full-path-to-report>\n\n");
  printf ("Options:\n");
  printf ("  -t   Show total information\n");
  printf ("  -e   Show error information only\n");
  HALT (1);
END Help;


--------------------------------------------------------------------------------
PROCEDURE ParseOption (VAR argInx: CARDINAL; argValue-: ARRAY OF CHAR);
BEGIN
  IF argValue = "-t" THEN
    show_total_info := TRUE;
  ELSIF argValue = "-e" THEN
    show_error_only := TRUE;  
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
      COPY (argValue, reportFileName);
    END;
    INC (i);
  END; 
  
  COPY (reportFileName, countFileName);
  str.Append(".cnt", countFileName);
END ParseArguments;


--------------------------------------------------------------------------------
VAR
  freport: ioc.ChanId;
  fconut : ioc.ChanId;
  res    : seq.OpenResults;

BEGIN
  n_line := 0;
  reportFileName[0] := 0C;
  countFileName[0]  := 0C;
  show_total_info := FALSE;
  show_error_only := FALSE; 

  ParseArguments();

  seq.OpenRead(freport, reportFileName, seq.read + seq.old + seq.text, res);
  IF res # seq.opened THEN
    Error("Report %s not found\n", reportFileName);
  END;

  fails := Fails;
  total := Total;
  tests := Tests;

  IF show_total_info THEN  
    ParseReportFile(freport, total, TRUE);
    seq.Reread(freport);
    ParseReportFile(freport, tests, FALSE);
    seq.Reread(freport);
    count := TotalTestCount;
    ParseReportFile(freport, count, TRUE);
  ELSE   
    ParseReportFile(freport, fails, FALSE);
    seq.OpenRead(fconut, countFileName, seq.read + seq.old + seq.text, res);
    IF res = seq.opened THEN
      count := TestCount;
      ParseReportFile(fconut, count, TRUE);
      seq.Close(fconut);
    END;
  END;

  seq.Close(freport);

  ShowReport();
END GetStat.
