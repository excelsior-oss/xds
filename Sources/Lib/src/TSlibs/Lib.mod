(* Copyright (C) 1996-99 XDS Ltd. *)

<*+M2ADDTYPES   *>
<*+M2EXTENSIONS *>

IMPLEMENTATION MODULE Lib;

IMPORT ProgEnv, ProgExec, TextIO, TimeConv, 
       StdChans, SysClock, xtsLib,
       SYSTEM, Str;

FROM SYSTEM IMPORT ADDRESS, BYTE, CAST;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;


(* New variant compatible in behavior with TS *) 
PROCEDURE HashString (S-: ARRAY OF CHAR; Range :CARDINAL) :CARDINAL;
VAR
  i, h: CARDINAL;
BEGIN
  h := 0;
  i := 0;
  WHILE (i <= HIGH(S)) AND (S[i] # 0C) DO
    h := CAST(CARDINAL,CAST(BITSET,h) / CAST(BITSET,ORD(S[i])));
    h := ((h*8) MOD 65536) + (h DIV 8192);
    h := (h+i) MOD 65536;
    INC(i);
  END;
  RETURN h MOD Range;
END HashString;

(* Old variant before 07.04.03 ------------------------------------
PROCEDURE HashString( S- :ARRAY OF CHAR; Range :CARDINAL) :CARDINAL;
VAR
  h, i :CARDINAL;
  hash :CARDINAL;

PROCEDURE xor (xa :CARDINAL) :CARDINAL;
BEGIN
  RETURN CARDINAL(BITSET(hash)/BITSET(xa));
END xor;

BEGIN
  h := LENGTH ( S );
  hash := 0;

  FOR i := 0 TO h-1 DO  <-- range error here when h=0!
    hash := xor ( ORD( S[i] ) ) * 2;
  END;

  RETURN xor (h) MOD Range;
END HashString;
----------------------------------------------------------------*)

PROCEDURE HSort(N: CARDINAL; Less: CompareProc; Swap: SwapProc);
VAR
  i,j,k : CARDINAL;
BEGIN
  IF N > 1 THEN
    i := N DIV 2;
    REPEAT
      j := i;
      LOOP (* Note that total repeats <= N/4 * 1 + N/8 * 2 + N/16 * 3 + .... *)
        k := j * 2;
        IF k > N THEN EXIT END;
        IF (k < N) AND Less(k,k+1) THEN INC(k) END;
        IF Less(j,k) THEN Swap(j,k) ELSE EXIT END;
        j := k;
      END;
      DEC(i);
    UNTIL i = 0;

    i := N;
    REPEAT
      j := 1;
      Swap(j,i);
      DEC(i);
      LOOP
        k := j * 2;
        IF k > i THEN EXIT END;
        IF ( k < i ) AND Less(k,k+1) THEN INC(k) END;
        Swap(j,k);
        j := k;
      END;
      LOOP
        k := j DIV 2;
        IF (k > 0) AND Less(k,j) THEN Swap(j,k); j := k ELSE EXIT END;
      END;
    UNTIL i = 0;
  END;
END HSort;


PROCEDURE QSort(N: CARDINAL; Less: CompareProc; Swap: SwapProc);

  PROCEDURE Sort(l,r: CARDINAL);
  VAR
    i,j:CARDINAL;
  BEGIN
    WHILE r > l DO
      i := l+1;
      j := r;
      WHILE i <= j DO
        WHILE (i <= j) AND NOT Less(l,i) DO INC(i) END;
        WHILE (i <= j) AND Less(l,j) DO DEC(j) END;
        IF i <= j THEN Swap(i,j); INC(i); DEC(j) END;
      END;
      IF j # l THEN Swap(j,l) END;
      IF j+j > r+l THEN (* small one recursively *)
        Sort(j+1,r);
        r := j-1;
      ELSE
        Sort(l,j-1);
        l := j+1;
      END;
    END;
  END Sort;

BEGIN
  Sort(1,N);
END QSort;


(*///////////////////////// RND ///////////////////////////*)

<* PUSH *>
<* COVERFLOW - *>

CONST
  HistoryMax = 54;
VAR
  History    : ARRAY [0..HistoryMax] OF CARDINAL;
  HistoryPtr : CARDINAL;
  LowerPtr   : CARDINAL;

PROCEDURE SetUpHistory(Seed: CARDINAL);
VAR
  x : LONGCARD;
  i : CARDINAL;
BEGIN
  x := VAL(LONGCARD, Seed);
  FOR  i:= 0 TO HistoryMax DO
    x := x * 3141592621 + 17;
    History[i] := CARDINAL(x DIV 10000H);
  END;

  HistoryPtr := HistoryMax;
  LowerPtr := 23;
END SetUpHistory;

PROCEDURE RANDOM (Range :CARDINAL) :CARDINAL;
VAR
  res:CARDINAL;
BEGIN
  IF HistoryPtr = 0 THEN
    IF LowerPtr = 0 THEN
      SetUpHistory(12345);
    ELSE
      HistoryPtr := HistoryMax;
      DEC (LowerPtr);
    END;
  ELSE
    DEC (HistoryPtr);
    IF LowerPtr = 0 THEN
      LowerPtr := HistoryMax;
    ELSE
      DEC (LowerPtr);
    END;
  END;
  res := History[HistoryPtr]+History[LowerPtr];
  History[HistoryPtr] := res;
  IF Range = 0 THEN
    RETURN res;
  ELSE
    RETURN res MOD Range;
  END;
END RANDOM;

PROCEDURE RANDOMIZE;
VAR d :SysClock.DateTime;
BEGIN
  SysClock.GetClock (d);
  SetUpHistory( CARDINAL(d.fractions) * CARDINAL(d.second) );
END RANDOMIZE;


PROCEDURE RAND(): REAL;
VAR
  x: LONGCARD;
BEGIN
  x := RANDOM(MAX(SYSTEM.CARD16)) + RANDOM(MAX(SYSTEM.CARD16))*10000H;
  RETURN VAL(REAL, x) / (VAL (REAL, MAX(LONGCARD)) + 1.1);
END RAND;

<* POP *> (* Restore overflow checks *)

PROCEDURE Environment(N :CARDINAL; VAR result :ARRAY OF CHAR);
BEGIN
  xtsLib.Environment(N,result);
END Environment;

PROCEDURE EnvironmentFind (name :ARRAY OF CHAR; VAR result :ARRAY OF CHAR );
  VAR str: POINTER TO ARRAY [0..16*1024-1] OF CHAR;
  VAR len: CARDINAL;
BEGIN
  len:=ProgEnv.StringLength(name);
  IF len=0 THEN
    COPY("",result);
  ELSE
    ALLOCATE(str,len+1);
    ProgEnv.String(name,str^);
    COPY(str^,result);
    DEALLOCATE(str,len+1);
  END;
END EnvironmentFind;

PROCEDURE ParamStr(VAR S :ARRAY OF CHAR; n :CARDINAL);
BEGIN
  IF n = 0 THEN 
    ProgEnv.ProgramName(S) 
  ELSE
    ProgEnv.GetArg(n-1,S) 
  END;
END ParamStr;

PROCEDURE ParamCount() :CARDINAL;
BEGIN
  RETURN ProgEnv.ArgNumber()
END ParamCount;

(*//// OS-depended realisations: ////*)

PROCEDURE Delay( t :LONGCARD);
BEGIN
  xtsLib.Delay(t);
END Delay;

PROCEDURE Speaker( FreqHz, TimeMs :CARDINAL);
BEGIN
  xtsLib.Speaker(FreqHz, TimeMs);
END Speaker;

PROCEDURE Exec (command-: ARRAY OF CHAR; Params-: ARRAY OF CHAR; Env: ExecEnvPtr): CARDINAL;
VAR 
  res: CARDINAL;
BEGIN
  IF ProgExec.Execute(command, Params, res) THEN
    RETURN res
  ELSE
    RETURN 255
  END;
END Exec;

PROCEDURE ExecCmd(command-: ARRAY OF CHAR ): CARDINAL;
VAR res: CARDINAL;
BEGIN
  IF ProgExec.Command(command, res) THEN
    RETURN res
  ELSE
    RETURN 255
  END;
END ExecCmd;


(*/////////////////////// Clock //////////////////////////////*)

PROCEDURE GetTime ( VAR Hrs,Mins,Secs,Hsecs :CARDINAL ) ;
VAR d :SysClock.DateTime;
BEGIN
  SysClock.GetClock (d);
  Hrs   := CARDINAL(d.hour);
  Mins  := CARDINAL(d.minute);
  Secs  := CARDINAL(d.second);
  Hsecs := CARDINAL(d.fractions);
END GetTime ;

PROCEDURE GetDate ( VAR Year,Month,Day :CARDINAL ;
                    VAR DayOfWeek :DayType ) ;
VAR d :SysClock.DateTime;

(*
PROCEDURE DOW (year, month, day :CARDINAL) :CARDINAL;
TYPE
  NDT = ARRAY [1..12] OF CARDINAL;
CONST
  nd  = NDT { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

VAR
  dw       :CARDINAL;
  lpY,totY :CARDINAL;  --full leap-years / total full years
  i        :CARDINAL;
BEGIN
  totY := year - 80;
  IF totY # 0
    THEN lpY := ((totY-1) DIV 4) + 1; --1980 was a leap-year
    ELSE lpY := 0;
  END;

  dw := 366 * lpY + (totY - lpY) * 365;

  FOR i := 1 TO month - 1 DO
    INC (dw, nd[i]);
  END;
  IF ((totY MOD 4) = 0) AND (month > 2)  -- if the needed year is leap-year
   THEN INC ( dw );
  END;

  INC (dw , day - 1 );

  dw := (dw + 2 ) MOD 7;     -- 1/1/80 day was Tuesday (0 is Sunday )

  RETURN dw;
END DOW;
*)

BEGIN
  SysClock.GetClock (d);
  Year := CARDINAL(d.year);
  Month := CARDINAL(d.month);
  Day := CARDINAL(d.day);
  DayOfWeek := SYSTEM.CAST(DayType, TimeConv.WeekDay( d ) );
END GetDate ;


PROCEDURE SetDate (Year,Month,Day :LONGCARD);
VAR d :SysClock.DateTime;
BEGIN
    SysClock.GetClock (d);
    d.year  := Year;
    d.month := SysClock.Month ( Month );
    d.day   := SysClock.Day   (Day);
    SysClock.SetClock ( d );
END SetDate;


PROCEDURE SetTime (Hrs,Mins,Secs,Hsecs :LONGCARD );
VAR d :SysClock.DateTime;
BEGIN
    SysClock.GetClock (d);
    d.hour      := SysClock.Hour ( Hrs  );
    d.minute    := SysClock.Min  ( Mins );
    d.second    := SysClock.Sec  ( Secs );
    d.fractions := SysClock.Fraction ( Hsecs );
    SysClock.SetClock ( d );
END SetTime ;


PROCEDURE WriteErrorString(errS- :ARRAY OF CHAR);
BEGIN
  TextIO.WriteString (StdChans.StdErrChan(), errS);
  TextIO.WriteLn (StdChans.StdErrChan());
END WriteErrorString;

PROCEDURE FatalError(S- :ARRAY OF CHAR);
BEGIN
  WriteErrorString( S );
  HALT;
END FatalError;

PROCEDURE SysErrno() :LONGCARD;
BEGIN
  RETURN 0;
END SysErrno;


(*----------------------------------------------------------------------------*)
(* Original TopSpeed implementation *)
PROCEDURE MakeAllPath(VAR Path: ARRAY OF CHAR; Drive-, Dir-, Name-, Ext-: ARRAY OF CHAR);
VAR pos: CARDINAL;
BEGIN
  Str.Copy(Path, Drive);
  IF Dir[0] # CHAR(0) THEN
    Str.Append(Path, Dir);
    pos:= Str.Length(Path)-1;
    IF NOT((Path[pos] = '\') OR (Path[pos] = '/')) THEN
        Path[pos+1]:= '\';
        Path[pos+2]:= CHAR(0);
    END;
  END;
  Str.Append(Path, Name);
  IF Ext[0] # CHAR(0) THEN
    IF Ext[0] # '.'THEN
      pos:= Str.Length(Path);
      Path[pos]:= '.';
      Path[pos+1]:= CHAR(0);
    END;
    Str.Append(Path, Ext);
  END;
  RETURN;
END MakeAllPath;


(*----------------------------------------------------------------------------*)
(* Original TopSpeed implementation *)
PROCEDURE SplitAllPath(Path- :ARRAY OF CHAR; VAR Drive, Dir, Name, Ext :ARRAY OF CHAR);
VAR n, p: CARDINAL;
    dir_start, name_start, ext_start, path_end: CARDINAL;
    c: CHAR;
BEGIN
  n := 0;
  IF (Path[0] # 0C) AND ((Path[1] = ':') OR (Path[2] = ':')) THEN
    REPEAT
      c := Path[n];
      Drive[n] := c;
      INC(n);
    UNTIL ((c = ':') OR (n > HIGH(Drive)));
  END;
  IF n <= HIGH(Drive) THEN
    Drive[n] := 0C;
  END;
  dir_start := n;
  name_start := n;
  ext_start := MAX(CARDINAL);
  LOOP
    c := Path[n];
    IF c = 0C THEN EXIT END;
    CASE c OF
     | '.' :
        IF (NOT((Path[n+1] = '.') OR (Path[n+1] = '\') OR (Path[n+1] = '/'))) THEN
          ext_start := n;
        END;
        INC(n);
     | '/', '\' :
        INC(n);
        name_start := n;
     ELSE
        INC(n);
    END;
  END;
  path_end := n;
  IF ext_start = MAX(CARDINAL) THEN
    ext_start := n;
  END;
  n:= dir_start;
  p := 0;
  WHILE ((n < name_start) AND (p <= HIGH(Dir)) AND (n < ext_start)) DO
    Dir[p] := Path[n];
    INC(p);
    INC(n);
  END;
  IF p <= HIGH(Dir) THEN
    Dir[p] := 0C;
  END;
  n := name_start;
  p := 0;
  WHILE (n < ext_start) AND (p <= HIGH(Name)) DO
    Name[p] := Path[n];
    INC(n);
    INC(p);
  END;
  IF p <= HIGH(Name) THEN
    Name[p] := 0C;
  END;
  p := 0;
  IF(ext_start >= name_start) THEN
    n := ext_start;
    WHILE (n < path_end) AND (p <= HIGH(Ext)) DO
      Ext[p] := Path[n];
      INC(n);
      INC(p);
    END;
  END;
  IF p <= HIGH(Ext) THEN
    Ext[p] := 0C;
  END;
END SplitAllPath;

PROCEDURE AddAddr(A: ADDRESS; increment: CARDINAL) : ADDRESS;
BEGIN
  RETURN SYSTEM.ADDADR(A, increment);
END AddAddr;

PROCEDURE SubAddr(A: ADDRESS; decrement: CARDINAL) : ADDRESS;
BEGIN
  RETURN SYSTEM.SUBADR(A, decrement);
END SubAddr;

PROCEDURE IncAddr(VAR A: ADDRESS; increment: CARDINAL);
BEGIN
  A := SYSTEM.ADDADR(A, increment);
END IncAddr;

PROCEDURE DecAddr(VAR A: ADDRESS; decrement: CARDINAL);
BEGIN
  A := SYSTEM.SUBADR(A, decrement);
END DecAddr;

PROCEDURE Move    (Source,Dest:ADDRESS;Count:CARDINAL);
VAR
  diff : INTEGER;
BEGIN
  IF Count = 0 THEN RETURN END;
  diff := SYSTEM.DIFADR(Source,Dest);
  IF VAL(CARDINAL,ABS(diff)) >= Count THEN
    SYSTEM.MOVE(Source, Dest, Count)
  ELSIF diff > 0 THEN
    WHILE Count > 0 DO
      Dest^  := Source^;
      Source := SYSTEM.ADDADR(Source,1);
      Dest   := SYSTEM.ADDADR(Dest,1);
      DEC(Count);
    END;
  ELSIF diff < 0 THEN
    Source := SYSTEM.ADDADR(Source, Count-1);
    Dest   := SYSTEM.ADDADR(Dest, Count-1);
    WHILE Count > 0 DO
      Dest^  := Source^;
      Source := SYSTEM.SUBADR(Source,1);
      Dest   := SYSTEM.SUBADR(Dest,1);
      DEC(Count);
    END;
  END
END Move;

PROCEDURE FastMove(Source,Dest:ADDRESS;Count:CARDINAL);
BEGIN
  SYSTEM.MOVE(Source, Dest, Count)
END FastMove;

PROCEDURE WordMove(Source,Dest:ADDRESS;WordCount:CARDINAL);
(*
   FIXME: Temporary implementation assuming 16-bit words.
          This procedure has to be provided for 16 and 32-bit words.
*)
BEGIN
  Move(Source, Dest, WordCount * 2);
END WordMove;

PROCEDURE Fill(Dest: ADDRESS; Count: CARDINAL; Value: BYTE);
BEGIN
  SYSTEM.FILL(Dest, Value, Count)
END Fill;

PROCEDURE WordFill(Dest: ADDRESS; WordCount: CARDINAL; Value: SYSTEM.CARD16);
(*
   FIXME: Temporary implementation assuming 16-bit words.
          This procedure has to be provided for 16 and 32-bit words.
*)
TYPE
  PVAL = POINTER TO SYSTEM.CARD16;
BEGIN
  WHILE WordCount > 0 DO
    PVAL(Dest)^ := Value;
    Dest        := SYSTEM.ADDADR(Dest,2);
    DEC(WordCount);
  END;    
END WordFill;

PROCEDURE ScanR  (Dest: ADDRESS; Count: CARDINAL; Value: BYTE) : CARDINAL;
VAR i : CARDINAL;
BEGIN
  i := 0;
  WHILE i < Count DO
    IF Dest^ = Value THEN RETURN i END;
    Dest := SYSTEM.ADDADR(Dest,1);
    INC(i);
  END;
  RETURN i;
END ScanR;

PROCEDURE ScanL  (Dest: ADDRESS; Count: CARDINAL; Value: BYTE) : CARDINAL;
VAR i : CARDINAL;
BEGIN
  i := 0;
  WHILE i < Count DO
    IF Dest^ = Value THEN RETURN i END;
    Dest := SYSTEM.SUBADR(Dest,1);
    INC(i);
  END;
  RETURN i;
END ScanL;

PROCEDURE ScanNeR(Dest: ADDRESS; Count: CARDINAL; Value: BYTE) : CARDINAL;
VAR i : CARDINAL;
BEGIN
  i := 0;
  WHILE i < Count DO
    IF Dest^ <> Value THEN RETURN i END;
    Dest := SYSTEM.ADDADR(Dest,1);
    INC(i);
  END;
  RETURN i;
END ScanNeR;

PROCEDURE ScanNeL(Dest: ADDRESS; Count: CARDINAL; Value: BYTE) : CARDINAL;
VAR i : CARDINAL;
BEGIN
  i := 0;
  WHILE i < Count DO
    IF Dest^ <> Value THEN RETURN i END;
    Dest := SYSTEM.SUBADR(Dest,1);
    INC(i);
  END;
  RETURN i;
END ScanNeL;

PROCEDURE Compare(Source,Dest: ADDRESS; Len: CARDINAL) : CARDINAL;
VAR i: CARDINAL;
BEGIN
  i := 0;
  WHILE (i < Len) AND (Source^ = Dest^) DO
    Source := SYSTEM.ADDADR(Source,1);
    Dest   := SYSTEM.ADDADR(Dest,1);
    INC(i);
  END;
  RETURN i    
END Compare;

BEGIN
  HistoryPtr := 0;
  LowerPtr   := 0;
END Lib.
