(* Copyright (C) 2000 Excelsior. *)

<*+M2ADDTYPES   *>
<*+M2EXTENSIONS *>
<*-IOVERFLOW    *>
<*-COVERFLOW    *>
<*-CHECKINDEX   *>
<*-CHECKRANGE   *>
<*-CHECKNIL     *>

IMPLEMENTATION MODULE FIO; (* VitVit 2.9.96 *)

IMPORT Str,
       SYSTEM, EXCEPTIONS, xFilePos,
       IOChan, RndFile, StdChans, SeqFile, SysClock,
       ChanConsts, IOConsts,
       FileSys, SysErr, XIOChan, xtsFIO;

CONST
<* IF TARGET_FAMILY="UNIX" THEN *>
  FirstYear = 1970;
  dirSep = '/';
<* ELSE *>
  FirstYear = 1980;
  dirSep = '\';
<* END *>
  FalseStr  = 'FALSE';
  TrueStr   = 'TRUE';

CONST MaxWrLength = 256;
TYPE  StrM        = ARRAY[0..MaxWrLength-1] OF CHAR;

TYPE
  iStr = ARRAY [0..79] OF CHAR;

VAR
  source :EXCEPTIONS.ExceptionSource;
  exTx   :iStr;

CONST
  LineBreaks = Str.CHARSET{CHR(10),CHR(13)};


PROCEDURE RAISE (n :CARDINAL; s-: ARRAY OF CHAR);
VAR
  m :iStr;
BEGIN
  Str.Concat(m, "FIO.",s);
  EXCEPTIONS.RAISE(source,n,m);
END RAISE;

PROCEDURE IsFIOException () :BOOLEAN;
BEGIN
  RETURN EXCEPTIONS.IsCurrentSource(source)
END IsFIOException;


(* multi-thread program support
   ============================ *)

VAR
  IOR :CARDINAL;

PROCEDURE IOresult() :CARDINAL;
BEGIN
<* IF multithread THEN *>
  RETURN IOR;               -- temp !!!!!!!
<* ELSE *>
  RETURN IOR;
<* END *>
END IOresult;

PROCEDURE setIOR ( rc :CARDINAL );
BEGIN
<* IF multithread THEN *>
  IOR := rc;               -- temp !!!!!!!
<* ELSE *>
  IOR := rc;
<* END *>
END setIOR;


PROCEDURE ThreadOK() :BOOLEAN;
BEGIN
<* IF multithread THEN *>
  RETURN OK;               -- temp !!!!!!!
<* ELSE *>
  RETURN OK;
<* END *>
END ThreadOK;

PROCEDURE setOK ( b :BOOLEAN );
BEGIN
<* IF multithread THEN *>
  OK := b;               -- temp !!!!!!!
<* ELSE *>
  OK := b;
<* END *>
END setOK;


PROCEDURE ThreadEOF() :BOOLEAN;
BEGIN
<* IF multithread THEN *>
  RETURN EOF;               -- temp !!!!!!!
<* ELSE *>
  RETURN EOF;
<* END *>
END ThreadEOF;

PROCEDURE setEOF ( b :BOOLEAN );
BEGIN
<* IF multithread THEN *>
  EOF := b;               -- temp !!!!!!!
<* ELSE *>
  EOF := b;
<* END *>
END setEOF;


(*--------------------------------------- open/close operation -----------------------------------------*)

VAR
  cres :ChanConsts.OpenResults;

PROCEDURE RecdOpRes(cres :ChanConsts.OpenResults) :CARDINAL;
BEGIN
  CASE cres OF
    |ChanConsts.opened      :RETURN SysErr.allRight;
    |ChanConsts.tooManyOpen :RETURN SysErr.tooManyOpen;
    |ChanConsts.noSuchFile  :RETURN SysErr.fileNotFound;
    |ChanConsts.fileExists  :RETURN SysErr.fileExists;
    |ELSE                    RETURN SysErr.otherProblem;
  END;
END RecdOpRes;

PROCEDURE setShM(readonly: BOOLEAN);
BEGIN
  CASE ShareMode OF
  |ShareCompat:   IF readonly THEN xtsFIO.setShM(FALSE,TRUE) ELSE xtsFIO.setShM(TRUE, TRUE) END;
  |ShareDenyRW:   xtsFIO.setShM(TRUE,TRUE);
  |ShareDenyRD:   xtsFIO.setShM(TRUE,FALSE);
  |ShareDenyWR:   xtsFIO.setShM(FALSE,TRUE);
  |ShareDenyNone: xtsFIO.setShM(FALSE,FALSE);
  END;
END setShM;

PROCEDURE restoreShM;
BEGIN
  xtsFIO.restoreShM;
END restoreShM;


PROCEDURE Open ( Name- :ARRAY OF CHAR ) :File;
VAR
  f: File;
BEGIN
  setShM(FALSE);
  RndFile.OpenOld (f, Name, RndFile.raw+RndFile.read+RndFile.write, cres);
  restoreShM;
  setIOR ( RecdOpRes(cres) );
  IF (cres # ChanConsts.opened) AND  IOcheck
    THEN Str.Concat (exTx, "Open: cannot open file ", Name );
         RAISE (IOresult(), exTx)
  END;
  RETURN f;
EXCEPT
  xtsFIO.restoreShM;
  setIOR ( SysErr.otherProblem );
  IF NOT IOcheck THEN RETURN f; END;
END Open;

PROCEDURE OpenRead(Name- :ARRAY OF CHAR) :File;
VAR
  f: File;
BEGIN
  setShM(TRUE);
  RndFile.OpenOld(f, Name, RndFile.raw+RndFile.read, cres);
  restoreShM;
  setIOR ( RecdOpRes(cres) );
  IF (cres # ChanConsts.opened) AND  IOcheck
    THEN Str.Concat (exTx, "OpenRead: cannot open file ", Name );
         RAISE (IOresult(), exTx)
  END;
  RETURN f;
EXCEPT
  xtsFIO.restoreShM;
  setIOR ( SysErr.otherProblem );
  IF NOT IOcheck THEN RETURN f; END;
END OpenRead;


PROCEDURE Create (Name- :ARRAY OF CHAR) :File;
VAR
  f: File;
BEGIN
  setShM(FALSE);
  RndFile.OpenClean(f, Name, RndFile.raw+RndFile.read+RndFile.write+RndFile.old, cres);
  restoreShM;
  setIOR ( RecdOpRes(cres) );
  IF (cres # ChanConsts.opened) AND  IOcheck
   THEN Str.Concat (exTx, "Create: cannot open file ", Name );
         RAISE (IOresult(), exTx)
  END;
  RETURN f;
EXCEPT
  xtsFIO.restoreShM;
  setIOR ( SysErr.otherProblem );
  IF NOT IOcheck THEN RETURN f; END;
END Create;


PROCEDURE Append(Name- :ARRAY OF CHAR) :File;
VAR
  f: File;
BEGIN
  setShM(FALSE);
  RndFile.OpenOld(f, Name, RndFile.raw+RndFile.read+RndFile.write, cres);
  restoreShM;
  setIOR ( RecdOpRes(cres) );
  IF cres = ChanConsts.opened
    THEN RndFile.SetPos(f, RndFile.EndPos(f) );
    ELSIF IOcheck
      THEN Str.Concat (exTx, "Append: cannot open file ", Name );
           RAISE (IOresult(), exTx)
  END;
  RETURN f;
EXCEPT
  xtsFIO.restoreShM;
  setIOR ( SysErr.otherProblem );
  IF NOT IOcheck THEN RETURN f; END;
END Append;



PROCEDURE Close(VAR F: File);
BEGIN
  setIOR ( SysErr.allRight );
  RndFile.Close(F);
EXCEPT
  setIOR ( SysErr.otherProblem );
  IF NOT IOcheck THEN RETURN; END;
END Close;

(*--------------------------------------- rnd file operation -----------------------------------------*)

MODULE PosConv;

IMPORT SYSTEM, RndFile, xFilePos;

EXPORT QUALIFIED Rnd2LC, LC2Rnd,
                 Save, GetSaved;

VAR
  RndPos :RndFile.FilePos;

PROCEDURE Rnd2LC ( rndpos- :RndFile.FilePos ) :LONGCARD;
VAR
  b :BOOLEAN;
  c :LONGCARD;
BEGIN
  b := xFilePos.PosToCard (c, rndpos);
  RETURN c;
END Rnd2LC;

PROCEDURE LC2Rnd ( lcPos :LONGCARD ) :RndFile.FilePos;
BEGIN
  xFilePos.CardToPos (RndPos, lcPos );
  RETURN RndPos;
END LC2Rnd;


VAR
  svpos  :RndFile.FilePos;
PROCEDURE Save ( pos- :RndFile.FilePos ); BEGIN svpos := pos; END Save;
PROCEDURE GetSaved() :RndFile.FilePos; BEGIN RETURN svpos; END GetSaved;

END PosConv;

-----------

PROCEDURE GetPos(F: File) :LONGCARD;
BEGIN
  setIOR ( SysErr.allRight );
  setOK ( TRUE );
  RETURN PosConv.Rnd2LC ( RndFile.CurrentPos(F) );
EXCEPT
  setIOR ( SysErr.otherProblem );
  IF NOT IOcheck THEN RETURN 0; END;
END GetPos;


PROCEDURE Seek ( F :File; pos:LONGCARD );
BEGIN
  setIOR ( SysErr.allRight );
  RndFile.SetPos(F, PosConv.LC2Rnd(pos));
EXCEPT
  setIOR ( SysErr.otherProblem );
  IF NOT IOcheck THEN RETURN; END;
END Seek;

PROCEDURE Size(F: File) :LONGCARD;
VAR
  Ret: LONGCARD;
BEGIN
  PosConv.Save ( RndFile.CurrentPos(F) );
  Ret  := PosConv.Rnd2LC ( RndFile.EndPos(F) );
  RndFile.SetPos( F, PosConv.GetSaved() );
  RETURN Ret;
EXCEPT
  setIOR ( SysErr.otherProblem );
  IF NOT IOcheck THEN RETURN 0; END;
END Size;


PROCEDURE Truncate ( F :File );
VAR
BEGIN
  setOK ( TRUE );
  XIOChan.Truncate(F);
  IF (IOresult() # SysErr.allRight) AND IOcheck
    THEN RAISE (IOresult(), "Truncate: wrong parameters")
  END;
END Truncate;


PROCEDURE AppendHandle ( fh :LONGCARD; ReadOnly :BOOLEAN ) :File;
VAR
  flags :ChanConsts.FlagSet;
  cid   :IOChan.ChanId;
  res   :ChanConsts.OpenResults;
BEGIN
  flags := ChanConsts.read+ChanConsts.raw;
  IF NOT ReadOnly THEN flags := flags + ChanConsts.write END;
  xtsFIO.mk_chan( cid, fh, flags, res );

  setIOR ( RecdOpRes(res) );
  IF (res # ChanConsts.opened) AND IOcheck
   THEN RAISE (IOresult(), "AppendHandle: cannot make channel")
  END;
  RETURN cid;
EXCEPT
  setIOR ( SysErr.otherProblem );
  IF NOT IOcheck
    THEN RETURN StdChans.NullChan()
  END;
END AppendHandle;


(*--------------------------------------- file handling -----------------------------------------*)

PROCEDURE Erase(fname- :ARRAY OF CHAR);
VAR
  done :BOOLEAN;
BEGIN
  FileSys.Remove( fname, done );
  IF done
   THEN setIOR ( SysErr.allRight );
   ELSE setIOR ( SysErr.otherProblem );
        IF IOcheck
           THEN Str.Concat (exTx, "Erase: cannot erase file ", fname );
           RAISE (IOresult(), exTx)
        END;
  END;
END Erase;


PROCEDURE Rename( fname-, newname- :ARRAY OF CHAR);
VAR
  done :BOOLEAN;
BEGIN
  FileSys.Rename( fname, newname, done );
  IF done
   THEN setIOR ( SysErr.allRight );
   ELSE setIOR ( SysErr.otherProblem );
        IF IOcheck
           THEN Str.Concat (exTx, "Rename: cannot rename file ", fname );
           RAISE (IOresult(), exTx)
        END;
  END;
END Rename;

PROCEDURE PackDOSDateTime(dt :SysClock.DateTime): LONGCARD;
BEGIN
  RETURN  (dt.year-FirstYear)*2000000H +
          ORD(dt.month)*200000H +
          ORD(dt.day)*10000H+
          dt.hour*800H+
          dt.minute*20H+
          dt.second DIV 2;
END PackDOSDateTime;

PROCEDURE UnpackDOSDateTime(VAR dt :SysClock.DateTime; d :LONGCARD);
BEGIN
  dt.year   := d DIV 2000000H + FirstYear; d := d MOD 2000000H;
  dt.month  := d DIV 200000H;         d := d MOD 200000H;
  dt.day    := d DIV 10000H;          d := d MOD 10000H;
  dt.hour   := d DIV 800H;            d := d MOD 800H;
  dt.minute := d DIV 20H; 
  dt.second := d MOD 20H * 2;
  dt.zone   := 0;
  dt.fractions  := 0;
END UnpackDOSDateTime;

PROCEDURE GetCurrentDate() :LONGCARD;
VAR
  dt :SysClock.DateTime;
BEGIN
  setIOR ( SysErr.allRight );
  SysClock.GetClock (dt);
  RETURN PackDOSDateTime(dt);
END GetCurrentDate;

PROCEDURE GetFileDate( f : File) :LONGCARD;
VAR 
  b :FileStamp;
BEGIN 
  IF GetFileStamp(f, b) THEN 
    RETURN PackDOSDateTime(b)
  ELSE
    RETURN 0
  END;
END GetFileDate;

PROCEDURE SetFileDate( f : File ; d : LONGCARD ) ;
VAR 
  b: SysClock.DateTime;
BEGIN
  UnpackDOSDateTime(b,d);
  xtsFIO.SetFileDate(f,b);
END SetFileDate;

PROCEDURE GetFileStamp(F :File ; VAR b :FileStamp) :BOOLEAN;
BEGIN
  RETURN xtsFIO.GetFileStamp(F, b);
END GetFileStamp;

(*--------------------------------------- file buffering -----------------------------------------*)

PROCEDURE AssignBuffer (F :File; VAR Buf: ARRAY OF SYSTEM.LOC);
BEGIN
END AssignBuffer;

PROCEDURE Flush (F :File);
BEGIN
  IOChan.Flush(F);
END Flush;

(*-------------------------------------- raw read/write -----------------------------------------*)

PROCEDURE RdBin (F: File; VAR Buf: ARRAY OF SYSTEM.LOC; Count: LONGCARD) :LONGCARD;
VAR
 actRead :LONGCARD;
BEGIN
  setIOR ( SysErr.allRight );
  IF (Count > (HIGH(Buf)+1)) THEN  Count := HIGH(Buf)+1;  END;
  IOChan.RawRead ( F, SYSTEM.ADR(Buf), Count, actRead );
  setEOF ( (IOChan.ReadResult(F) = IOConsts.endOfInput) );
  RETURN actRead;
EXCEPT
  setIOR ( SysErr.otherProblem );
  setOK (FALSE);
  IF NOT IOcheck THEN RETURN 0; END;
END RdBin;

PROCEDURE WrBin (F: File; Buf- :ARRAY OF SYSTEM.LOC; Count :LONGCARD);
BEGIN
  setIOR ( SysErr.allRight );
  IOChan.RawWrite ( F, SYSTEM.ADR(Buf), Count );
EXCEPT
  setIOR ( SysErr.otherProblem );
  setOK (FALSE);
  IF NOT IOcheck THEN RETURN; END;
END WrBin;


(*-------------------------------------- formatted typed read/write -----------------------------------------*)


PROCEDURE RdChar(F: File ) : CHAR;
VAR c       :CHAR;
    actRead :LONGCARD;
BEGIN
  setIOR ( SysErr.allRight );
  setOK ( TRUE );
  IOChan.RawRead ( F, SYSTEM.ADR(c), 1, actRead);
  setEOF ( (IOChan.ReadResult(F) = IOConsts.endOfInput) );
  RETURN c;
EXCEPT
  setIOR ( SysErr.otherProblem );
  IF NOT IOcheck THEN RETURN 0C; END;
END RdChar;


PROCEDURE RdStr(F: File; VAR Buf :ARRAY OF CHAR);
VAR
  i,h : CARDINAL;
  c   : CHAR;
BEGIN
  setOK ( TRUE );
  h  := HIGH( Buf );
  FOR i := 0 TO h DO
    c := RdChar( F );
    IF ThreadEOF() OR ( c = CHR(26) ) THEN
      Buf[i] := 0C;
      RETURN;
    ELSIF (c=0DX) THEN
      Buf[i] := 0C;
      c := RdChar( F ); (* After 0DH, OAH is expected - skip it *)
      RETURN;
    ELSIF (c=0AX) THEN
      Buf[i] := 0C;
      RETURN;
    ELSE Buf[i] := c;
    END;
  END;
END RdStr;

PROCEDURE RdItem( F : File; VAR S :ARRAY OF CHAR );
VAR
  c   :CHAR;
  i,L :CARDINAL;
BEGIN
  i := 0;
  LOOP
    c := RdChar(F);
    IF NOT ThreadOK()  OR  NOT (c IN (Separators+LineBreaks)) THEN EXIT; END;
  END;
  L := HIGH( S );
  LOOP
    IF NOT ThreadOK()  OR  (c IN (Separators+LineBreaks)) THEN EXIT; END;
    S[i] := c;
    INC( i );
    IF i > L THEN
      EXIT;
    ELSE
      c := RdChar( F );
      IF ThreadEOF() THEN
        setOK ( TRUE );
        EXIT;
      END;
    END;
  END;
  IF i <= L THEN S[i] := 0C; END;
END RdItem;


PROCEDURE RdBool(F: File) :BOOLEAN;
VAR s : StrM;
BEGIN
  RdItem( F,s );
  RETURN Str.Compare( s,TrueStr )=0;
END RdBool;

PROCEDURE RdShtInt(F: File) : SHORTINT;
VAR
  S :StrM;
  i :LONGINT;
  b :BOOLEAN;
BEGIN
  RdItem(F,S );
  i  := Str.StrToInt( S, 10, b );
  setOK ( b AND ( i >= MIN(SHORTINT) ) AND ( i <= MAX(SHORTINT) ) );
  RETURN VAL (SHORTINT,  i );
END RdShtInt;

PROCEDURE RdInt(F: File) :INTEGER;
VAR
  S :StrM;
  i :LONGINT;
  b :BOOLEAN;
BEGIN
  RdItem(F,S);
  i  := Str.StrToInt( S, 10, b );
  setOK ( b AND ( i >= MIN(INTEGER) ) AND ( i <= MAX(INTEGER) ) );
  RETURN VAL (INTEGER, i);
END RdInt;

PROCEDURE RdLngInt(F: File) :LONGINT;
VAR
  S :StrM;
  i :LONGINT;
  b :BOOLEAN;
BEGIN
  RdItem(F,S);
  i  := Str.StrToInt( S, 10, b );
  setOK ( b );
  RETURN i;
END RdLngInt;

PROCEDURE RdShtCard(F: File) : SHORTCARD;
VAR
  S :StrM;
  i :LONGCARD;
  b :BOOLEAN;
BEGIN
  RdItem(F,S);
  i  := Str.StrToCard( S, 10, b );
  setOK ( b AND (i <= MAX(SHORTCARD) ) );
  RETURN VAL (SHORTCARD,  i );
END RdShtCard;


PROCEDURE RdCard(F: File) : CARDINAL;
VAR
  S :StrM;
  i :LONGCARD;
  b :BOOLEAN;
BEGIN
  RdItem(F,S);
  i  := Str.StrToCard( S, 10, b );
  setOK (b AND (i <= MAX(CARDINAL) ) );
  RETURN VAL(CARDINAL , i );
END RdCard;


PROCEDURE RdLngCard(F: File) : LONGCARD;
VAR
  S :StrM;
  i :LONGCARD;
  b :BOOLEAN;
BEGIN
  RdItem(F,S);
  i  := Str.StrToCard( S, 10, b );
  setOK ( b );
  RETURN i;
END RdLngCard;


PROCEDURE RdShtHex(F: File) : SHORTCARD;
VAR
  S :StrM;
  i :LONGCARD;
  b :BOOLEAN;
BEGIN
  RdItem(F,S);
  i := Str.StrToCard( S, 16, b );
  setOK ( b AND (i <= MAX(SHORTCARD) ) );
  RETURN VAL (SHORTCARD , i );
END RdShtHex;


PROCEDURE RdHex(F: File) : CARDINAL;
VAR
  S :StrM;
  i :LONGCARD;
  b :BOOLEAN;
BEGIN
  RdItem(F,S);
  i  := Str.StrToCard( S,16,b );
  setOK ( b AND (i <= MAX(CARDINAL) ) );
  RETURN VAL (CARDINAL,  i );
END RdHex;

PROCEDURE RdLngHex(F: File) : LONGCARD;
VAR
  S :StrM;
  i :LONGCARD;
  b :BOOLEAN;
BEGIN
  RdItem(F,S);
  i  := Str.StrToCard( S,16,b );
  setOK ( b );
  RETURN i;
END RdLngHex ;

PROCEDURE RdReal(F: File) :REAL;
VAR
  S :StrM;
  r :LONGREAL;
  b :BOOLEAN;
BEGIN
  RdItem(F,S );
  r := Str.StrToReal( S, b );
  setOK ( b AND (r <= MAX(REAL)) AND (r >= MIN(REAL)) );
  IF OK THEN
    RETURN VAL (REAL, r);
  ELSE
    RETURN 0.0;
  END;
END RdReal;

PROCEDURE RdLngReal(F: File) : LONGREAL;
VAR
  S :StrM;
  r :LONGREAL;
  b :BOOLEAN;
BEGIN
  RdItem(F,S);
  r := Str.StrToReal( S,b);
  setOK ( b );
  RETURN r;
END RdLngReal;




PROCEDURE WrChar(F: File; c: CHAR);
BEGIN
  setIOR ( SysErr.allRight );
  setOK ( TRUE );
  WrBin( F, c, 1 );
EXCEPT
  setIOR ( SysErr.otherProblem );
  IF NOT IOcheck THEN RETURN; END;
END WrChar;


PROCEDURE WrStr (F :File; Buf- :ARRAY OF CHAR);
BEGIN
  setIOR ( SysErr.allRight );
  setOK ( TRUE );
  WrBin( F, Buf, Str.Length(Buf) );
EXCEPT
  setIOR ( SysErr.otherProblem );
  IF NOT IOcheck THEN RETURN; END;
END WrStr;


PROCEDURE WrCharRep(F: File; V: CHAR ; count: CARDINAL);
VAR
  s   :StrM;
  i,j :CARDINAL;
BEGIN
  WHILE count>0 DO
    i := MaxWrLength-2;
    IF i>count THEN i := count END;
    DEC(count,i);
    j := 0;
    WHILE (j<i) DO s[j] := V; INC(j) END;
    s[j] := CHR(0);
    WrStr(F, s);
  END;
END WrCharRep;


PROCEDURE WrStrAdj(F: File; S- :ARRAY OF CHAR; Length: INTEGER);
VAR
  L :CARDINAL;
  d :INTEGER;
BEGIN
  setOK ( TRUE );
  L := ABS( Length );
  d := L - LENGTH(S);
  IF (d < 0) AND ChopOff THEN
    WrCharRep(F, '?', L);
    setOK ( FALSE );
    RETURN;
  END;
  IF (Length > 0) AND (d > 0) THEN WrCharRep( F, PrefixChar, d ); END;
  WrStr( F, S );
  IF (Length < 0) AND (d > 0) THEN WrCharRep( F, SuffixChar, d ); END;
END WrStrAdj;



PROCEDURE WrBool(F :File; V :BOOLEAN; Length :INTEGER);
BEGIN
  IF V THEN
    WrStrAdj(F,TrueStr,Length);
  ELSE
    WrStrAdj(F,FalseStr,Length);
  END;
END WrBool;

PROCEDURE WrShtInt(F: File; V: SHORTINT; Length: INTEGER);
VAR
  S : StrM;
  b : BOOLEAN;
BEGIN
  Str.IntToStr( VAL(LONGINT,V), S, 10, b);
  setOK ( b );
  IF b THEN
    WrStrAdj(F, S, Length );
  END;
END WrShtInt;

PROCEDURE WrInt(F :File; V :INTEGER; Length :INTEGER);
VAR
  S : StrM;
  b : BOOLEAN;
BEGIN
  Str.IntToStr( VAL(LONGINT,V), S, 10, b);
  setOK ( b );
  IF b THEN
    WrStrAdj(F,S,Length );
  END;
END WrInt;

PROCEDURE WrLngInt(F :File; V :LONGINT; Length :INTEGER);
VAR
  S : StrM;
  b : BOOLEAN;
BEGIN
  Str.IntToStr( V,S,10,b);
  setOK ( b );
  IF b THEN
    WrStrAdj(F,S,Length );
  END;
END WrLngInt;

PROCEDURE WrShtCard(F :File; V: SHORTCARD; Length: INTEGER);
VAR
  S :StrM;
  b :BOOLEAN;
BEGIN
  Str.CardToStr(VAL(LONGCARD,V), S, 10, b);
  setOK ( b );
  IF b THEN
    WrStrAdj(F,S,Length );
  END;
END WrShtCard;

PROCEDURE WrCard(F :File; V: CARDINAL; Length: INTEGER);
VAR
  S : StrM;
  b : BOOLEAN;
BEGIN
  Str.CardToStr(VAL(LONGCARD, V),S,10,b);
  setOK ( b );
  IF b THEN
    WrStrAdj(F,S,Length );
  END;
END WrCard;

PROCEDURE WrLngCard(F :File; V: LONGCARD; Length: INTEGER);
VAR
  S : StrM;
  b : BOOLEAN;
BEGIN
  Str.CardToStr(V,S,10,b);
  setOK ( b );
  IF b THEN
    WrStrAdj(F,S,Length );
  END;
END WrLngCard;

PROCEDURE WrShtHex(F :File; V: SHORTCARD; Length: INTEGER);
VAR
  S :StrM;
  b :BOOLEAN;
BEGIN
  Str.CardToStr(VAL(LONGCARD, V), S, 16, b);
  setOK ( b );
  IF b THEN
    WrStrAdj(F,S,Length );
  END;
END WrShtHex;

PROCEDURE WrHex(F :File; V: CARDINAL; Length: INTEGER);
VAR
  S :StrM;
  b :BOOLEAN;
BEGIN
  Str.CardToStr(VAL(LONGCARD, V),S,16,b);
  setOK ( b );
  IF b THEN
    WrStrAdj(F,S,Length );
  END;
END WrHex;

PROCEDURE WrLngHex(F :File; V: LONGCARD; Length: INTEGER);
VAR
  S :StrM;
  b :BOOLEAN;
BEGIN
  Str.CardToStr(V,S,16,b);
  setOK ( b );
  IF b THEN
    WrStrAdj(F,S,Length );
  END;
END WrLngHex;

PROCEDURE WrReal(F :File; V: REAL; Precision: CARDINAL; Length: INTEGER);
VAR
  S :StrM;
  b :BOOLEAN;
BEGIN
  Str.RealToStr( VAL(LONGREAL, V ),Precision,Eng,S,b);
  setOK ( b );
  IF b THEN
    WrStrAdj(F,S,Length );
  END;
END WrReal;

PROCEDURE WrFixReal(F :File; V :REAL; Precision :CARDINAL; Length :INTEGER);
VAR
  S :StrM;
  b :BOOLEAN;
BEGIN
  Str.FixRealToStr( VAL(LONGREAL,V), Precision,S,b );
  setOK ( b );
  IF b THEN
    WrStrAdj(F,S,Length );
  END;
END WrFixReal;

PROCEDURE WrLngReal(F :File; V :LONGREAL; Precision :CARDINAL; Length :INTEGER);
VAR
  S :StrM;
  b :BOOLEAN;
BEGIN
  Str.RealToStr( V,Precision,Eng,S,b);
  setOK ( b );
  IF b THEN
    WrStrAdj(F,S,Length );
  END;
END WrLngReal;

PROCEDURE WrFixLngReal(F :File; V :LONGREAL; Precision :CARDINAL; Length :INTEGER);
VAR
  S :StrM;
  b :BOOLEAN;
BEGIN
  Str.FixRealToStr(V,Precision,S,b );
  setOK ( b );
  IF b THEN
    WrStrAdj(F,S,Length );
  END;
END WrFixLngReal;


PROCEDURE WrLn(F: File);
TYPE dmt = ARRAY [0..1] OF CHAR;
BEGIN
  WrBin( F, dmt{CHR( 13 ),CHR( 10 )}, 2 );
END WrLn;



(*--------------------------------------- directory operations -----------------------------------------*)

PROCEDURE ChDir (Name- :ARRAY OF CHAR);
VAR
  rc :CARDINAL;
BEGIN
  xtsFIO.ChgDir (Name, rc );
  setIOR ( rc );
  IF IOcheck AND ( rc # SysErr.allRight)
    THEN Str.Concat (exTx, "ChDir: wrong path ", Name );
         RAISE ( rc, exTx)
  END;
END ChDir;

PROCEDURE MkDir (Name- :ARRAY OF CHAR);
VAR
  rc :CARDINAL;
BEGIN
  xtsFIO.CreateDir (Name, rc );
  setIOR ( rc );
  IF IOcheck AND (rc # SysErr.allRight)
    THEN Str.Concat (exTx, "MkDir: wrong path ", Name );
         RAISE (rc, exTx)
  END;
END MkDir;

PROCEDURE RmDir (Name- :ARRAY OF CHAR);
VAR
  rc :CARDINAL;
BEGIN
  xtsFIO.RmvDir (Name, rc);
  setIOR ( rc );
  IF IOcheck AND (rc # SysErr.allRight)
    THEN Str.Concat (exTx, "RmDir: wrong path ", Name );
         RAISE (rc, exTx)
  END;
END RmDir;

PROCEDURE GetDir (Drive: SHORTCARD; VAR Name :ARRAY OF CHAR);
VAR
  rc :CARDINAL;
BEGIN
  Name[0] := dirSep;
  xtsFIO.GetDir (VAL(LONGCARD, Drive), Name, rc);
  setIOR ( rc );
  IF IOcheck AND (rc # SysErr.allRight)
    THEN RAISE (rc, "GetDir: invalid argument")
  END;
END GetDir;


PROCEDURE GetLabel( drive :SHORTCARD; VAR label :ARRAY OF CHAR );
VAR
  rc :CARDINAL;
BEGIN
  xtsFIO.GetLabel(VAL(LONGCARD, drive), label, rc);
  setIOR ( rc );
  IF IOcheck AND (rc # SysErr.allRight)
    THEN RAISE (rc, "GetLabel: invalid argument")
  END;
END GetLabel;


PROCEDURE GetDrive() :SHORTCARD;
BEGIN
  setIOR ( SysErr.allRight );
  RETURN VAL (SHORTCARD, xtsFIO.GetDrive() );
END GetDrive;


PROCEDURE SetDrive ( Drive :SHORTCARD );
VAR
  rc :CARDINAL;
BEGIN
  xtsFIO.SetDrive (VAL(LONGCARD, Drive), rc);
  setIOR ( rc );
  IF IOcheck AND (rc # SysErr.allRight)
    THEN RAISE (rc, "SetDrive: invalid argument")
  END;
END SetDrive;


PROCEDURE ReadFirstEntry(DirName- :ARRAY OF CHAR;
                         attr     :FileAttr;
                         VAR D    :DirEntry) :BOOLEAN;
VAR
  rc :CARDINAL;
BEGIN
  xtsFIO.ScanFirst (DirName, attr, D, rc);
  setIOR ( rc );

  IF IOcheck AND (rc # SysErr.allRight) AND (rc # SysErr.noMoreFiles)
    THEN RAISE (rc, "ReadFirstEntry: invalid argument")
  END;

  RETURN rc = SysErr.allRight;
END ReadFirstEntry;


PROCEDURE ReadNextEntry (VAR D :DirEntry) :BOOLEAN;
VAR
  rc :CARDINAL;
BEGIN
  xtsFIO.ScanNext ( D, rc );
  setIOR ( rc );
  IF IOcheck AND (rc # SysErr.allRight) AND (rc # SysErr.noMoreFiles)
    THEN RAISE (rc, "ReadNextEntry: invalid argument")
  END;

  RETURN rc = SysErr.allRight;
END ReadNextEntry;


PROCEDURE ReadClose (VAR D :DirEntry);
BEGIN
  setIOR ( SysErr.allRight );
  xtsFIO.ScanClose ( D );
END ReadClose;



BEGIN
  EXCEPTIONS.AllocateSource(source);

  setIOR ( SysErr.allRight );
  setOK  ( TRUE );
  setEOF ( FALSE );
  Eng     := FALSE;
  IOcheck := TRUE;
  ChopOff := FALSE;
  PrefixChar := ' ';
  SuffixChar := ' ';

  ShareMode  := ShareCompat;

  Separators := Str.CHARSET{CHR(9),CHR(26),' '}+LineBreaks;

  StandardInput  := StdChans.StdInChan();
  StandardOutput := StdChans.StdOutChan();
  ErrorOutput    := StdChans.StdErrChan();
  AuxDevice      := StdChans.NullChan();

END FIO.

