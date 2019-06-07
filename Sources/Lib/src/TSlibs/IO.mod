(* Copyright (C) 2000 Excelsior. *)

<*+M2ADDTYPES   *>
<*+M2EXTENSIONS *>

<*-IOVERFLOW    *>
<*-COVERFLOW    *>
<*-CHECKINDEX   *>
<*-CHECKRANGE   *>
<*-CHECKNIL     *>

(*
   C compilers on MS-DOS derived systems are used to have <io.h> header file.
   To avoid conflicts, XDS-C includes module IO_ instead.
   Before building the library with XDS-C, copy this file to IO_.mod
*)

<* IF (BACKEND="C") & ((target_family="WIN32") OR (target_family="OS2")) THEN *>
IMPLEMENTATION MODULE IO_;
<* ELSE *>
IMPLEMENTATION MODULE IO;
<* END *>

IMPORT SYSTEM, EXCEPTIONS,
       Str,
       SeqFile, IOChan, StdChans, ChanConsts, IOConsts,
       BiosIO, xtsIO;

<* IF (BACKEND<>"C") AND (target_family<>"UNIX") THEN *>
IMPORT xtsEvQue;   (* This module has to be initialized before IO *)
<* END *>

CONST
  TrueStr  = 'TRUE';
  FalseStr = 'FALSE';

TYPE
  StrM    = ARRAY[0..MaxWrLength-1] OF CHAR;
  Ch2StrT = ARRAY[0..0] OF CHAR;

VAR
  isLnEmp   :BOOLEAN;
  CidR,CidW :IOChan.ChanId;


VAR
  source :EXCEPTIONS.ExceptionSource;

PROCEDURE RAISE (n :CARDINAL; s-: ARRAY OF CHAR);
  VAR m: ARRAY [0..79] OF CHAR;
BEGIN
  Str.Concat(m, "IO.",s);
  EXCEPTIONS.RAISE(source,n,m);
END RAISE;


(* multi-thread program support
   ============================ *)

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


(*///////////////////////////////// direct keyboard handlin' (irrelev to the curr stream) *)

PROCEDURE KeyPressed() :BOOLEAN;
BEGIN
  RETURN BiosIO.KeyPressed()
END KeyPressed;

PROCEDURE RdKey() :CHAR;
BEGIN
  RETURN BiosIO.RdKey()
END RdKey;

(* ////////////////////////////////////// redirection *)

PROCEDURE RdStr ( VAR s :ARRAY OF CHAR );
VAR
  i, h :CARDINAL;
BEGIN
  RdStrRedirect(s);
  h := HIGH(s); 
  i := 0;
  WHILE ( i<=h ) & ( s[i] # 0C ) DO
    IF ( s[i] = 0DX ) OR ( s[i] = 0AX ) THEN 
      s[i] := 0C;
      RETURN;
    END;
    INC (i);
  END; 
END RdStr;

PROCEDURE WrStr(s-: ARRAY OF CHAR);
BEGIN
  WrStrRedirect(s);
END WrStr;


PROCEDURE FileRdStr(VAR string :ARRAY OF CHAR);
VAR
  NumRead: LONGCARD;
  tmp    : SYSTEM.CARD32;
  res    : IOConsts.ReadResults;
  ch     : CHAR;
BEGIN
  IF Prompt AND isLnEmp THEN WrStr('?'); END;
  isLnEmp := TRUE;

  xtsIO.SetConMode(TRUE, tmp);
  IOChan.TextRead (CidR, SYSTEM.ADR(string), HIGH(string)+1, NumRead);
  IOChan.Look(CidR, ch, res);
  IF (res = IOConsts.endOfLine) THEN IOChan.Skip(CidR); END;
  xtsIO.SetConMode(FALSE, tmp);

  IF ( NumRead <= HIGH(string) ) THEN string[NumRead] := 0C; END;
END FileRdStr;

PROCEDURE FileWrStr(string- :ARRAY OF CHAR);
BEGIN
  isLnEmp := FALSE;
  IOChan.TextWrite (CidW, SYSTEM.ADR(string), LENGTH(string) );
END FileWrStr;


PROCEDURE RedirectInput(FileName-: ARRAY OF CHAR);
VAR
  cid :IOChan.ChanId;
  res :ChanConsts.OpenResults;
BEGIN
  SeqFile.OpenRead (cid, FileName, SeqFile.text, res);
  IF res # ChanConsts.opened
    THEN RAISE (0, 'RedirectInput: Cannot redirect');
  END;
  IF InputRedirected THEN SeqFile.Close(CidR); END;
  CidR := cid;
  InputRedirected := TRUE;
END RedirectInput;

PROCEDURE SetStdInput ();
BEGIN
  CidR := StdChans.InChan(); -- Standard or chanded by xtsEvQue
  InputRedirected := FALSE;
END SetStdInput;


PROCEDURE RedirectOutput(FileName-: ARRAY OF CHAR);
VAR
  cid :IOChan.ChanId;
  res :ChanConsts.OpenResults;
BEGIN
  SeqFile.OpenWrite (cid, FileName, SeqFile.text+SeqFile.old, res);
  IF res # ChanConsts.opened
    THEN RAISE (0, 'RedirectOutput: Cannot redirect');
  END;
  IF OutputRedirected THEN SeqFile.Close(CidW); END;
  CidW := cid;
  OutputRedirected := TRUE;
END RedirectOutput;

PROCEDURE SetStdOutput ();
BEGIN
  CidW := StdChans.StdOutChan();
  OutputRedirected := FALSE;
END SetStdOutput;


(* ////////////////////////////////// READ BUFFER *)

VAR
  Buffer      :ARRAY [0..MaxRdLength-1] OF CHAR;
  begBf,endBf :CARDINAL;                         -- begBF<endBf => Buffer isn't empty

PROCEDURE UpdRdBuff;
VAR
  rsL :CARDINAL;
BEGIN
  IF begBf<endBf THEN RETURN; END;
  RdStrRedirect(Buffer);
  rsL := LENGTH(Buffer);
  IF rsL>MaxRdLength-2 THEN rsL := MaxRdLength-2; END;

  Buffer[rsL] := CHR(13); INC(rsL);
  Buffer[rsL] := CHR(10); INC(rsL);

--  IF rsL<MaxRdLength THEN Buffer[rsL] := 0C; INC(rsL); END;
  begBf := 0; endBf := rsL;
END UpdRdBuff;

PROCEDURE RdLn;
BEGIN
  begBf := endBf;
END RdLn;

PROCEDURE EndOfRd(Skip: BOOLEAN) : BOOLEAN;
BEGIN
  IF Skip THEN
     WHILE (begBf<endBf) AND (Buffer[begBf] IN (Separators + CHARSET{CHR(0)})) DO INC(begBf) END;
  END;
  RETURN begBf >= endBf;
END EndOfRd;

PROCEDURE RdItem(VAR V: ARRAY OF CHAR);
VAR L,i : CARDINAL;
BEGIN
  setOK ( TRUE );
  L := HIGH(V);
  REPEAT
    UpdRdBuff();
    WHILE (begBf<endBf) AND ( Buffer[begBf] IN Separators ) DO INC(begBf); END;
    i := 0;
    WHILE (begBf<endBf) AND (i<=L) AND NOT ( Buffer[begBf] IN Separators ) DO
      V[i] := Buffer[begBf];
      INC(begBf);
      INC(i);
    END;
    IF i <= L THEN V[i] := 0C; END;
  UNTIL V[0] # 0C;
END RdItem;

PROCEDURE RdChar() :CHAR;
BEGIN
  UpdRdBuff();
  INC (begBf);
  RETURN Buffer[begBf-1];
END RdChar;



(* ///////////////////////// Built-in types' I/O *)

PROCEDURE WrCharRep(V: CHAR; count: CARDINAL);
VAR
  s   : StrM;
  i,j : CARDINAL;
BEGIN
  IF RdLnOnWr THEN RdLn; END;
  WHILE count>0 DO
    i := MaxWrLength-2;
    IF i>count THEN i := count END;
    DEC(count,i);
    j := 0;
    WHILE (j<i) DO s[j] := V; INC(j) END;
    s[j] := CHR(0);
    WrStr(s);
  END;
END WrCharRep;


PROCEDURE WrStrAdj( S- :ARRAY OF CHAR; Length :INTEGER );
VAR
  L : CARDINAL;
  d : INTEGER;
BEGIN
  IF RdLnOnWr  THEN RdLn; END;
  setOK ( TRUE );
  L := ABS( Length );
  d := L - LENGTH(S);
  IF (d < 0) AND ChopOff THEN
    WrCharRep('?', L);
    setOK ( FALSE );
    RETURN;
  END;
  IF (Length > 0) AND (d > 0) THEN WrCharRep( PrefixChar,d ); END;
  WrStr( S );
  IF (Length < 0) AND (d > 0) THEN WrCharRep( SuffixChar,d ); END;
END WrStrAdj;


PROCEDURE WrChar( V: CHAR );
BEGIN
  IF RdLnOnWr THEN RdLn; END;
  WrStr( Ch2StrT(V) );
END WrChar;

PROCEDURE WrBool(V: BOOLEAN; Length: INTEGER);
BEGIN
  IF V THEN
    WrStrAdj(TrueStr,Length);
  ELSE
    WrStrAdj(FalseStr,Length);
  END;
END WrBool;

PROCEDURE WrShtInt(V: SHORTINT; Length: INTEGER);
VAR
  S : StrM;
  b : BOOLEAN;
BEGIN
  Str.IntToStr( VAL(LONGINT,V), S, 10, b);
  setOK ( b );
  IF b THEN
    WrStrAdj( S, Length );
  END;
END WrShtInt;

PROCEDURE WrInt(V: INTEGER; Length: INTEGER);
VAR
  S : StrM;
  b : BOOLEAN;
BEGIN
  Str.IntToStr( VAL(LONGINT,V), S, 10, b);
  setOK ( b );
  IF b THEN
    WrStrAdj(S,Length );
  END;
END WrInt;

PROCEDURE WrLngInt(V: LONGINT; Length: INTEGER);
VAR
  S : StrM;
  b : BOOLEAN;
BEGIN
  Str.IntToStr( V,S,10,b);
  setOK ( b );
  IF b THEN
    WrStrAdj(S,Length );
  END;
END WrLngInt;

PROCEDURE WrShtCard(V: SHORTCARD; Length: INTEGER);
VAR S : StrM;
  b : BOOLEAN;
BEGIN
  Str.CardToStr(VAL(LONGCARD, V), S, 10, b);
  setOK ( b );
  IF b THEN
    WrStrAdj(S,Length );
  END;
END WrShtCard;

PROCEDURE WrCard(V: CARDINAL; Length: INTEGER);
VAR
  S : StrM;
  b : BOOLEAN;
BEGIN
  Str.CardToStr(VAL(LONGCARD, V), S, 10, b);
  setOK ( b );
  IF b THEN
    WrStrAdj(S,Length );
  END;
END WrCard;

PROCEDURE WrLngCard(V: LONGCARD; Length: INTEGER);
VAR
  S : StrM;
  b : BOOLEAN;
BEGIN
  Str.CardToStr(V,S,10,b);
  setOK ( b );
  IF b THEN
    WrStrAdj(S,Length );
  END;
END WrLngCard;

PROCEDURE WrShtHex(V: SHORTCARD; Length: INTEGER);
VAR
  S : StrM;
  b : BOOLEAN;
BEGIN
  Str.CardToStr(VAL(LONGCARD, V), S, 16, b);
  setOK ( b );
  IF b THEN
    WrStrAdj(S,Length );
  END;
END WrShtHex;

PROCEDURE WrHex(V: CARDINAL; Length: INTEGER);
VAR
  S : StrM;
  b : BOOLEAN;
BEGIN
  Str.CardToStr(VAL(LONGCARD, V), S, 16, b);
  setOK ( b );
  IF b THEN
    WrStrAdj(S,Length );
  END;
END WrHex;

PROCEDURE WrLngHex(V: LONGCARD; Length: INTEGER);
VAR S : StrM;
  b : BOOLEAN;
BEGIN
  Str.CardToStr(V,S,16,b);
  setOK ( b );
  IF b THEN
    WrStrAdj(S,Length );
  END;
END WrLngHex;

PROCEDURE WrReal(V: REAL; Precision: CARDINAL; Length: INTEGER);
VAR
  S : StrM;
  b : BOOLEAN;
BEGIN
  Str.RealToStr( VAL(LONGREAL, V ),Precision,Eng,S,b);
  setOK ( b );
  IF b THEN
    WrStrAdj(S,Length );
  END;
END WrReal;

PROCEDURE WrFixReal(V :REAL; Precision :CARDINAL; Length :INTEGER);
VAR
  S : StrM;
  b : BOOLEAN;
BEGIN
  Str.FixRealToStr( VAL(LONGREAL, V ),Precision,S,b );
  setOK ( b );
  IF b THEN
    WrStrAdj(S,Length );
  END;
END WrFixReal;

PROCEDURE WrLngReal(V :LONGREAL; Precision :CARDINAL; Length :INTEGER);
VAR
  S : StrM;
  b : BOOLEAN;
BEGIN
  Str.RealToStr( V,Precision,Eng,S,b);
  setOK ( b );
  IF b THEN
    WrStrAdj(S,Length );
  END;
END WrLngReal;

PROCEDURE WrFixLngReal(V :LONGREAL; Precision :CARDINAL; Length :INTEGER);
VAR
  S : StrM;
  b : BOOLEAN;
BEGIN
  Str.FixRealToStr(V,Precision,S,b );
  setOK ( b );
  IF b THEN
    WrStrAdj(S,Length );
  END;
END WrFixLngReal;

PROCEDURE WrLn;
BEGIN
  IF RdLnOnWr THEN RdLn; END;
  WrChar( CHR(10) );
  isLnEmp := TRUE;
END WrLn;

PROCEDURE RdBool() : BOOLEAN;
VAR s : StrM;
BEGIN
  RdItem( s );
  RETURN Str.Compare( s,TrueStr )=0;
END RdBool;

PROCEDURE RdShtInt() : SHORTINT;
VAR
  S : StrM;
  i : LONGINT;
  b : BOOLEAN;
BEGIN
  RdItem(S );
  i  := Str.StrToInt( S,10,b );
  setOK ( b AND ( i >= MIN(SHORTINT) ) AND ( i <= MAX(SHORTINT) ) );
  RETURN VAL (SHORTINT, i );
END RdShtInt;

PROCEDURE RdInt() :INTEGER;
VAR
  S : StrM;
  i : LONGINT;
  b : BOOLEAN;
BEGIN
  RdItem(S);
  i  := Str.StrToInt( S,10,b );
  setOK ( b AND ( i >= MIN(INTEGER) ) AND ( i <= MAX(INTEGER) ) );
  RETURN VAL(INTEGER, i);
END RdInt;

PROCEDURE RdLngInt() :LONGINT;
VAR
  S : StrM;
  i : LONGINT;
  b : BOOLEAN;
BEGIN
  RdItem(S);
  i  := Str.StrToInt( S,10,b );
  setOK ( b );
  RETURN i;
END RdLngInt;

PROCEDURE RdShtCard() : SHORTCARD;
VAR
  S : StrM;
  i : LONGCARD;
  b : BOOLEAN;
BEGIN
  RdItem(S);
  i  := Str.StrToCard( S,10,b );
  setOK ( b AND (i <= MAX(SHORTCARD) ) );
  RETURN VAL(SHORTCARD, i );
END RdShtCard;

PROCEDURE RdShtHex() : SHORTCARD;
VAR
  S : StrM;
  i : LONGCARD;
  b : BOOLEAN;
BEGIN
  RdItem(S);
  i  := Str.StrToCard( S,16,b );
  setOK ( b AND (i <= MAX(SHORTCARD)) );
  RETURN VAL(SHORTCARD, i );
END RdShtHex;

PROCEDURE RdCard() : CARDINAL;
VAR
  S : StrM;
  i : LONGCARD;
  b : BOOLEAN;
BEGIN
  RdItem(S);
  i  := Str.StrToCard( S,10,b );
  setOK ( b AND (i <= MAX(CARDINAL)) );
  RETURN VAL(CARDINAL, i );
END RdCard;

PROCEDURE RdHex() : CARDINAL;
VAR
  S : StrM;
  i : LONGCARD;
  b : BOOLEAN;
BEGIN
  RdItem(S);
  i  := Str.StrToCard( S,16,b );
  setOK ( b AND (i <= MAX(CARDINAL)) );
  RETURN VAL(CARDINAL, i );
END RdHex;

PROCEDURE RdLngCard() : LONGCARD;
VAR
  S : StrM;
  i : LONGCARD;
  b : BOOLEAN;
BEGIN
  RdItem(S);
  i  := Str.StrToCard( S,10,b );
  setOK ( b );
  RETURN i;
END RdLngCard;

PROCEDURE RdLngHex() : LONGCARD;
VAR
  S : StrM;
  i : LONGCARD;
  b : BOOLEAN;
BEGIN
  RdItem(S);
  i  := Str.StrToCard( S,16,b );
  setOK ( b );
  RETURN i;
END RdLngHex ;

PROCEDURE RdReal() :REAL;
VAR
  S   : StrM;
  r   : LONGREAL;
  b : BOOLEAN;
BEGIN
  RdItem(S );
  r  := Str.StrToReal( S,b);
  setOK ( b AND (r <= MAX(REAL)) AND (r >= MIN(REAL)) );
  IF OK THEN
    RETURN VAL (REAL, r);
  ELSE
    RETURN 0.0;
  END;
END RdReal;

PROCEDURE RdLngReal() : LONGREAL;
VAR
  S : StrM;
  r : LONGREAL;
  b : BOOLEAN;
BEGIN
  RdItem(S);
  r := Str.StrToReal( S,b);
  setOK ( b );
  RETURN r;

END RdLngReal;


BEGIN
  EXCEPTIONS.AllocateSource(source);

  SetStdInput  ();
  SetStdOutput ();
  WrStrRedirect := FileWrStr;
  RdStrRedirect := FileRdStr;

  begBf         := endBf;

  Prompt        := FALSE;
  RdLnOnWr      := FALSE;
  ChopOff       := FALSE;
  Separators    := CHARSET{CHR(9),CHR(10),CHR(13),CHR(26),' '};
  Eng           := FALSE;
  isLnEmp       := TRUE;
  PrefixChar    := ' ';
  SuffixChar    := ' ';
  setOK ( TRUE );

<* IF (BACKEND="C") & ((target_family="WIN32") OR (target_family="OS2")) THEN *>
END IO_
<* ELSE *>
END IO
<* END *>
.





