(* Copyright (C) 1996,98,99 XDS Ltd. *)

<* +M2ADDTYPES   *>
<* +M2EXTENSIONS *>

(*
<*-IOVERFLOW    *>
<*-COVERFLOW    *>
<*-CHECKINDEX   *>
<*-CHECKRANGE   *>
<*-CHECKNIL     *>
*)


IMPLEMENTATION MODULE Str;
IMPORT Strings,
       LongStr, LongConv,
       WholeStr,
       ConvTypes;


PROCEDURE Lows(VAR S: ARRAY OF CHAR);
CONST
  inc = ORD('a') > ORD('A');
  ofs = ABS(ORD('a')-ORD('A'));
VAR
  i  : CARDINAL;
BEGIN
  i := LENGTH(S);
  LOOP
    IF i = 0 THEN EXIT END;
    DEC(i);
    IF (S[i] >= 'A') AND (S[i] <= 'Z') THEN
     <* PUSH *> 
     <* WOFF311+ *>
      IF inc THEN INC(S[i],ofs) ELSE DEC(S[i],ofs) END;
     <* POP *> 
    END;
  END;
END Lows;

PROCEDURE Compare(S1-,S2-: ARRAY OF CHAR) : INTEGER;
TYPE
  T  = ARRAY Strings.CompareResults OF INTEGER;
CONST
  DC = T{-1, 0, 1};
BEGIN
  RETURN DC[Strings.Compare(S1,S2)];
END Compare;

PROCEDURE Concat(VAR R: ARRAY OF CHAR; S1-,S2-: ARRAY OF CHAR);
VAR
  i,l1,l2: CARDINAL;
BEGIN
  l1 := LENGTH(S1); 
  l2 := LENGTH(S2); 
  IF l1 > HIGH(R) THEN Copy(R,S1); RETURN END;
  IF l1+l2 <= HIGH(R) THEN
    R[l1+l2] := '';
    i := l2; 
  ELSE
    i := HIGH(R)-l1+1;
  END;
  LOOP 
    IF i = 0 THEN EXIT END;
    DEC(i);
    R[l1+i] := S2[i];
  END;
  i := 0;
  WHILE i < l1 DO R[i] := S1[i]; INC(i) END;
END Concat;

PROCEDURE Append(VAR R: ARRAY OF CHAR; S-: ARRAY OF CHAR);
  VAR pos,i,len: CARDINAL;
BEGIN
  pos:=LENGTH(R);
  len:=LENGTH(S);
  IF pos+len >HIGH(R)+1 THEN len:=HIGH(R)+1 - pos END;
  i:=0;
  WHILE i<len DO R[pos]:=S[i]; INC(i); INC(pos) END;
  IF pos<=HIGH(R) THEN R[pos]:=0C END;
END Append;

PROCEDURE Copy(VAR R: ARRAY OF CHAR; S-: ARRAY OF CHAR);
  VAR i,h: CARDINAL;
BEGIN
  h:=HIGH(S);
  IF h>HIGH(R) THEN h:=HIGH(R) END;
  i:=0;
  WHILE (i<=h) & (S[i]#0C) DO R[i]:=S[i]; INC(i) END;
  IF i<=HIGH(R) THEN R[i]:=0C END;
END Copy;

PROCEDURE Slice (VAR R: ARRAY OF CHAR; S-: ARRAY OF CHAR; P,L: CARDINAL);
  VAR i: CARDINAL;
BEGIN
  IF P>=LENGTH(S) THEN R[0]:=''; RETURN END;
  i:=0;
  WHILE (L>0) & (i<=HIGH(R)) & (P<=HIGH(S)) & (S[P]#0C) DO
    R[i]:=S[P]; DEC(L); INC(i); INC(P)
  END;
  IF i<=HIGH(R) THEN R[i]:=0C END;
END Slice;

PROCEDURE NextPos(S-,P-: ARRAY OF CHAR; Place: CARDINAL) : CARDINAL;
VAR b   :BOOLEAN;
    ps  :LONGCARD;
BEGIN
  Strings.FindNext(P,S,Place, b, ps);
  IF NOT b THEN ps := MAX(CARDINAL); END;
  RETURN ps;
END NextPos;

PROCEDURE Pos(S-,P-: ARRAY OF CHAR) : CARDINAL;
BEGIN
  RETURN NextPos(S,P,0);
END Pos;

PROCEDURE CharPos(S-: ARRAY OF CHAR; C: CHAR) : CARDINAL;
BEGIN
  RETURN Pos(S, Strings.String1(C));
END CharPos;

PROCEDURE RCharPos(S-: ARRAY OF CHAR; C: CHAR) : CARDINAL;
VAR
  b     :BOOLEAN;
  ps, l :LONGCARD;
BEGIN
  l := LENGTH(S);
  IF (l = 0) THEN RETURN MAX(CARDINAL) END;

  Strings.FindPrev(Strings.String1(C),S, l-1, b, ps);
  IF NOT b THEN ps := MAX(CARDINAL); END;
  RETURN ps;
END RCharPos;


PROCEDURE Item(VAR R: ARRAY OF CHAR; S-: ARRAY OF CHAR; T: CHARSET; N: CARDINAL);
VAR
  LS  :CARDINAL;
  I,J :CARDINAL;
BEGIN
  I := 0;
  LS := LENGTH(S);
  LOOP
    WHILE (I < LS) AND (S[I] IN T) DO INC(I); END;     (* skip delimiters *)
    IF (N = 0) OR (I = LS) THEN EXIT END;
    DEC(N);
    WHILE (I < LS) AND NOT (S[I] IN T) DO INC(I); END; (* skip token *)
  END;
  J := I;
  WHILE (J < LS) AND NOT (S[J] IN T) DO INC(J); END;
  Slice(R,S,I,J-I);
END Item;


PROCEDURE ItemS(VAR R :ARRAY OF CHAR; S-, T- :ARRAY OF CHAR; N :CARDINAL);
VAR
  Set :CHARSET;
  i   :CARDINAL;
BEGIN
  Set := CHARSET{};
  i := 0;
  WHILE (i <= HIGH(T)) AND (T[i] <> '') DO INCL(Set,T[i]); INC(i); END;
  Item(R,S,Set,N);
END ItemS;

PROCEDURE Prepend(VAR S1: ARRAY OF CHAR; S2-: ARRAY OF CHAR);
  VAR dlen,slen,tlen,rlen,i,n: CARDINAL;
BEGIN
  slen:=LENGTH(S2);
  IF slen=0 THEN RETURN END;
  dlen:=LENGTH(S1);
  tlen:=HIGH(S1)+1;
  IF slen>(tlen) THEN slen:=tlen END;
  IF slen=0 THEN RETURN END;
  rlen:=dlen; n:=tlen-(slen);
  IF rlen>n THEN rlen:=n END;
  IF (slen+rlen)<tlen THEN S1[slen+rlen]:=0C END;
  i:=rlen; n:=i+slen;
  WHILE rlen>0 DO DEC(n); DEC(i); S1[n]:=S1[i]; DEC(rlen) END;
  i:=0; n:=0;
  WHILE slen>0 DO S1[n]:=S2[i]; INC(n); INC(i); DEC(slen) END;
END Prepend;

PROCEDURE Insert(VAR R: ARRAY OF CHAR; S-: ARRAY OF CHAR; P: CARDINAL);
  VAR dlen,slen,tlen,rlen,i,n: CARDINAL;
BEGIN
  slen:=LENGTH(S);
  IF slen=0 THEN RETURN END;
  dlen:=LENGTH(R);
  IF P>dlen THEN P:=dlen END;
  tlen:=HIGH(R)+1;
  IF slen>(tlen-P) THEN slen:=tlen-P END;
  IF slen=0 THEN RETURN END;
  rlen:=dlen-P; n:=tlen-(P+slen);
  IF rlen>n THEN rlen:=n END;
  IF (P+slen+rlen)<tlen THEN R[P+slen+rlen]:=0C END;
  i:=P+rlen; n:=i+slen;
  WHILE rlen>0 DO DEC(n); DEC(i); R[n]:=R[i]; DEC(rlen) END;
  i:=0; n:=P;
  WHILE slen>0 DO R[n]:=S[i]; INC(n); INC(i); DEC(slen) END;
END Insert;

PROCEDURE Subst(VAR S :ARRAY OF CHAR; Target-, New-: ARRAY OF CHAR);
VAR b   :BOOLEAN;
    ps  :LONGCARD;
BEGIN
  Strings.FindNext(Target,S,0, b, ps);
  IF b
   THEN Strings.Delete(S, ps, LENGTH(Target) );
        Insert(S,New, ps);
  END;
END Subst;

PROCEDURE StrToC ( S- :ARRAY OF CHAR; VAR D :ARRAY OF CHAR) :BOOLEAN;
BEGIN
  D[0] := 0C;
  IF HIGH(D)>=LENGTH(S)
   THEN Insert(D,S,0);
        RETURN TRUE;
   ELSE RETURN FALSE;
  END;
END StrToC;

PROCEDURE StrToPas(S-: ARRAY OF CHAR; VAR D: ARRAY OF CHAR) :BOOLEAN;
VAR
  l,i : CARDINAL;
BEGIN
  l := LENGTH(S);
  IF (HIGH(D) >= l) AND (l <= ORD(MAX(CHAR))) THEN
    FOR i := l TO 1 BY -1 DO D[i] := S[i-1] END;
    D[0] := CHR(l);
    RETURN TRUE;
  ELSE
    D[0] := CHR(0);
    RETURN FALSE;
  END;
END StrToPas;

(*----------------------------- String conversion ----------------------------------------*)

PROCEDURE StrToReal(S-: ARRAY OF CHAR; VAR OK: BOOLEAN ) : LONGREAL;
VAR res :ConvTypes.ConvResults;
    r   :LONGREAL;
BEGIN
  LongStr.StrToReal (S, r, res);
  OK := (res=ConvTypes.strAllRight);
  RETURN r;
END StrToReal;


PROCEDURE FixRealToStr(V: LONGREAL; Precision: CARDINAL; VAR S: ARRAY OF CHAR; VAR OK: BOOLEAN);
VAR 
  places :INTEGER;
BEGIN
  (* TopSpeed suppresses the decimal point if Precision = 0 *)
  IF Precision = 0 THEN 
    places := -1
  ELSE
    places := Precision
  END;
  LongStr.RealToFixed(V, places, S);
  OK := (LongConv.LengthFixedReal(V,places) <= HIGH(S)+1);
  IF NOT OK THEN COPY("?",S) END;
END FixRealToStr;


PROCEDURE RealToStr(V: LONGREAL; Precision: CARDINAL; Eng: BOOLEAN;
                    VAR S: ARRAY OF CHAR; VAR OK : BOOLEAN);
VAR digits, pos :CARDINAL;
    inspoint :BOOLEAN;
    insspace :BOOLEAN;
    appzexp  :BOOLEAN;
BEGIN
  IF Precision = 0 THEN
    Precision := 1;
  ELSIF Precision > 17 THEN
    Precision := 17;
  END;
  IF Eng
   THEN LongStr.RealToEng(V, Precision, S);
        OK := (LongConv.LengthEngReal(V,INTEGER(Precision)) <= HIGH(S)+1);
   ELSE LongStr.RealToFloat(V, Precision, S);
        OK := (LongConv.LengthFloatReal(V,INTEGER(Precision)) <= HIGH(S)+1);
  END;
  IF OK THEN
        (* Now reformat according to TopSpeed rules *)
        pos := 0;
        inspoint := TRUE;
        insspace := V >= 0.0;
        LOOP
          INC(pos);
          IF (S[pos] = 0C) OR (pos = HIGH(S)) THEN appzexp := TRUE; EXIT END;
          IF inspoint AND (S[pos] = '.') THEN inspoint := FALSE END;
          IF S[pos]='E' THEN appzexp := FALSE; EXIT END;
        END; 
        digits := pos-ORD(NOT insspace)-ORD(NOT inspoint); 
        (* digits is now the number of digits in mantissa *)
        OK := LENGTH(S)+(Precision-digits)+
              ORD(inspoint)+
              ORD(insspace)+
              ORD(appzexp)*3 <= HIGH(S)+1;
        IF OK THEN
              WHILE digits<Precision DO
                Insert(S,'0',pos);
                INC(digits);
              END;
              IF inspoint THEN Insert(S,'.',pos) END;
              IF insspace THEN Insert(S,' ',0) END;
              IF appzexp  THEN Append(S,'E+0') END;
        END;
  END;
END RealToStr;



CONST Digits = "0123456789ABCDEF";

PROCEDURE TruncBase(VAR Base :CARDINAL);
BEGIN
  IF Base < 2
    THEN Base := 2
    ELSIF Base > 16
      THEN Base := 16;
  END;
END TruncBase;


PROCEDURE CardToStr(V :LONGCARD; VAR S :ARRAY OF CHAR; Base :CARDINAL; VAR OK :BOOLEAN);
VAR
  l,i  :CARDINAL;
  temp :CHAR;
BEGIN
  TruncBase(Base);
  l  := HIGH(S);
  i := 0; OK := TRUE;
  LOOP
    IF i > l THEN OK := FALSE; EXIT END;
    S[i] := Digits[V MOD Base]; INC(i);
    V := V DIV Base; IF V = 0 THEN EXIT END;
  END;
  IF i <= l THEN S[i] := 0C; END;

  l := 0;
  WHILE l<i DO
    DEC(i);
    temp := S[l]; S[l] := S [i]; S[i] := temp;
    INC(l);
  END;
END CardToStr;

PROCEDURE IntToStr (V: LONGINT; VAR S: ARRAY OF CHAR; Base: CARDINAL; VAR OK: BOOLEAN);
CONST
  OVR =  (-MAX(LONGINT)-MIN(LONGINT));
BEGIN
  IF (V = MIN(LONGINT)) THEN
    CardToStr(LONGCARD(MAX(LONGINT)) + LONGCARD(OVR), S, Base, OK);
  ELSE
    CardToStr(ABS(V), S, Base, OK);
  END;
  IF V<0
   THEN
     IF HIGH(S)+1 = LENGTH(S) THEN OK := FALSE; END;
     Prepend(S, "-");
  END;
END IntToStr;

TYPE  ATC  = ARRAY ['0'..'F'] OF SHORTCARD;
CONST ConvDig = ATC {0,1,2,3,4,5,6,7,8,9, 33,33,33,33,33,33,33, 10,11,12,13,14,15 };

PROCEDURE ComStrTo(S-: ARRAY OF CHAR; Base: CARDINAL; VAR OK: BOOLEAN) :LONGCARD;
VAR i,l :CARDINAL;
    sum :LONGCARD;
    c   :CHAR;
    x   :SHORTCARD;
BEGIN
  TruncBase(Base);
  OK := FALSE;
  i := 0; sum := 0;
  IF (S[0] = '-') OR (S[0] = '+') THEN i := 1; END;
  l := LENGTH(S);
  IF l=i THEN RETURN 0; END;
  WHILE (i < l) DO
    c := S[i];
    IF (c < '0') OR (c > 'F') THEN RETURN sum; END;
    x := ConvDig[c];
    IF (x > Base-1 ) OR (sum > (MAX(LONGCARD) - VAL(LONGCARD,x) ) DIV Base)
      THEN RETURN sum;
    END;
    sum := sum*Base+x;
    INC( i );
  END;
  OK := TRUE;
  RETURN sum;
END ComStrTo;


PROCEDURE StrToInt(S- :ARRAY OF CHAR; Base: CARDINAL; VAR OK: BOOLEAN) : LONGINT;
VAR t : LONGCARD;
BEGIN
  t  := ComStrTo( S,Base,OK);
  IF t > MAX(LONGINT) THEN OK := FALSE; END;
  IF S[0] = '-' THEN
   <* PUSH *>
   <* -IOVERFLOW *> -- avoid overflow -LONGINT(-LONGCARD(MIN(LONGINT)))
    RETURN -LONGINT(t);
   <* POP *>
  ELSE
    RETURN LONGINT(t);
  END;
END StrToInt;


PROCEDURE StrToCard(S- :ARRAY OF CHAR; Base: CARDINAL; VAR OK: BOOLEAN) : LONGCARD;
VAR t : LONGCARD;
BEGIN
  t  := ComStrTo( S,Base,OK);
  IF S[0] = '-' THEN OK := FALSE; END;
  RETURN t;
END StrToCard;


(*
  returns TRUE if the string in Source matches the string in Pattern
  The pattern may contain any number of the wild characters '*' and '?'
  '?' matches any single character
  '*' matches any sequence of characters (including a zero length sequence)
  EG '*m?t*i*' will match 'Automatic'
*)

PROCEDURE Match(S-, P-: ARRAY OF CHAR) : BOOLEAN;
VAR Sl, Pl :CARDINAL;

   PROCEDURE Rmatch(si,pi: CARDINAL) :BOOLEAN;

   (* si - beginning index in the sample to be test, pi - beg index in the pattern *)

   BEGIN
     WHILE (pi < Pl) DO
       IF P[pi]='*'
         THEN WHILE (si <= Sl) DO
                IF Rmatch(si,pi+1) THEN RETURN TRUE; END;
                INC(si);
              END;
              RETURN FALSE;
         ELSIF si=Sl
           THEN RETURN FALSE
           ELSIF P[pi]='?'
             THEN ;
             ELSIF CAP(S[si]) # CAP(P[pi])
                THEN RETURN FALSE;
       END;
       INC(pi); INC(si);
     END;
     RETURN (si=Sl);
   END Rmatch;

BEGIN
  Sl := LENGTH(S);
  Pl := LENGTH(P);
  IF Pl = 0 THEN RETURN (Sl = 0) END;
  RETURN Rmatch(0,0);
END Match;

PROCEDURE Same(Source,Pattern: ARRAY OF CHAR) : BOOLEAN;
VAR i: CARDINAL;
BEGIN
  i := 0;
  LOOP
    IF Source[i] <> Pattern[i] THEN RETURN FALSE END;
    IF Source[i] = '' THEN RETURN TRUE END;
    INC(i);
    IF i > HIGH(Source) THEN RETURN (i > HIGH(Pattern)) OR (Pattern[i] = '') END;
    IF i > HIGH(Pattern) THEN RETURN (Source[i] = '') END;
  END 
END Same;

PROCEDURE Count(S: ARRAY OF CHAR; C: CHAR) : CARDINAL;
VAR i,c: CARDINAL;
BEGIN
  i := 0; c := 0;
  WHILE (i <= HIGH(S)) AND (S[i] <> '') DO
    IF S[i] = C THEN INC(c) END;
    INC(i);
  END;
  RETURN c;
END Count;

PROCEDURE FindSubStr(S-, P- :ARRAY OF CHAR; VAR PL :ARRAY OF PosLen) : BOOLEAN;

VAR Sl, Pl :CARDINAL;
    i      :CARDINAL;

   PROCEDURE SetPL(i, p,l :CARDINAL);
   BEGIN
     IF i <= HIGH(PL)
      THEN PL[i].Len := l;
           PL[i].Pos := p;
     END;
   END SetPL;

   PROCEDURE Rmatch(si,pi, iPL: CARDINAL) :BOOLEAN;
   VAR i:CARDINAL;
   (* si - beginning index in the sample to be test, pi - beg index in the pattern *)

   BEGIN
     WHILE (pi < Pl) DO
       IF P[pi]='*'
         THEN FOR i:=si TO Sl DO
                SetPL(iPL, si, i-si);
                IF Rmatch(i,pi+1, iPL+1) THEN RETURN TRUE; END;
              END;
              RETURN FALSE;
         ELSIF si=Sl
           THEN RETURN FALSE
           ELSIF P[pi]='?'
             THEN SetPL(iPL, si, 1); INC(iPL);
             ELSIF CAP(S[si]) # CAP(P[pi])
                THEN RETURN FALSE;
       END;
       INC(pi); INC(si);
     END;
     RETURN TRUE;
   END Rmatch;
BEGIN
  Sl := LENGTH(S);
  Pl := LENGTH(P);
  IF Pl=0 THEN RETURN TRUE; END;
  FOR i:=0 TO Sl DO
    IF Rmatch(i,0, 0) THEN RETURN TRUE END;
  END;
  RETURN FALSE;
END FindSubStr;


END Str.

