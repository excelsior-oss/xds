<* IF NOT DEFINED(DEST_K26) THEN *>
  <* NEW DEST_K26- *>
<* END *>

<* Storage + *>

IMPLEMENTATION MODULE xStr;

IMPORT sys := SYSTEM;
IMPORT str := Strings;


(* Создает новую строку заданной длины *)
PROCEDURE alloc_str(VAR s: STRING; len: CARDINAL);
BEGIN
  IF len > 0 THEN
    NEW(s, len);
    s^[0] := '';
  ELSE
    s := NIL;
  END;
END alloc_str;


(* Создает и копирует в строку содержимое buf *)
PROCEDURE alloc_from(VAR s: STRING; buf-:ARRAY OF CHAR);
BEGIN
  alloc_str(s, LENGTH(buf)+1);
  COPY(buf, s^);
END alloc_from;


(* Уничтожает строку *)
PROCEDURE dealloc_str(VAR s: STRING);
BEGIN
  IF s = NIL THEN
    ASSERT(FALSE);
  ELSE
    DISPOSE(s);
  END;
  s := NIL;
END dealloc_str;


PROCEDURE Extract(s-: ARRAY OF CHAR; p,len: CARDINAL; VAR d: ARRAY OF CHAR);
VAR
  i: CARDINAL;
BEGIN
  i:=0;
  WHILE (len>0) & (i<=HIGH(d)) & (p<=HIGH(s)) & (s[p]#0C) DO
    d[i]:=s[p];
    DEC(len);
    INC(i);
    INC(p)
  END;
  IF i<=HIGH(d) THEN d[i]:=0C END;
END Extract;

PROCEDURE Insert(s-: ARRAY OF CHAR; pos: CARDINAL; VAR d: ARRAY OF CHAR);
  VAR dlen,slen,tlen,rlen,i,n: CARDINAL;
BEGIN
  slen:=LENGTH(s);
  IF slen=0 THEN RETURN END;
  dlen:=LENGTH(d);
  IF pos>dlen THEN RETURN END;
  tlen:=HIGH(d)+1;
  IF slen>(tlen-pos) THEN slen:=tlen-pos END;
  IF slen=0 THEN RETURN END;
  rlen:=dlen-pos; n:=tlen-(pos+slen);
  IF rlen>n THEN rlen:=n END;
  IF (pos+slen+rlen)<tlen THEN d[pos+slen+rlen]:=0C END;
  i:=pos+rlen; n:=i+slen;
  WHILE rlen>0 DO DEC(n); DEC(i); d[n]:=d[i]; DEC(rlen) END;
  i:=0; n:=pos;
  WHILE slen>0 DO d[n]:=s[i]; INC(n); INC(i); DEC(slen) END;
END Insert;

PROCEDURE Append(s-: ARRAY OF CHAR; VAR d: ARRAY OF CHAR);
  VAR pos,i,len: CARDINAL;
BEGIN
  pos:=LENGTH(d);
  len:=LENGTH(s);
  IF pos+len >HIGH(d)+1 THEN len:=HIGH(d)+1 - pos END;
  i:=0;
  WHILE i<len DO d[pos]:=s[i]; INC(i); INC(pos) END;
  IF pos<=HIGH(d) THEN d[pos]:=0C END;
END Append;

(* CharPos returns the position of first occurrence of character
   C in string S. If the character is not found then
   F = FALSE, otherwise F = TRUE *)
PROCEDURE CharPos (S-:ARRAY OF CHAR; C:CHAR; VAR F:BOOLEAN) : CARDINAL;
CONST
  FOUND = TRUE;
VAR
  i : CARDINAL;
  ln : CARDINAL;
BEGIN
  ln := LENGTH(S);
  IF ln > 0 THEN
    FOR i := 0 TO ln-1 DO IF S[i] = C THEN F := FOUND; RETURN i; END; END;
  END;
  F := NOT FOUND;
  RETURN ln;
END CharPos;

(* Replace 'find' to 'change' in dest *)
PROCEDURE ReplaceString (find-, change-: ARRAY OF CHAR; VAR dest: ARRAY OF CHAR);
VAR
  lf, lc, i: CARDINAL;
  ok: BOOLEAN;
BEGIN
  lf := LENGTH(find);
  lc := LENGTH(change);
  i := 0;
  LOOP
    str.FindNext(find,dest,i,ok,i);
    IF NOT ok THEN EXIT; END;
    str.Delete(dest,i,lf);
    Insert(change,i,dest);
    INC(i,lc);
  END;
END ReplaceString;

(* Replace 'find' to 'change' in dest *)
PROCEDURE ReplaceChar (find, change: CHAR; VAR dest: ARRAY OF CHAR);
VAR
  i: CARDINAL;
BEGIN
  FOR i := 0 TO LENGTH(dest)-1 DO
    IF dest[i] = find THEN dest[i] := change; END;
  END;
END ReplaceChar;


PROCEDURE UpChar (ch:CHAR) : CHAR;
BEGIN
<* IF DEST_K26 THEN *>
  IF (('a' <= ch) AND (ch <= 'z')) OR (('а' <= ch) AND (ch <= 'п')) THEN
    RETURN CHAR(sys.CARD8(ch)-32);
  ELSIF ('р' <= ch) AND (ch <= 'я') THEN
    RETURN CHAR(sys.CARD8(ch)-80);
  ELSE
    RETURN ch;
  END;
<* ELSE *>
  RETURN CAP(ch);
<* END *>
END UpChar;


PROCEDURE Uppercase (VAR s: ARRAY OF CHAR);
VAR
  l, i: CARDINAL;
BEGIN
  l := LENGTH(s);
  IF l > 0 THEN
    FOR i := 0 TO l-1 DO
      s[i] := UpChar(s[i]);
    END;
  END;
END Uppercase;


(* Удаляет все ведущие пробелы и выделяет первое слово в строке *)
PROCEDURE StripWord (str-: ARRAY OF CHAR; VAR word: ARRAY OF CHAR);
VAR
  i,j: CARDINAL;
BEGIN
  i := 0;
  WHILE (str[i] = ' ') AND (str[i] # 0C) AND (i < HIGH(str)) DO INC(i); END;
  j := i;
  WHILE (str[i] # ' ') AND (str[i] # 0C) AND (i < HIGH(str)) DO
    word[i-j] := str[i];
    INC(i);
  END;
  word[i-j] := 0C;
END StripWord;



PROCEDURE Match(Source-, Pattern-: ARRAY OF CHAR) : BOOLEAN;
(* returns TRUE if the string in Source matches the string in Pattern
   The pattern may contain any number of the wild characters '*' and '?'
   '?' matches any single character
   '*' matches any sequence of charcters (including a zero length sequence)
   EG '*m?t*i*' will match 'Automatic'
*)

   PROCEDURE Rmatch(s-: ARRAY OF CHAR; i: CARDINAL;
                    p-: ARRAY OF CHAR; j: CARDINAL) : BOOLEAN;

   (* s = to be tested ,    i = position in s *)
   (* p = pattern to match ,j = position in p *)

   VAR
     matched: BOOLEAN;
     k      : CARDINAL;
   BEGIN
     IF p[0]=CHR(0) THEN RETURN TRUE END;
     LOOP
       IF ((i > HIGH(s)) OR (s[i] = CHR(0))) AND
          ((j > HIGH(p)) OR (p[j] = CHR(0))) THEN
         RETURN TRUE
       ELSIF ((j > HIGH(p)) OR (p[j] = CHR(0))) THEN
         RETURN FALSE
       ELSIF (p[j] = '*') THEN
         k :=i;
         IF ((j = HIGH(p)) OR (p[j+1] = CHR(0))) THEN
           RETURN TRUE
         ELSE
           LOOP
             matched := Rmatch(s,k,p,j+1);
             IF matched OR (k > HIGH(s)) OR (s[k] = CHR(0)) THEN
               RETURN matched;
             END;
             INC(k);
           END;
         END
       ELSIF ((p[j]='?')AND(s[i]<>0C)) OR (CAP(p[j]) = CAP(s[i])) THEN
         INC(i);
         INC(j);
       ELSE
         RETURN FALSE;
       END;
     END;
   END Rmatch;

BEGIN
  RETURN Rmatch(Source,0,Pattern,0);
END Match;


PROCEDURE TruncStr (VAR s: ARRAY OF CHAR; l: CARDINAL);
VAR
  i,p,len: CARDINAL;
BEGIN
  IF l <= 6 THEN RETURN; END;
  len := LENGTH(s);
  IF len <= l THEN RETURN; END;
  FOR i := 0 TO 2 DO s[i] := '.'; END;
  p := len - l;
  i := 3;
  REPEAT
    s[i] := s[i+p]; 
    INC(i);
  UNTIL i = l;  
  s[i] := 0C;
END TruncStr;


CONST Digits = "0123456789ABCDEF";

PROCEDURE TruncBase(VAR Base :CARDINAL);
BEGIN
  IF Base < 2
    THEN Base := 2
    ELSIF Base > 16
      THEN Base := 16;
  END;
END TruncBase;


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


PROCEDURE StrToCard (S- :ARRAY OF CHAR; Base: CARDINAL; VAR OK: BOOLEAN): LONGCARD;
VAR t : LONGCARD;
BEGIN
  t  := ComStrTo( S,Base,OK);
  IF S[0] = '-' THEN OK := FALSE; END;
  RETURN t;
END StrToCard;



PROCEDURE Slice (VAR R: ARRAY OF CHAR; S-: ARRAY OF CHAR; P,L: CARDINAL);
  VAR i: CARDINAL;
BEGIN
  i:=0;
  WHILE (L>0) & (i<=HIGH(R)) & (P<=HIGH(S)) & (S[P]#0C) DO
    R[i]:=S[P]; DEC(L); INC(i); INC(P)
  END;
  IF i<=HIGH(R) THEN R[i]:=0C END;
END Slice;


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
  I   :CARDINAL;
BEGIN
  Set := CHARSET{};
  FOR I:=LENGTH(T)-1 TO 0 BY -1 DO INCL(Set,T[I]); END;
  Item(R,S,Set,N);
END ItemS;


END xStr.
