(* (c) xTech 1993. All rights reserved. *)
<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE Strings;

IMPORT xmRTS, SYSTEM;

PROCEDURE Length(stringVal-: ARRAY OF CHAR): CARDINAL;
BEGIN
  RETURN LENGTH(stringVal);
END Length;

PROCEDURE CanAssignAll(sourceLength: CARDINAL; VAR destination: ARRAY OF CHAR): BOOLEAN;
BEGIN
  RETURN sourceLength <= HIGH(destination) +1
END CanAssignAll;

PROCEDURE Assign(s: ARRAY OF CHAR; VAR d: ARRAY OF CHAR);
  VAR i,h: CARDINAL;
BEGIN
  h:=HIGH(s);
  IF h>HIGH(d) THEN h:=HIGH(d) END;
  i:=0;
  WHILE (i<=h) & (s[i]#0C) DO d[i]:=s[i]; INC(i) END;
  IF i<=HIGH(d) THEN d[i]:=0C END;
END Assign;

PROCEDURE CanExtractAll(slen,pos,len: CARDINAL; VAR d: ARRAY OF CHAR): BOOLEAN;
BEGIN
  RETURN (pos+len<=slen) & (len<=HIGH(d)+1)
END CanExtractAll;

PROCEDURE Extract(s: ARRAY OF CHAR; p,len: CARDINAL; VAR d: ARRAY OF CHAR);
  VAR i: CARDINAL;
BEGIN
  i:=0;
  WHILE (len>0) & (i<=HIGH(d)) & (p<=HIGH(s)) & (s[p]#0C) DO
    d[i]:=s[p]; DEC(len); INC(i); INC(p)
  END;
  IF i<=HIGH(d) THEN d[i]:=0C END;
END Extract;

PROCEDURE CanDeleteAll(slen,pos,len: CARDINAL): BOOLEAN;
BEGIN
  RETURN pos+len <= slen
END CanDeleteAll;

PROCEDURE Delete(VAR s: ARRAY OF CHAR; pos,len: CARDINAL);
  VAR slen,i: CARDINAL;
BEGIN
  IF len=0 THEN RETURN END;
  slen:=LENGTH(s);
  IF pos<slen THEN
    i:=pos+len;
    WHILE i<slen DO s[pos]:=s[i]; INC(pos); INC(i) END;
    s[pos]:=0C;
  END;
END Delete;

PROCEDURE CanInsertAll(slen,pos: CARDINAL; VAR d: ARRAY OF CHAR): BOOLEAN;
  VAR dlen: CARDINAL;
BEGIN
  dlen:=LENGTH(d);
  RETURN (pos <= dlen) & (slen+dlen <= HIGH(d)+1)
END CanInsertAll;

PROCEDURE Insert(s: ARRAY OF CHAR; pos: CARDINAL; VAR d: ARRAY OF CHAR);
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

PROCEDURE CanReplaceAll(slen,pos: CARDINAL; VAR d: ARRAY OF CHAR): BOOLEAN;
BEGIN
  RETURN slen+pos <= LENGTH(d)
END CanReplaceAll;

PROCEDURE Replace(s: ARRAY OF CHAR; pos: CARDINAL; VAR d: ARRAY OF CHAR);
  VAR dlen,len,i: CARDINAL;
BEGIN
  dlen:=LENGTH(d);
  IF pos>=dlen THEN RETURN END;
  len:=LENGTH(s);
  IF len + pos > dlen THEN len:=dlen-pos END;
  i:=0;
  WHILE i<len DO d[pos]:=s[i]; INC(i); INC(pos) END;
END Replace;

PROCEDURE CanAppendAll(slen: CARDINAL; VAR d: ARRAY OF CHAR): BOOLEAN;
BEGIN
  RETURN LENGTH(d) + slen <= HIGH(d)+1
END CanAppendAll;

PROCEDURE Append(s: ARRAY OF CHAR; VAR d: ARRAY OF CHAR);
  VAR pos,i,len: CARDINAL;
BEGIN
  pos:=LENGTH(d);
  len:=LENGTH(s);
  IF pos+len >HIGH(d)+1 THEN len:=HIGH(d)+1 - pos END;
  i:=0;
  WHILE i<len DO d[pos]:=s[i]; INC(i); INC(pos) END;
  IF pos<=HIGH(d) THEN d[pos]:=0C END;
END Append;

PROCEDURE CanConcatAll(s1len,s2len: CARDINAL; VAR d: ARRAY OF CHAR): BOOLEAN;
BEGIN
  RETURN s1len + s2len <= HIGH(d) + 1
END CanConcatAll;

PROCEDURE Concat(s1,s2: ARRAY OF CHAR; VAR d: ARRAY OF CHAR);
BEGIN
  Assign(s1,d); Append(s2,d);
END Concat;

(*----------------------------------------------------------------*)

<* IF (env_target="x86nt")  OR
       (env_target="x86os2") OR
       (NATIVE_LIBRARY)      THEN *>

PROCEDURE Compare(s1-,s2-: ARRAY OF CHAR): CompareResults;
BEGIN
  RETURN VAL (CompareResults, xmRTS.X2C_STRCMP_PROC (SYSTEM.ADR(s1), SIZE (s1), SYSTEM.ADR(s2), SIZE (s2))+1);
END Compare;

PROCEDURE Equal(s1-,s2-: ARRAY OF CHAR): BOOLEAN;
BEGIN
  RETURN xmRTS.X2C_STRCMP_PROC (SYSTEM.ADR(s1), SIZE (s1), SYSTEM.ADR(s2), SIZE (s2)) = 0;
END Equal;

<* ELSE *>

PROCEDURE Compare(s1-,s2-: ARRAY OF CHAR): CompareResults;
  VAR i: CARDINAL; c1,c2: CHAR;
BEGIN
  i:=0;
  LOOP
    IF i<=HIGH(s1) THEN c1:=s1[i] ELSE c1:=0C END;
    IF i<=HIGH(s2) THEN c2:=s2[i] ELSE c2:=0C END;
    IF c1>c2 THEN RETURN greater
    ELSIF c1<c2 THEN RETURN less
    ELSIF c1=0C THEN
      IF c2=0C THEN RETURN equal ELSE RETURN less END;
    END;
    INC(i)
  END;
END Compare;

PROCEDURE Equal(s1-,s2-: ARRAY OF CHAR): BOOLEAN;
BEGIN
  RETURN Compare(s1,s2)=equal
END Equal;

<* END *>

PROCEDURE FindDiff(s1-,s2-: ARRAY OF CHAR; VAR diff: BOOLEAN; VAR pos: CARDINAL);
  VAR i: CARDINAL; c1,c2: CHAR;
BEGIN
  diff:=FALSE; i:=0;
  LOOP
    IF i<=HIGH(s1) THEN c1:=s1[i] ELSE c1:=0C END;
    IF i<=HIGH(s2) THEN c2:=s2[i] ELSE c2:=0C END;
    IF c1#c2 THEN diff:=TRUE; pos:=i; RETURN
    ELSIF c1=0C THEN RETURN
    END;
    INC(i);
  END;
END FindDiff;

PROCEDURE Capitalize(VAR s: ARRAY OF CHAR);
  VAR i: CARDINAL;
BEGIN
  i:=0;
  WHILE (i<=HIGH(s)) & (s[i]#0C) DO s[i]:=CAP(s[i]); INC(i) END;
END Capitalize;

PROCEDURE FindNext(p-,s-: ARRAY OF CHAR; start: CARDINAL;
                VAR done: BOOLEAN; VAR pos: CARDINAL);
  VAR n,i,slen,plen: CARDINAL;
BEGIN
  done:=FALSE;
  slen:=LENGTH(s);
  plen:=LENGTH(p);
  IF (plen=0) OR (plen>slen) THEN RETURN END;
  FOR i:=start TO slen-plen DO
    n:=0;
    WHILE (n<plen) & (s[i+n]=p[n]) DO INC(n) END;
    IF n=plen THEN done:=TRUE; pos:=i; RETURN END;
  END;
END FindNext;

PROCEDURE FindPrev(p-,s-: ARRAY OF CHAR; start: CARDINAL;
                VAR done: BOOLEAN; VAR pos: CARDINAL);
  VAR n,i,slen,plen: CARDINAL;
BEGIN
  done:=FALSE;
  slen:=LENGTH(s);
  plen:=LENGTH(p);
  IF (plen=0) OR (plen>slen) THEN RETURN END;
  IF start > slen-plen THEN start:=slen-plen END;
  FOR i:=start TO 0 BY -1 DO
    n:=0;
    WHILE (n<plen) & (s[i+n]=p[n]) DO INC(n) END;
    IF n=plen THEN done:=TRUE; pos:=i; RETURN END;
  END;
END FindPrev;

END Strings.
