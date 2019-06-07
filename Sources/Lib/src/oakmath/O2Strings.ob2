(** Oakwood Oberon-2 library *)
(** Copyright (c) xTech 1995. All rights reserved. *)
<*+ O2EXTENSIONS *>
MODULE O2Strings;

PROCEDURE Length*(stringVal-: ARRAY OF CHAR): INTEGER;
(** Returns the number of characters in s up to and excluding the first 0X *)
BEGIN
  RETURN SHORT(LENGTH(stringVal))
END Length;

PROCEDURE Insert*(s: ARRAY OF CHAR; pos: INTEGER; VAR d: ARRAY OF CHAR);
(** Inserts the string s into the string d at position pos
  (0 <= pos <= Length(d)). If pos = Length(d), s is appended to d.
  If the size of d is not large enough to hold the result of operation,
  the result is truncated so that d is always terminated with 0X.
*)
  VAR dlen,slen,tlen,rlen,i,n: LONGINT;
BEGIN
  slen:=LENGTH(s);
  IF slen = 0 THEN RETURN END;
  dlen:=LENGTH(d);
  IF (pos <0) OR (pos > dlen) THEN RETURN END;
  tlen:=LEN(d)-1;
  IF slen > tlen-pos THEN slen:=tlen-pos END;
  IF slen = 0 THEN RETURN END;
  rlen:=dlen-pos; n:=tlen-(pos+slen);
  IF rlen > n THEN rlen:=n END;
  d[pos+slen+rlen]:=0X;
  i:=pos+rlen; n:=i+slen;
  WHILE rlen > 0 DO DEC(n); DEC(i); d[n]:=d[i]; DEC(rlen) END;
  i:=0; n:=pos;
  WHILE slen > 0 DO d[n]:=s[i]; INC(n); INC(i); DEC(slen) END;
END Insert;

PROCEDURE Append*(s: ARRAY OF CHAR; VAR d: ARRAY OF CHAR);
(** Has the same effect as Insert(s,Length(d),d) *)
  VAR pos,i,len: LONGINT;
BEGIN
  pos:=LENGTH(d);
  len:=LENGTH(s);
  IF pos+len >= LEN(d) THEN len:=LEN(d) - pos - 1 END;
  i:=0;
  WHILE i < len DO d[pos]:=s[i]; INC(i); INC(pos) END;
  d[pos]:=0X;
END Append;

PROCEDURE Delete*(VAR s: ARRAY OF CHAR; pos,n: INTEGER);
(** Deletes n characters from s starting at position pos
  (0 <= pos < Length(d)). If n > Length(s)-pos, the new
  length of s is pos.
*)
  VAR slen,i: LONGINT;
BEGIN
  IF n = 0 THEN RETURN END;
  slen:=LENGTH(s);
  IF (pos >=0) & (pos < slen) THEN
    i:=pos+n;
    WHILE i < slen DO s[pos]:=s[i]; INC(pos); INC(i) END;
    s[pos]:=0X;
  END;
END Delete;

PROCEDURE Replace*(s: ARRAY OF CHAR; pos: INTEGER; VAR d: ARRAY OF CHAR);
(** Has the same effect as Delete(d,pos,Length(s)) followed by
  an Insert(s,pos,d).
*)
  VAR dlen,len,i: LONGINT;
BEGIN
  dlen:=LENGTH(d);
  IF pos >= dlen THEN RETURN END;
  len:=LENGTH(s);
  IF len + pos > dlen THEN len:=dlen-pos END;
  i:=0;
  WHILE i<len DO d[pos]:=s[i]; INC(i); INC(pos) END;
END Replace;

PROCEDURE Extract*(s: ARRAY OF CHAR; p,l: INTEGER; VAR d: ARRAY OF CHAR);
(** Extracts a substring d with n characters from position pos
  (0 <= pos < Length(s)) in s. If n > Length(s) - pos, d is only
  the part of s from pos to Length(s)-1.
  If the size of d is not large enough to hold the result of operation,
  the result is truncated so that d is always terminated with 0X.
*)
  VAR i: INTEGER;
BEGIN
  i:=0;
  WHILE (l > 0) & (i < LEN(d)-1) & (p < LEN(s)) & (s[p] # 0X) DO
    d[i]:=s[p]; DEC(l); INC(i); INC(p)
  END;
  d[i]:=0X;
END Extract;

(*----------------------------------------------------------------*)

PROCEDURE Pos*(p-,s-: ARRAY OF CHAR; start: INTEGER): INTEGER;
(** Returns the position of the first occurences of the pattern p
  in s after position pos (inclusive). If p is not found, -1 is
  returned.
*)
  VAR n,i,slen,plen: LONGINT;
BEGIN
  slen:=LENGTH(s);
  plen:=LENGTH(p);
  IF (plen = 0) OR (plen > slen) THEN RETURN -1 END;
  FOR i:=start TO slen-plen DO
    n:=0;
    WHILE (n < plen) & (s[i+n]=p[n]) DO INC(n) END;
    IF n = plen THEN RETURN SHORT(i) END;
  END;
  RETURN -1
END Pos;

PROCEDURE Cap*(VAR s: ARRAY OF CHAR);
(** Replaces each lower case letter within s by its upper case
  equivalent.
*)
  VAR i: LONGINT;
BEGIN
  i:=0;
  WHILE (i < LEN(s)) & (s[i]#0X) DO s[i]:=CAP(s[i]); INC(i) END;
END Cap;

END O2Strings.
