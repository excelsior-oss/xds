(** Copyright (c) 1995 xTech Ltd, Russia. All Rights Reserved. *)
(** XDS Librarian. Strings library. *)
<* +o2extensions *>
MODULE xlStrings; (* Hady. Oct 25 1995 *)

IMPORT  SYSTEM, FormOut;

TYPE
  String *= POINTER TO ARRAY OF CHAR;
  SIO = POINTER TO SIODesc;
  SIODesc = RECORD
    buf: String;
    pos: LONGINT;
  END;

PROCEDURE Make*(from-: ARRAY OF CHAR): String;
  VAR s: String;
BEGIN
  NEW(s,LENGTH(from)+1);
  COPY(from,s^);
  RETURN s;
END Make;

PROCEDURE Assign*(s-: ARRAY OF CHAR; VAR d: String);
  VAR l: LONGINT;
BEGIN
  l:=LENGTH(s);
  IF (d=NIL) OR (l>=LEN(d^)) THEN NEW(d,l+1) END;
  COPY(s,d^);
END Assign;

PROCEDURE Append*(a-: ARRAY OF CHAR; VAR s: String);
  VAR l,i,j: LONGINT; o: String;
BEGIN
  l:=LENGTH(a);
  IF s=NIL THEN
    NEW(o,l+1);
    COPY(a,o^);
  ELSE
    i:=LENGTH(s^);
    IF i+l+1>LEN(s^) THEN
      NEW(o,i+l+1); COPY(s^,o^);
    ELSE
      o:=s;
    END;
    j:=0;
    WHILE (j<l) DO o^[i]:=a[j]; INC(i); INC(j) END;
    o[i]:=0X;
  END;
  s:=o;
END Append;

PROCEDURE ToUpper*(VAR s: ARRAY OF CHAR);
  VAR i: LONGINT;
BEGIN
  i:=0;
  WHILE (i<LEN(s)) & (s[i]#0X) DO s[i]:=CAP(s[i]); INC(i) END;
END ToUpper;

PROCEDURE write(a: SYSTEM.ADDRESS; s-: ARRAY OF CHAR; len: LONGINT);
  VAR io: SIO; new: String; i: LONGINT;
BEGIN
  io:=SYSTEM.VAL(SIO,a);
  IF io.pos+len>=LEN(io.buf^) THEN
    NEW(new,io.pos+len+1);
    COPY(io.buf^,new^);
    io.buf:=new;
  END;
  i:=0;
  WHILE (i<len) DO io.buf[io.pos]:=s[i]; INC(i); INC(io.pos) END;
END write;

PROCEDURE Print*(VAR s: String; fmt-: ARRAY OF CHAR; SEQ x: SYSTEM.BYTE);
  VAR io: SIO;
BEGIN
  NEW(io);
  io.pos:=0;
  NEW(io.buf,32);
  FormOut.format(SYSTEM.VAL(SYSTEM.ADDRESS,io),write,fmt,FormOut.text,SYSTEM.ADR(x),SIZE(x));
  io.buf[io.pos]:=0X;
  s:=io.buf;
END Print;

END xlStrings.
