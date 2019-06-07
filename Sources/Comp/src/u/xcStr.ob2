(** Copyright (c) 1994,97 XDS Ltd, Russia. All Rights Reserved. *)
(** o2/m2 development system v2.0. *)
(** Concrete interface. Strings facilities *)
MODULE xcStr; (* Ned 04-Mar-94. *)

IMPORT  SYSTEM, FormOut, FormStr, DStrings;

TYPE
  STR    = POINTER [1] TO ARRAY 4000H OF CHAR;
  String = DStrings.String;
  desc_rec = RECORD
	       str : STR;
               dstr: String;
	       high: LONGINT;
	       pos : LONGINT;
	     END;

  desc_ptr = POINTER [1] TO desc_rec;

PROCEDURE [1] format_app(x: SYSTEM.ADDRESS; s-: ARRAY OF CHAR; l: LONGINT);
  VAR
    ptr: desc_ptr;
    h,p,i: LONGINT;
    n: String;
BEGIN
  ptr:=x;
  p:=ptr.pos;
  i:=0;
  h:=p+l;
  IF ptr.dstr#NIL THEN
    IF h>ptr.high THEN
      ptr.dstr[p]:=0X;
      NEW(n,h+32);
      COPY(ptr.dstr^,n^);
      ptr.dstr:=n;
      ptr.high:=LEN(n^)-1;
    END;
    IF p<h THEN
      REPEAT ptr.dstr[p]:=s[i]; INC(i); INC(p) UNTIL p=h
    END;
  ELSE
    IF h>ptr.high THEN h:=ptr.high END;
    IF p<h THEN
      REPEAT ptr.str[p]:=s[i]; INC(i); INC(p) UNTIL p=h
    END;
  END;
  ptr.pos:=p
END format_app;

PROCEDURE prn_txt*(VAR s: ARRAY OF CHAR; f: ARRAY OF CHAR; SEQ x: SYSTEM.BYTE);
  VAR r: desc_rec; a: SYSTEM.ADDRESS;
BEGIN
  a:=SYSTEM.ADR(s);
  r.pos :=0;
  r.high:=LEN(s)-1;
  r.str :=a;
  r.dstr:=NIL;
  FormOut.format(SYSTEM.ADR(r),format_app,f,FormOut.text,SYSTEM.ADR(x),SIZE(x));
  IF r.pos<=r.high THEN s[r.pos]:=0X END;
END prn_txt;

PROCEDURE prn_bin*(VAR s: ARRAY OF CHAR; f: ARRAY OF CHAR; SEQ x: SYSTEM.BYTE);
  VAR r: desc_rec; a: SYSTEM.ADDRESS;
BEGIN
  a:=SYSTEM.ADR(s);
  r.pos :=0;
  r.high:=LEN(s)-1;
  r.str :=a;
  r.dstr:=NIL;
  FormOut.format(SYSTEM.ADR(r),format_app,f,FormOut.default,SYSTEM.ADR(x),SIZE(x));
  IF r.pos<=r.high THEN s[r.pos]:=0X END;
END prn_bin;

PROCEDURE dprn_txt*(VAR s: String; f: ARRAY OF CHAR; SEQ x: SYSTEM.BYTE);
  VAR r: desc_rec;
BEGIN
  NEW(s,32);
  r.pos:=0;
  r.high:=LEN(s^)-1;
  r.str:=NIL;
  r.dstr:=s;
  FormOut.format(SYSTEM.ADR(r),format_app,f,FormOut.text,SYSTEM.ADR(x),SIZE(x));
  s:=r.dstr;
  s[r.pos]:=0X;
END dprn_txt;

PROCEDURE dprn_bin*(VAR s: String; f: ARRAY OF CHAR; SEQ x: SYSTEM.BYTE);
  VAR r: desc_rec;
BEGIN
  NEW(s,32);
  r.pos:=0;
  r.high:=LEN(s^)-1;
  r.str:=NIL;
  r.dstr:=s;
  FormOut.format(SYSTEM.ADR(r),format_app,f,FormOut.default,SYSTEM.ADR(x),SIZE(x));
  s:=r.dstr;
  s[r.pos]:=0X;
END dprn_bin;

PROCEDURE StrToInt*(s-: ARRAY OF CHAR; VAR i: LONGINT): BOOLEAN;
  VAR done: BOOLEAN;
         p: SYSTEM.CARD;
BEGIN
  p:=0;
  FormStr.iscan(i,s,p,done);
  RETURN done
END StrToInt;

PROCEDURE Capitalize*(VAR s: ARRAY OF CHAR);
  VAR i: INTEGER;
BEGIN
  i:=0;
  WHILE (i < LEN(s)) & (s[i]#0X) DO
    s[i]:=CAP(s[i]); INC(i)
  END;
END Capitalize;

PROCEDURE Internalize*(VAR ss: ARRAY OF CHAR);
  VAR
    i,j : LONGINT;
    nl  : ARRAY 8 OF CHAR;
    ch  : CHAR;
BEGIN
  nl[0]:=0X;
  i:=0; j:=0;
  LOOP
    ch:=ss[i]; INC(i);
    IF ch='\' THEN
      ch:=ss[i]; INC(i);
      IF    ch='n' THEN
        IF nl[0]=0X THEN prn_bin(nl,"\n") END;
        IF nl[1]=0X THEN ch:=nl[0] ELSE ss[j]:=nl[0]; INC(j); ch:=nl[1] END;
      ELSIF ch='r' THEN ch:=15C;
      ELSIF ch='l' THEN ch:=12C;
      ELSIF ch='f' THEN ch:=14C;
      ELSIF ch='t' THEN ch:=11C;
      ELSIF ch='e' THEN ch:=33C;
      ELSIF ch='g' THEN ch:=07C;
      ELSIF ch='\' THEN ch:='\';
      ELSE ch:='\'; DEC(i);
      END;
    END;
    ss[j]:=ch; INC(j);
    IF ch=0X THEN EXIT END;
  END;
END Internalize;

END xcStr.
