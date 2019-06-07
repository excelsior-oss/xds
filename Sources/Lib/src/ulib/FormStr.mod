(* Copyright (c) xTech 1992,95. All Rights Reserved *)
<*+ m2extensions *>
<*+ m2addtypes   *>
IMPLEMENTATION MODULE FormStr;

IMPORT  sys:=SYSTEM;
IMPORT  out:=FormOut;

(*---------------------------  FORMAT  --------------------------*)
(*                           ----------                          *)

TYPE
  Str = POINTER TO ARRAY [0..7FFFH] OF CHAR;
  desc_rec = RECORD
	       str: Str;
	       high: CARDINAL;
	       pos: CARDINAL;
	     END;
  desc_ptr = POINTER TO desc_rec;

PROCEDURE format_app(x: sys.ADDRESS; s-: ARRAY OF CHAR; l: INTEGER);
  VAR ptr: desc_ptr;
      h,p,i: CARDINAL;
BEGIN
  ptr:=desc_ptr(x);
  p:=ptr^.pos;
  i:=0;
  h:=p+VAL(CARDINAL,l);
  IF h>ptr^.high THEN h:=ptr^.high END;
  IF p<h THEN
    REPEAT ptr^.str^[p]:=s[i]; i:=i+1; p:=p+1 UNTIL p=h
  END;
  ptr^.pos:=p
END format_app;

(* Не пишите '-' у параметра f - будет плохо! *)

PROCEDURE print(VAR s: ARRAY OF CHAR; f: ARRAY OF CHAR;
                                      SEQ x: sys.BYTE);
  VAR r: desc_rec;
BEGIN
  r.pos:=0;
  r.high:=HIGH(s);
  r.str:=sys.CAST(Str,sys.ADR(s));
  out.format(sys.ADR(r),format_app,f,out.default,sys.ADR(x),SIZE(x));
  IF r.pos<=HIGH(s) THEN s[r.pos]:=0C END;
END print;

PROCEDURE append(VAR s: ARRAY OF CHAR; f: ARRAY OF CHAR;
                                       SEQ x: sys.BYTE);
  VAR r: desc_rec;
BEGIN
  r.pos:=LENGTH(s);
  r.high:=HIGH(s);
  r.str:=sys.CAST(Str,sys.ADR(s));
  out.format(sys.ADR(r),format_app,f,out.default,sys.ADR(x),SIZE(x));
  IF r.pos<=HIGH(s) THEN s[r.pos]:=0C END;
END append;

PROCEDURE image(VAR s: ARRAY OF CHAR; VAR pos: CARDINAL;
                   f: ARRAY OF CHAR; SEQ x  : sys.BYTE);
  VAR r: desc_rec;
BEGIN
  r.pos:=pos;
  r.high:=HIGH(s);
  r.str:=sys.CAST(Str,sys.ADR(s));
  out.format(sys.ADR(r),format_app,f,out.default,sys.ADR(x),SIZE(x));
  IF r.pos<=HIGH(s) THEN s[r.pos]:=0C END;
  pos:=r.pos;
END image;

(*-------------------------  NUMBERs  ---------------------------*)
(*                         -----------                           *)

PROCEDURE iscan(VAR num : INTEGER;
                    str-: ARRAY OF CHAR;
                VAR pos : CARDINAL;
                VAR done: BOOLEAN);

  CONST
    O=ORD('0');         A=ORD('A');
    B=ORD('B');         C=ORD('C');
    H=ORD('H');     Dovfl=MAX(INTEGER) DIV 10;

  TYPE
    S32 = SET OF [0..31];

  VAR Plus: BOOLEAN;

  PROCEDURE Fin(n: CARDINAL): BOOLEAN;
    VAR ch: CHAR;
  BEGIN
    IF n>HIGH(str) THEN RETURN TRUE END;
    ch:=CAP(str[n]);
    RETURN NOT ( ((ch>="0") & (ch<="9")) OR
                 ((ch>="A") & (ch<="F")) OR
                 (ch="H")
               )
  END Fin;

  PROCEDURE Sgn(x: INTEGER): INTEGER;
  BEGIN
    IF (x = MIN(INTEGER)) OR Plus THEN RETURN x ELSE RETURN -x END
  END Sgn;

  VAR c: CHAR;    ch, dig: INTEGER;
      Oval, Dval, Hval,t : INTEGER;
      Oct_, Dec_, o, d, h: BOOLEAN;
BEGIN
  done:=FALSE;
  IF pos > HIGH(str) THEN RETURN (*illegal*) END;
  WHILE (pos <= HIGH(str)) & (str[pos]=' ') DO INC(pos) END;
  IF (pos > HIGH(str)) OR (str[pos] = 0C) THEN RETURN (*illegal*) END;
  Plus:=(str[pos] # '-');
  IF NOT Plus OR (str[pos]='+') THEN INC(pos) END;
  IF Fin(pos) OR NOT ((ORD(str[pos])-O) IN {0..9}) & Fin(pos+1) THEN
    RETURN (*illegal*) (* no number *)
  END;
  Oval:=0; Dval:=0; Hval:=0;
  Oct_:=TRUE; Dec_:=TRUE;
  o:=TRUE; d:=TRUE; h:=TRUE;
  LOOP
    IF pos <= HIGH(str) THEN c:=str[pos];
      c:=CAP(c); ch:=ORD(c);
    ELSE ch:=0
    END;
    IF    (ch-O) IN {0..7} THEN dig:=ch-O;
    ELSIF (ch-O) IN {8..9} THEN dig:=ch-O;
      Oct_:=FALSE;
    ELSIF ((ch=B) OR (ch=C)) & Fin(pos+1) THEN
      num:=Sgn(Oval);
      IF o & Oct_ THEN INC(pos); done:=TRUE; RETURN
      ELSIF  Oct_ THEN RETURN (*overflow   *)
      ELSE             RETURN (*invalidbase*)
      END;
    ELSIF (ch-A) IN {0..5} THEN dig:=10+ch-A;
      Oct_:=FALSE; Dec_:=FALSE;
    ELSIF ch=H THEN
      num:=Sgn(Hval);
      IF h THEN INC(pos); done:=TRUE; RETURN ELSE RETURN (*overflow*) END;
    ELSE
      num:=Sgn(Dval);
      IF d & Dec_ THEN done:=TRUE; RETURN
      ELSIF  Dec_ THEN RETURN (*overflow   *)
      ELSE             RETURN (*invalidbase*)
      END;
    END;
    IF h THEN
      h:=(S32(Hval)*S32{28..31} = S32{});
      IF h THEN Hval:=sys.CAST(INTEGER,BITSET(ASH(Hval,4))+BITSET(dig)) END
    END;
    IF d & Dec_ THEN
      IF dig=9 THEN t:=1 ELSE t:=0 END;
      IF    (Dval=Dovfl) & (dig=8)        THEN Dval:=MIN(INTEGER);
      ELSIF (Dval+t) > Dovfl THEN d:=FALSE
      ELSE   Dval:=Dval*10+VAL(INTEGER,dig)
      END
    END;
    IF o & Oct_ THEN
      o:=(S32(Oval)*S32{29..31} = S32{});
      IF o THEN Oval:=sys.CAST(INTEGER,BITSET(ASH(Oval,3))+BITSET(dig)) END
    END;
    INC(pos);
  END;
END iscan;

END FormStr.
