(* Copyright (c) xTech 1992,95.  All Rights Reserved *)
(* IF DEF NOFLOAT *)
<*+ m2extensions *>
<*+ m2addtypes   *>
IMPLEMENTATION MODULE FormOut; (* Sem  07-Aug-88. *)
			       (* Leo  03-Nov-88. *)
			       (* Hady 28-Jul-89. *)
			       (* Leg  06-Feb-92. *)
			       (* Ned  08-Apr-94. *)
                               (* Ned  11-Feb-95. *)
                               (* Sem  15-Mar-95. *)

IMPORT  SYSTEM;

<* IF NOT NOFLOAT THEN *> IMPORT  XReal, Strings; <* END *>

CONST
  CAP_DIG = "0123456789ABCDEF";
  SML_DIG = "0123456789abcdef";
  MAX_DIG = 14;

TYPE
  StrPtr = POINTER TO ARRAY [0..7FFFH] OF CHAR;
  Flags  = SET OF (lj,sg,zr,bs,sp,cap,w1,w2,w3,cn,lng,ign);
  Buffer = RECORD
    wd1,wd2,wd3: LONGINT;
    flags      : Flags;
    buf        : ARRAY [0..255] OF CHAR;
    cnt,ptr    : LONGINT;
  END;
  LineSep = ARRAY [0..3] OF CHAR;

VAR
  defaultNL : LineSep;
  defaultTL : LineSep;

(* --------------------- STRINGs --------------------- *)

PROCEDURE appString(VAR b: Buffer; str: StrPtr; high: LONGINT);
  VAR i: LONGINT;
BEGIN
  b.ptr:=0; b.cnt:=0; i:=0;
<*$< CHECKINDEX- *>
  IF w3 IN b.flags THEN
    WHILE (i<=high) & (str^[i]#0C) & (b.wd3>0) DO INC(i); DEC(b.wd3) END;
    IF (i>high) OR (str^[i]=0C) THEN RETURN END;
  END;
  IF w2 IN b.flags THEN
    WHILE (b.cnt<=HIGH(b.buf)) & (i<=high) & (str^[i]#0C) & (b.wd2>0) DO
      b.buf[b.cnt]:=str^[i]; INC(b.cnt); INC(i); DEC(b.wd2);
    END;
  ELSE
    WHILE (b.cnt<=HIGH(b.buf)) & (i<=high) & (str^[i]#0C) DO
      b.buf[b.cnt]:=str^[i]; INC(b.cnt); INC(i);
    END;
  END;
<*$>*>
END appString;

PROCEDURE appChar(VAR b: Buffer; ch: CHAR);
BEGIN
  b.ptr:=0; b.cnt:=0;
  IF NOT (w2 IN b.flags) THEN b.wd2:=1 END;
  WHILE (b.wd2>0) & (b.cnt < HIGH(b.buf)) DO
    b.buf[b.cnt]:=ch; INC(b.cnt); DEC(b.wd2)
  END;
END appChar;

(* --------------------- SETs --------------------- *)

PROCEDURE appSet(VAR b: Buffer; set: BITSET);

  PROCEDURE appNum(n: LONGINT);
  BEGIN
    IF n DIV 10 > 0 THEN appNum(n DIV 10) END;
    b.buf[b.cnt]:=CHR(ORD(n MOD 10) + ORD('0')); INC(b.cnt);
  END appNum;

  VAR i,Cou: LONGINT; Emp: BOOLEAN;
BEGIN
  Cou:=0;  Emp:=TRUE;
  b.cnt:=1; b.ptr:=0; b.buf[0]:='{';
  i:=0;
  REPEAT
    IF i IN set THEN INC(Cou)
    ELSIF Cou>0 THEN
      IF NOT Emp THEN b.buf[b.cnt]:=','; INC(b.cnt) END;
      IF Cou=1 THEN
        appNum(i-1);
      ELSE
        appNum(i-Cou);
        b.buf[b.cnt]:='.'; INC(b.cnt); b.buf[b.cnt]:='.'; INC(b.cnt);
        appNum(i-1);
      END;
      Cou:=0; Emp:=FALSE;
    END;
    i:=i+1;
  UNTIL i > MAX(BITSET)+1;
  b.buf[b.cnt]:='}'; INC(b.cnt);
END appSet;

(* --------------------- WHOLEs --------------------- *)

PROCEDURE appInt(VAR b: Buffer; int: LONGINT);

  PROCEDURE append_string(s-: ARRAY OF CHAR);
    VAR i: INTEGER;
  BEGIN
    i:=LENGTH(s)-1;
    WHILE (b.wd2 > 0) OR (i >= 0) DO
      DEC(b.ptr);
      IF i < 0 THEN b.buf[b.ptr]:='0' ELSE b.buf[b.ptr]:=s[i] END;
      DEC(b.wd2); DEC(i)
    END
  END append_string;

  CONST min_int = "2147483648";
  VAR j: LONGINT; sig: BOOLEAN;
BEGIN
  sig:=int < 0;
  b.cnt:=HIGH(b.buf)+1;
  b.ptr:=b.cnt;
  IF NOT (w2 IN b.flags) THEN b.wd2:=1 END;
  IF b.ptr-b.wd2 < 1     THEN b.wd2:=b.ptr-1 END;
  IF int = MIN(LONGINT) THEN append_string(min_int)
  ELSE
    IF sig THEN int:=ABS(int) END;
    WHILE (int # 0) OR (b.wd2>0) DO
      j:=ABS(int MOD 10);
      DEC(b.ptr); b.buf[b.ptr]:=CHR(j+INT('0'));
      int:=int DIV 10; DEC(b.wd2);
    END
  END;
  IF (zr IN b.flags) & (w1 IN b.flags) THEN j:=b.cnt-b.wd1;
    IF j<0 THEN j:=0 END;
    IF ( Flags{sp,sg}*b.flags # Flags{} ) OR sig THEN INC(j) END;
    WHILE j<b.ptr DO DEC(b.ptr); b.buf[b.ptr]:='0' END;
  END;
  IF sp IN b.flags THEN
    DEC(b.ptr);
    IF sig THEN b.buf[b.ptr]:='-' ELSE b.buf[b.ptr]:=' ' END
  ELSIF sg IN b.flags THEN
    DEC(b.ptr);
    IF sig THEN b.buf[b.ptr]:='-' ELSE b.buf[b.ptr]:='+' END
  ELSIF sig THEN
    DEC(b.ptr); b.buf[b.ptr]:='-'
  END
END appInt;

PROCEDURE appCar(VAR b: Buffer; card: LONGCARD; n: CARDINAL; base: CHAR);
  VAR j: LONGINT;
    dig: ARRAY [0..16] OF CHAR;
BEGIN
  IF cap IN b.flags THEN dig:=CAP_DIG ELSE dig:=SML_DIG END;
  j:=1;
  b.cnt:=HIGH(b.buf)+1;
  b.ptr:=b.cnt;
  IF (bs IN b.flags) & (base # 0C) THEN DEC(b.ptr); b.buf[b.ptr]:=base END;
  IF NOT (w2 IN b.flags) THEN b.wd2:=1 END;
  IF b.ptr-b.wd2 < 1 THEN b.wd2:=b.ptr-1 END;
  WHILE (b.wd2>0) OR (card#0) DO
    DEC(b.ptr);
    b.buf[b.ptr]:=dig[ ORD(card MOD n) ];
    card:=card DIV n;
    DEC(b.wd2);
  END;
  IF (zr IN b.flags) & (w1 IN b.flags) THEN
    j:=b.cnt-b.wd1;
    IF j<0 THEN j:=0 END;
    WHILE j<b.ptr DO DEC(b.ptr); b.buf[b.ptr]:='0' END;
  END;
END appCar;

(* --------------------- FLOATs --------------------- *)

<* IF NOFLOAT THEN *>

PROCEDURE appNoFloat(VAR b: Buffer; s-: ARRAY OF CHAR);
BEGIN
  b.ptr:=0; b.cnt:=0;
  WHILE (b.cnt<LEN(s)) & (s[b.cnt]#0C) DO
    b.buf[b.cnt]:=s[b.cnt]; INC(b.cnt);
  END;
END appNoFloat;

<* ELSE *>
PROCEDURE float_sig(s-: XReal.STR; VAR b: Buffer);
BEGIN
  IF s[0]='-' THEN RETURN END;
  IF sg IN b.flags THEN b.buf[0]:="+"; b.cnt:=1;
  ELSIF sp IN b.flags THEN b.buf[0]:=" "; b.cnt:=1;
  END;
END float_sig;
<* END *>

PROCEDURE appFloatF(VAR b: Buffer; real_val: LONGREAL);
<* IF NOFLOAT THEN *>
BEGIN
  appNoFloat(b,"0.0");
<* ELSE *>
  VAR s: XReal.STR; place,i: INTEGER;
BEGIN
  b.ptr:=0; b.cnt:=0;
  IF NOT (w2 IN b.flags) THEN place:=6 ELSE place:=b.wd2 END;
  XReal.to_fixed(real_val,place,MAX_DIG,s);
  float_sig(s,b);
  i:=0;
  WHILE s[i]#0C DO b.buf[b.cnt]:=s[i]; INC(i); INC(b.cnt) END;
  IF NOT (bs IN b.flags) THEN
    WHILE b.buf[b.cnt-1]='0' DO DEC(b.cnt) END;
    IF b.buf[b.cnt-1]='.' THEN DEC(b.cnt) END;
  END;
<* END *>
END appFloatF;

PROCEDURE appFloatE(VAR b: Buffer; real_val: LONGREAL);
<* IF NOFLOAT THEN *>
BEGIN
  appNoFloat(b,"0.0");
<* ELSE *>
  VAR s: XReal.STR; exp: CHAR; i: INTEGER;
BEGIN
  b.ptr:=0; b.cnt:=0;
  IF NOT (w2 IN b.flags) THEN b.wd2:=6 END;
  IF cap IN b.flags THEN exp:='E' ELSE exp:='e' END;
  XReal.to_float(real_val,b.wd2+1,1,MAX_DIG,exp,bs IN b.flags,TRUE,s);
  float_sig(s,b);
  i:=0;
  WHILE s[i]#0C DO b.buf[b.cnt]:=s[i]; INC(i); INC(b.cnt) END;
<* END *>
END appFloatE;

PROCEDURE appFloatG(VAR b: Buffer; real_val: LONGREAL);
<* IF NOFLOAT THEN *>
BEGIN
  appNoFloat(b,"0.0");
<* ELSE *>
  VAR r: LONGREAL; e: LONGINT;
BEGIN
  b.cnt:=0;
  r:=ABS(real_val); e:=0;
  WHILE (r>=10.) & (r/10.#r) DO INC(e); r:=r/10. END;
  WHILE (r<1.0) & (r*10.#r) DO DEC(e); r:=r*10. END;
  IF (e<-4) OR (e>=b.wd2) THEN
    DEC(b.wd2); appFloatE(b,real_val);
  ELSE
    b.wd2:=b.wd2-e-1; appFloatF(b,real_val);
  END;
<* END *>
END appFloatG;

(* --------------------- SCAN FORMAT --------------------- *)

PROCEDURE format(p: SYSTEM.ADDRESS;
	     write: write_proc;
              fmt-: ARRAY OF CHAR;
           linesep: CHAR;
	       xxx: SYSTEM.ADDRESS;
              size: CARDINAL);

  TYPE
    Arg  = RECORD
             CASE : INTEGER OF
               |0: card: SYSTEM.CARD32;
               |1: int : SYSTEM.INT32;
               |2: adr : SYSTEM.ADDRESS;
               |3: set : BITSET;
             END;
           END;

    Args = POINTER TO ARRAY [0..1FFFH] OF Arg;

  CONST max=512;

  VAR args       : Args;
      arglen     : LONGINT;
      acnt       : LONGINT;
      out        : ARRAY [0..max] OF CHAR;
      ocnt,fcnt  : LONGINT;
      ch         : CHAR;

  PROCEDURE get_arg(VAR a: Arg);
  BEGIN
    a:=args^[acnt];
    INC(acnt);
  END get_arg;

  PROCEDURE get_arg_int(): SYSTEM.INT32;
  BEGIN
    INC(acnt); RETURN args^[acnt-1].int;
  END get_arg_int;

  PROCEDURE appStr(VAR b: Buffer; val: Arg): BOOLEAN;
    VAR str: StrPtr; high,i: LONGINT;
  BEGIN
    b.cnt:=0; b.ptr:=0;
    (* checking that 2nd word for string = 0 *)
    IF (acnt>=arglen) OR (get_arg_int()#0) THEN RETURN TRUE END;
    IF acnt>=arglen THEN RETURN TRUE END;
    high:=get_arg_int();
    IF high <= 0 THEN RETURN FALSE END;
    str:=val.adr;
    IF str = NIL THEN RETURN FALSE END;
    IF ign IN b.flags THEN RETURN FALSE END;
<*$< CHECKINDEX- *>
    IF Flags{w1,w3}*b.flags=Flags{} THEN
      IF (w2 IN b.flags) & (b.wd2 <= high) THEN
	IF b.wd2<=0 THEN RETURN FALSE END;
	high:=b.wd2-1;
      END;
      i:=0;
      LOOP
	IF i>high THEN EXIT END;
	IF str^[i]=0C THEN
	  IF i=0 THEN RETURN FALSE END;
	  high:=i-1; EXIT;
	END;
	INC(i);
      END;
      IF ocnt>0 THEN write(p,out,ocnt); ocnt:=0 END;
      write(p,str^,high+1);
      RETURN FALSE
    END;
<*$>*>
    appString(b,str,high);
    RETURN FALSE;
  END appStr;

  PROCEDURE Next;
  BEGIN
    IF fcnt<LEN(fmt) THEN ch:=fmt[fcnt]; INC(fcnt) ELSE ch:=0C END;
  END Next;

  PROCEDURE width(VAR b: Buffer; n: LONGINT);
  BEGIN
    IF    w3 IN b.flags THEN b.wd3:=n
    ELSIF w2 IN b.flags THEN b.wd2:=n
    ELSE INCL(b.flags,w1);   b.wd1:=n
    END;
  END width;

  PROCEDURE scan_format(VAR b: Buffer; VAR base: CHAR);
    VAR n: LONGINT; done: BOOLEAN;
  BEGIN
    b.flags:=Flags{};
    b.wd1:=-1; b.wd2:=-1; b.wd3:=-1;
    done:=FALSE;
    REPEAT
      CASE ch OF
        |'#': INCL(b.flags,bs);            |'+': INCL(b.flags,sg);
        |'$': INCL(b.flags,zr);            |' ': INCL(b.flags,sp);
        |'-': INCL(b.flags,lj);            |'L': INCL(b.flags,lng);
        |'|': INCL(b.flags,cn);            |'~': INCL(b.flags,ign);
        |'.': IF w2 IN b.flags THEN INCL(b.flags,w3)
              ELSE INCL(b.flags,w2);
              END;
        |'*': IF acnt<=arglen-1 THEN
                width(b,get_arg_int());
              END;
        |'0'..'9':
              n:=ORD(ch)-ORD('0'); Next;
              IF (n=0) & ('0'<=ch) & (ch<='9') & NOT (w2 IN b.flags) THEN
                INCL(b.flags,zr)
              END;
              WHILE ('0'<=ch) & (ch<='9') DO
                n:=n*10+INT(ch)-INT('0'); Next
              END;
              width(b,n); fcnt:=fcnt-1;
        |'l','h':
      ELSE  base:=ch; done:=TRUE
      END;
      IF fcnt<LEN(fmt) THEN ch:=fmt[fcnt]; INC(fcnt) ELSE ch:=0C END
    UNTIL done;
    IF b.wd1<0 THEN EXCL(b.flags,w1); b.wd1:=0 END;
    IF b.wd2<0 THEN EXCL(b.flags,w2); b.wd2:=0 END;
    IF b.wd3<0 THEN EXCL(b.flags,w3); b.wd3:=0 END;
  END scan_format;

  PROCEDURE write_buf(VAR b: Buffer);
  BEGIN
    WHILE b.ptr<b.cnt DO
      IF ocnt>=max THEN write(p,out,max); ocnt:=0 END;
      out[ocnt]:=b.buf[b.ptr]; INC(ocnt); INC(b.ptr);
    END;
  END write_buf;

  PROCEDURE left_justify(VAR b: Buffer; spaces: LONGINT);
  BEGIN
    write_buf(b);
    WHILE spaces>0 DO
      IF ocnt>=max THEN write(p,out,max); ocnt:=0 END;
      out[ocnt]:=' '; INC(ocnt); DEC(spaces)
    END;
  END left_justify;

  PROCEDURE right_justify(VAR b: Buffer; spaces: LONGINT);
  BEGIN
    WHILE spaces>0 DO
      IF ocnt>=max THEN write(p,out,max); ocnt:=0 END;
      out[ocnt]:=' '; INC(ocnt); DEC(spaces);
    END;
    write_buf(b);
  END right_justify;

  PROCEDURE center(VAR b: Buffer; spaces: LONGINT);
    VAR tail: LONGINT;
  BEGIN
    tail:=spaces DIV 2; spaces:=spaces-tail;
    WHILE spaces>0 DO
      IF ocnt>=max THEN write(p,out,max); ocnt:=0 END;
      out[ocnt]:=' '; INC(ocnt); DEC(spaces);
    END;
    write_buf(b);
    WHILE tail>0 DO
      IF ocnt>=max THEN write(p,out,max); ocnt:=0 END;
      out[ocnt]:=' '; INC(ocnt); DEC(tail);
    END;
  END center;

  PROCEDURE get_real(VAR real_val: LONGREAL): BOOLEAN;
  BEGIN
    IF acnt >= arglen THEN RETURN TRUE END;
    INC(acnt);
    SYSTEM.GET(SYSTEM.ADR(args^[acnt-2]),real_val);
    RETURN FALSE
  END get_real;

  PROCEDURE SubFormat(): BOOLEAN;
    VAR b: Buffer; spaces: LONGINT; real_val: LONGREAL; base: CHAR;
      val: Arg;
  BEGIN
    scan_format(b,base);
    IF acnt >= arglen THEN RETURN TRUE END;
    get_arg(val);
    IF (base>='A') & (base<='Z') THEN INCL(b.flags,cap) END;
    CASE base OF
     |'i','d': IF ~(ign IN b.flags) THEN appInt(b,val.int) END;
     |'x','X': IF ~(ign IN b.flags) THEN appCar(b,val.card,16,'H'); END;
     |'o': IF ~(ign IN b.flags) THEN appCar(b,val.card,8,'B'); END;
     |'b': IF ~(ign IN b.flags) THEN appCar(b,val.card,2,0C); END;
     |'u': IF ~(ign IN b.flags) THEN appCar(b,val.card,10,0C); END;
     |'f','F': IF get_real(real_val) THEN RETURN TRUE END;
               IF ~(ign IN b.flags) THEN appFloatF(b,real_val); END;
     |'e','E': IF get_real(real_val) THEN RETURN TRUE END;
	       IF ~(ign IN b.flags) THEN appFloatE(b,real_val); END;
     |'g','G': IF get_real(real_val) THEN RETURN TRUE END;
               IF ~(ign IN b.flags) THEN appFloatG(b,real_val); END;
     |'s': IF appStr(b,val) THEN RETURN TRUE END;
     |'c': IF ORD(MAX(CHAR))<val.card THEN RETURN TRUE END;
           IF ~(ign IN b.flags) THEN appChar(b,CHR(val.card)); END;
     |'{': IF ch='}' THEN Next;
             IF ~(ign IN b.flags) THEN appSet(b,val.set) END;
           ELSE DEC(acnt); RETURN TRUE
           END;
    ELSE (* illegal base, unget argument *)
      DEC(acnt); RETURN TRUE;
    END;
    IF ign IN b.flags THEN RETURN FALSE END;
    ASSERT(b.cnt<=HIGH(b.buf)+1);
    ASSERT(b.ptr>=0);
    IF    w1 IN b.flags THEN
      spaces:=b.wd1-b.cnt+b.ptr;
      IF    lj IN b.flags THEN left_justify(b,spaces)
      ELSIF cn IN b.flags THEN center(b,spaces)
      ELSE                     right_justify(b,spaces)
      END;
    ELSIF b.cnt>b.ptr THEN
      write_buf(b);
    END;
    RETURN FALSE;
  END SubFormat;

  PROCEDURE Format(): BOOLEAN;
    VAR f,a: LONGINT;
  BEGIN
    f:=fcnt; a:=acnt;
    IF SubFormat() THEN fcnt:=f; acnt:=a; ch:=fmt[fcnt-1]; RETURN TRUE END;
    RETURN FALSE;
  END Format;

  PROCEDURE write_NL(nl-: ARRAY OF CHAR): CHAR;
    VAR i: INTEGER;
  BEGIN
    i:=0;
    WHILE nl[i+1]#0C DO
      out[ocnt]:=nl[i]; INC(i); INC(ocnt);
      IF ocnt >= max-1 THEN write(p,out,ocnt); ocnt:=0 END;
    END;
    RETURN nl[i];
  END write_NL;

BEGIN
  ASSERT(SIZE(SYSTEM.INT32) = SIZE(SYSTEM.CARD32));
  ASSERT(SIZE(LONGREAL) <= SIZE(Arg)*2);
  args:=SYSTEM.CAST(Args,xxx); arglen:=size DIV SIZE(Arg);
  ch:=fmt[0]; fcnt:=1; ocnt:=0; acnt:=0;
  LOOP
    CASE ch OF
    |'\':
      IF fcnt<LEN(fmt) THEN ch:=fmt[fcnt]; INC(fcnt) ELSE ch:=0C END;
      IF ocnt >= max-1 THEN write(p,out,ocnt); ocnt:=0 END;
      IF ch='n' THEN
        IF linesep = default THEN ch:=write_NL(defaultNL);
        ELSIF linesep = text THEN ch:=write_NL(defaultTL);
        ELSIF linesep = crlf THEN out[ocnt]:=15C; INC(ocnt); ch:=12C;
        ELSE ch:=linesep;
        END;
      ELSIF  ch='r' THEN ch:=15C;
      ELSIF  ch='l' THEN ch:=12C;
      ELSIF  ch='f' THEN ch:=14C;
      ELSIF  ch='t' THEN ch:=11C;
      ELSIF  ch='e' THEN ch:=33C;
      ELSIF  ch='g' THEN ch:=07C;
      ELSIF  ch='\' THEN ch:='\';
      ELSIF  ch=0C  THEN
	out[ocnt]:='\'; INC(ocnt); EXIT;
      ELSE
	out[ocnt]:='\'; INC(ocnt);
      END;
      out[ocnt]:=ch; INC(ocnt);
      Next;
    |'%':
      IF fcnt<LEN(fmt) THEN ch:=fmt[fcnt]; INC(fcnt) ELSE ch:=0C END;
      IF ocnt >=max THEN write(p,out,max); ocnt:=0 END;
      IF ch=0C THEN
        out[ocnt]:='%'; INC(ocnt); EXIT;
      ELSIF ch='%' THEN
        out[ocnt]:='%'; INC(ocnt); Next;
      ELSIF Format() THEN
        out[ocnt]:='%'; INC(ocnt);
      END;
    |0C:
      EXIT;
    ELSE
      IF ocnt>=max THEN write(p,out,max); ocnt:=0 END;
      out[ocnt]:=ch; INC(ocnt);
      IF fcnt>=LEN(fmt) THEN EXIT END;
      ch:=fmt[fcnt]; INC(fcnt);
    END;
  END;
  IF ocnt > 0 THEN write(p,out,ocnt) END;
END format;

PROCEDURE LineSeparator(nl-: ARRAY OF CHAR);
BEGIN
  COPY(nl,defaultNL);
END LineSeparator;

PROCEDURE TextSeparator(nl-: ARRAY OF CHAR);
BEGIN
  COPY(nl,defaultTL);
END TextSeparator;

BEGIN
  defaultNL[0]:=12C;
  defaultNL[1]:=0C;
  defaultTL:=defaultNL;
END FormOut.
