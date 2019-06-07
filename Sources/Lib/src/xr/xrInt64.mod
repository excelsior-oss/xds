(* Copyright (c) xTech 1992,96.  All Rights Reserved *)
<*- checkindex  *>
<*- checkdindex *>
<*- checknil    *>
<*- checkset    *>
<*- checkrange  *>
<*- ioverflow   *>
<*- coverflow   *>
<*- foverflow   *>
<* +M2EXTENSIONS*>
IMPLEMENTATION MODULE xrInt64; (* Hady. 21.06.96 10:20 *)

IMPORT  SYSTEM;

TYPE
  CARD16 = SYSTEM.CARD16;
  Arr = ARRAY [0..3] OF CARD32;

CONST M = 10000H;
      S =  7FFFH;

PROCEDURE ["C"] X2C_INTTO64(VAR res: X2C_int64; val: INT32);
BEGIN
  IF val<0 THEN
    res.high:=0FFFFFFFFH;
  ELSE
    res.high:=0;
  END;
  res.low:=SYSTEM.CAST(CARD32,val);
END X2C_INTTO64;

PROCEDURE ["C"] X2C_CARDTO64(VAR res: X2C_int64; val: CARD32);
BEGIN
  res.high:=0; res.low:=val;
END X2C_CARDTO64;

PROCEDURE ["C"] X2C_IsNeg64(a: X2C_int64): BOOLEAN;
BEGIN
  RETURN a.high>VAL(CARD32,MAX(INT32));
END X2C_IsNeg64;

PROCEDURE toArr(VAR a: Arr; val-: X2C_int64);
BEGIN
  a[0]:=val.low MOD M;
  a[1]:=val.low DIV M;
  a[2]:=val.high MOD M;
  a[3]:=val.high DIV M;
END toArr;

PROCEDURE to64(VAR x: X2C_int64; a-: Arr);
BEGIN
  x.low:=a[1]*M+a[0];
  x.high:=a[3]*M+a[2];
END to64;

PROCEDURE neg(VAR a: Arr);
  VAR i: CARDINAL; r: CARD32;
BEGIN
  FOR i:=0 TO 3 DO a[i]:=M-a[i]-1 END;
  r:=1; i:=0;
  WHILE (i<4) & (r#0) DO
    r:=r+a[i]; a[i]:=r MOD M; r:=r DIV M;
    INC(i);
  END;
END neg;

PROCEDURE add(VAR res: Arr; a-,b-: Arr): BOOLEAN;
  VAR i: CARDINAL; r: CARD32;
BEGIN
  r:=0;
  FOR i:=0 TO 3 DO
    r:=a[i]+b[i]+r; res[i]:=r MOD M; r:=r DIV M;
  END;
  RETURN r#0;
END add;

PROCEDURE mul(VAR res: Arr; a,b: CARD32);
  VAR r: CARD32; a0,a1,b0,b1: CARD32;
BEGIN
  a0:=a MOD M; a1:=a DIV M;
  b0:=b MOD M; b1:=b DIV M;
  r:=a0*b0;
  res[0]:=r MOD M; r:=r DIV M;
  r:=r+(a0*b1+b0*a1);
  res[1]:=r MOD M; r:=r DIV M;
  r:=r+a1*b1;
  res[2]:=r MOD M;
  res[3]:=r DIV M;
END mul;

PROCEDURE ["C"] X2C_UnMinus64(VAR res: X2C_int64; x: X2C_int64): BOOLEAN;
  VAR a: Arr; s: BOOLEAN;
BEGIN
  toArr(a,x);
  s:=(a[3]>S);
  neg(a);
  IF (s=(a[3]>S)) & ((x.high#0) OR (x.low#0)) THEN RETURN TRUE END;
  to64(res,a);
  RETURN FALSE
END X2C_UnMinus64;

PROCEDURE ["C"] X2C_ADD64(VAR res: X2C_int64; A,B: X2C_int64): BOOLEAN;
  VAR a,b,r: Arr; sa,sb,cr: BOOLEAN;
BEGIN
  toArr(a,A); toArr(b,B);
  sa:=a[3]>S; sb:=b[3]>S;
  cr:=add(r,a,b);
  to64(res,r);
  RETURN (sa=sb)&((r[3]>S)#sa);
END X2C_ADD64;

PROCEDURE ["C"] X2C_64TOINT(VAR res: INT32; x: X2C_int64): BOOLEAN;
BEGIN
  res:=SYSTEM.CAST(INT32,x.low);
  RETURN
    ((x.high#0) OR (x.low>=80000000H)) &
    ((x.high#0FFFFFFFFH) OR (x.low<80000000H));
END X2C_64TOINT;

PROCEDURE ["C"] X2C_64TOCARD(VAR res: CARD32; x: X2C_int64): BOOLEAN;
BEGIN
  res:=x.low; RETURN (x.high#0)
END X2C_64TOCARD;

PROCEDURE ["C"] X2C_MUL64(VAR res: X2C_int64; a: INT32; b: CARD32);
  VAR x: CARD32; sig: BOOLEAN; ra: Arr;
BEGIN
  sig:=a<0;
  IF sig THEN
    IF a=MIN(INT32) THEN x:=80000000H;
    ELSE x:=SYSTEM.CAST(CARD32,-a)
    END;
  ELSE
    x:=SYSTEM.CAST(CARD32,a);
  END;
  mul(ra,x,b);
  IF sig THEN neg(ra) END;
  to64(res,ra);
END X2C_MUL64;

PROCEDURE ["C"] X2C_CMP64(a,b: X2C_int64): SYSTEM.int;
  VAR an,bn: BOOLEAN;
BEGIN
  an:=a.high>VAL(CARD32,MAX(INT32));
  bn:=b.high>VAL(CARD32,MAX(INT32));
  IF    an & NOT bn THEN (* a<0 & b>=0 *)
    RETURN -1
  ELSIF bn & NOT an THEN (* a>=0 & b<0 *)
    RETURN 1
  ELSIF a.high>b.high THEN RETURN 1
  ELSIF a.high<b.high THEN RETURN -1
  ELSIF a.low>b.low THEN RETURN 1
  ELSIF a.low<b.low THEN RETURN -1
  END;
  RETURN 0;
END X2C_CMP64;

END xrInt64.
