<*- checkindex  *>
<*- checkdindex *>
<*- checknil    *>
<*- checkset    *>
<*- checkrange  *>
<*- ioverflow   *>
<*- coverflow   *>
<*- foverflow   *>
<*- gendebug    *> (* Don enable! History would not work (SYSTEM.CODE) *)
<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE xrSETs;

FROM SYSTEM    IMPORT  INT32, INT16, SET32;
IMPORT  SYSTEM;

PROCEDURE ["C"] / X2C_doROTL(x: BITSET; bits: CARDINAL): BITSET;
PROCEDURE ["C"] / X2C_doROTR(x: BITSET; bits: CARDINAL): BITSET;
PROCEDURE ["C"] / X2C_doASHR(x: BITSET; bits: CARDINAL): BITSET;


PROCEDURE [2] X2C_ASH(a: INT32; b: INT32): INT32;
BEGIN
  IF b>=0 THEN
    RETURN SYSTEM.CAST(INT32,X2C_doROTL(SYSTEM.CAST(BITSET,a),b));
  END;
  RETURN SYSTEM.CAST(INT32,X2C_doASHR(SYSTEM.CAST(BITSET,a),-b));
END X2C_ASH;

PROCEDURE [2] X2C_ROT (a: SET32; length: INT16; n: INT32): SET32;
  VAR m: SET32;
BEGIN
  m:=SET32{};
  IF length = 32 THEN
    m:={0..31}
  ELSE
    INCL(m,length);
    m:=SYSTEM.CAST(SET32,SYSTEM.CAST(INT32,m)-1);
  END;
  a:=a*m;
  IF n >= 0 THEN
    n:=n MOD length;
    RETURN (X2C_doROTL(a,n)+X2C_doROTR(a,length-n)) * m;
  END;
  n:=(-n) MOD length;
  RETURN (X2C_doROTR(a,n)+X2C_doROTL(a,length-n)) * m;
END X2C_ROT;

PROCEDURE [2] X2C_LSH (a: SET32; length: INT16; n: INT32): SET32;
  VAR m: SET32;
BEGIN
  m:=SET32{};
  IF length=32 THEN
    m:={0..31};
  ELSE
    INCL(m,length);
    m:=SYSTEM.CAST(BITSET,SYSTEM.CAST(INT32,m)-1);
  END;
  a:=a*m;
  IF n > 0 THEN
    IF n>=length THEN RETURN SET32{} END;
    RETURN X2C_doROTL(a,n)*m;
  END;
  IF -n>=length THEN RETURN SET32{} END;
  RETURN X2C_doASHR(a,-n)*m;
END X2C_LSH;

END xrSETs.
