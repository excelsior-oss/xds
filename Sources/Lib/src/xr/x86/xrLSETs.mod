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
IMPLEMENTATION MODULE xrLSETs;

FROM SYSTEM    IMPORT  CODE, INT16, INT32, CARD16, CARD32;
IMPORT
  RTS:=xmRTS
	,SYSTEM
	,X2C
	,M2EXCEPTION
	;

CONST
  X2C_LSET_SIZE = X2C.X2C_LSET_SIZE;

TYPE
  pLSET = POINTER TO ARRAY [0..(MAX(SYSTEM.CARD16) DIV X2C_LSET_SIZE)-1] OF BITSET;

PROCEDURE [2] X2C_AND(res,a,b: RTS.LSET; length: CARD16): RTS.LSET;
  VAR c: RTS.LSET;
BEGIN
  c:=res;
  WHILE (length>0) DO
    c^:=a^*b^;
    c:=SYSTEM.ADDADR(c,SIZE(BITSET));
    a:=SYSTEM.ADDADR(a,SIZE(BITSET));
    b:=SYSTEM.ADDADR(b,SIZE(BITSET));
    DEC(length);
  END;
  RETURN res;
END X2C_AND;

PROCEDURE [2] X2C_OR (res,a,b: RTS.LSET; length: CARD16): RTS.LSET;
  VAR c: RTS.LSET;
BEGIN
  c:=res;
  WHILE (length>0) DO
    c^:=a^+b^;
    c:=SYSTEM.ADDADR(c,SIZE(BITSET));
    a:=SYSTEM.ADDADR(a,SIZE(BITSET));
    b:=SYSTEM.ADDADR(b,SIZE(BITSET));
    DEC(length);
  END;
  RETURN res;
END X2C_OR;

PROCEDURE [2] X2C_XOR(res,a,b: RTS.LSET; length: CARD16): RTS.LSET;
  VAR c: RTS.LSET;
BEGIN
  c:=res;
  WHILE (length>0) DO
    c^:=a^/b^;
    c:=SYSTEM.ADDADR(c,SIZE(BITSET));
    a:=SYSTEM.ADDADR(a,SIZE(BITSET));
    b:=SYSTEM.ADDADR(b,SIZE(BITSET));
    DEC(length);
  END;
  RETURN res;
END X2C_XOR;

PROCEDURE [2] X2C_BIC(res,a,b: RTS.LSET; length: CARD16): RTS.LSET;
  VAR c: RTS.LSET;
BEGIN
  c:=res;
  WHILE (length>0) DO
    c^:=a^-b^;
    c:=SYSTEM.ADDADR(c,SIZE(BITSET));
    a:=SYSTEM.ADDADR(a,SIZE(BITSET));
    b:=SYSTEM.ADDADR(b,SIZE(BITSET));
    DEC(length);
  END;
  RETURN res;
END X2C_BIC;

PROCEDURE [2] X2C_COMPLEMENT(res,a: RTS.LSET; length: CARD16): RTS.LSET;
  VAR c: RTS.LSET;
BEGIN
  c:=res;
  WHILE (length>0) DO
    c^:=-a^;
    c:=SYSTEM.ADDADR(c,SIZE(BITSET));
    a:=SYSTEM.ADDADR(a,SIZE(BITSET));
    DEC(length);
  END;
  RETURN res;
END X2C_COMPLEMENT;

PROCEDURE [2] X2C_SET_EQU(a,b: RTS.LSET; bits: CARD16): BOOLEAN;
  VAR mask: X2C.X2C_LSET_BASE;
BEGIN
  WHILE (bits>=X2C_LSET_SIZE) DO
    IF a^#b^ THEN RETURN FALSE END;
    a:=SYSTEM.ADDADR(a,SIZE(BITSET));
    b:=SYSTEM.ADDADR(b,SIZE(BITSET));
    bits:=bits-X2C_LSET_SIZE;
  END;
  IF bits=0 THEN RETURN TRUE END;
  mask:=SYSTEM.CAST(BITSET,SYSTEM.CAST(INTEGER,SYSTEM.ROT({1},SYSTEM.CAST(SYSTEM.INT16,bits)))-1);
  RETURN a^*mask=b^*mask;
END X2C_SET_EQU;

PROCEDURE [2] X2C_SET_LEQ(a,b: RTS.LSET; bits: CARD16): BOOLEAN;
  VAR mask: X2C.X2C_LSET_BASE;
BEGIN
  WHILE (bits>=X2C_LSET_SIZE) DO
    IF (a^*(-b^))#{} THEN RETURN FALSE END;
    a:=SYSTEM.ADDADR(a,SIZE(BITSET));
    b:=SYSTEM.ADDADR(b,SIZE(BITSET));
    bits:=bits-X2C_LSET_SIZE;
  END;
  IF bits=0 THEN RETURN TRUE END;
  mask:=SYSTEM.CAST(BITSET,SYSTEM.CAST(INTEGER,SYSTEM.ROT({1},SYSTEM.CAST(SYSTEM.INT16,bits)))-1);
  RETURN ((a^*mask) * (-(b^*mask))) = {};
END X2C_SET_LEQ;

PROCEDURE [2] X2C_INCL(set: RTS.LSET; i: CARD32; bits: CARD16): RTS.LSET;
BEGIN
  IF i>=bits THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  set:=SYSTEM.ADDADR(set,(i DIV X2C_LSET_SIZE)*SIZE(BITSET));
  INCL(set^,i MOD X2C_LSET_SIZE);
  RETURN set;
END X2C_INCL;

PROCEDURE [2] X2C_EXCL(set: RTS.LSET; i: CARD32; bits: CARD16): RTS.LSET;
BEGIN
  IF i>=bits THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  set:=SYSTEM.ADDADR(set,(i DIV X2C_LSET_SIZE)*SIZE(BITSET));
  EXCL(set^,i MOD X2C_LSET_SIZE);
  RETURN set;
END X2C_EXCL;

PROCEDURE [2] X2C_LONGSET(set: RTS.LSET; a,b: CARD32; bits: CARD16): RTS.LSET;
  VAR x: POINTER TO ARRAY [0..(MAX(SYSTEM.CARD16) DIV X2C_LSET_SIZE)-1] OF BITSET;
BEGIN
  IF (a>b) OR (a>=bits) OR (b>=bits) THEN
    RTS.X2C_TRAP_F(ORD(M2EXCEPTION.rangeException))
  END;
  x:=SYSTEM.CAST(SYSTEM.ADDRESS,set);
  WHILE a<b DO
    INCL(x^[a DIV X2C_LSET_SIZE],a MOD X2C_LSET_SIZE);
    INC(a)
  END;
  RETURN set;
END X2C_LONGSET;

PROCEDURE [2] X2C_ROTL(res: RTS.LSET; a: RTS.LSET; length: INT16; n: INT32): RTS.LSET;
	VAR i,j: INT16; x,xr: pLSET;
BEGIN
	xr:=SYSTEM.CAST(SYSTEM.ADDRESS,res);
	x:=SYSTEM.CAST(SYSTEM.ADDRESS,a);
  FOR i:=0 TO ((length+X2C_LSET_SIZE-1) DIV X2C_LSET_SIZE) - 1 DO xr^[i]:={}  END;
	i:=0;
  IF n>=0 THEN
		j:=VAL(INT16,n MOD length);
		WHILE i<length DO
			IF (i MOD X2C_LSET_SIZE) IN x^[i DIV X2C_LSET_SIZE] THEN
        INCL(xr^[((i+j) MOD length) DIV X2C_LSET_SIZE],((i+j) MOD length) MOD X2C_LSET_SIZE);
			END;
			INC(i);
		END;
	ELSE
    j:=VAL(INT16,(-n) MOD length);
    WHILE (i<length) DO
      IF (VAL(CARDINAL,(i+j) MOD length) MOD X2C_LSET_SIZE) IN x^[VAL(CARDINAL,(i+j) MOD length) DIV X2C_LSET_SIZE] THEN
        INCL(xr^[i DIV X2C_LSET_SIZE],i MOD X2C_LSET_SIZE);
      END;
			INC(i);
		END;
	END;
  RETURN res;
END X2C_ROTL;

PROCEDURE [2] X2C_LSHL(res: RTS.LSET; a: RTS.LSET; length: INT16; n: INT32): RTS.LSET;
	VAR i: INT32; x,xr: pLSET;
BEGIN
	xr:=SYSTEM.CAST(SYSTEM.ADDRESS,res);
	x:=SYSTEM.CAST(SYSTEM.ADDRESS,a);
  FOR i:=0 TO ((VAL(INT32,length)+X2C_LSET_SIZE-1) DIV X2C_LSET_SIZE) - 1 DO xr^[i]:={} END;
  IF n>=0 THEN
		i:=0;
		WHILE i+n<VAL(INT32,length) DO
			IF (i MOD X2C_LSET_SIZE) IN x^[i DIV X2C_LSET_SIZE] THEN
				INCL(xr^[(i+n) DIV X2C_LSET_SIZE],(i+n) MOD X2C_LSET_SIZE);
			END;
			INC(i);
		END;
	ELSE
		i:=-n;
		WHILE i<VAL(INT32,length) DO
			IF (i MOD X2C_LSET_SIZE) IN x^[i DIV X2C_LSET_SIZE] THEN
				INCL(xr^[(i+n) DIV X2C_LSET_SIZE],(i+n) MOD X2C_LSET_SIZE);
			END;
			INC(i);
		END;
	END;
  RETURN res;
END X2C_LSHL;

END xrLSETs.
