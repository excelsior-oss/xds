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

PROCEDURE [2] X2C_AND(res,a,b: RTS.LSET; length: CARD16): RTS.LSET;
  VAR c: RTS.LSET;
BEGIN
  c:=res;
  CODE("while (length) { length--; *c++ = *a++ & *b++; }");
  RETURN res;
END X2C_AND;

PROCEDURE [2] X2C_OR (res,a,b: RTS.LSET; length: CARD16): RTS.LSET;
  VAR c: RTS.LSET;
BEGIN
  c:=res;
  CODE("while (length) { length--; *c++ = *a++ | *b++; }");
  RETURN res;
END X2C_OR;

PROCEDURE [2] X2C_XOR(res,a,b: RTS.LSET; length: CARD16): RTS.LSET;
  VAR c: RTS.LSET;
BEGIN
  c:=res;
  CODE("while (length) { length--; *c++ = *a++ ^ *b++; }");
  RETURN res;
END X2C_XOR;

PROCEDURE [2] X2C_BIC(res,a,b: RTS.LSET; length: CARD16): RTS.LSET;
  VAR c: RTS.LSET;
BEGIN
  c:=res;
  CODE("while (length) { length--; *c++ = *a++ & ~( *b++ ); }");
  RETURN res;
END X2C_BIC;

PROCEDURE [2] X2C_COMPLEMENT(res,a: RTS.LSET; length: CARD16): RTS.LSET;
  VAR c: RTS.LSET;
BEGIN
  c:=res;
  CODE("while (length) { length--; *c++ = ~( *a++ ); }");
  RETURN res;
END X2C_COMPLEMENT;

PROCEDURE [2] X2C_SET_EQU(a,b: RTS.LSET; bits: CARD16): BOOLEAN;
  VAR mask: X2C.X2C_LSET_BASE;
BEGIN
  CODE("while (bits >= X2C_LSET_SIZE) {");
  CODE("  if ( *a++ != *b++ ) return 0;");
  CODE("  bits -= X2C_LSET_SIZE;");
  CODE("}");
  CODE("if (bits == 0) return 1;");
  CODE("mask = (1<<bits) - 1;");
  CODE("return ( *a & mask ) == ( *b & mask );");
  RETURN FALSE;
END X2C_SET_EQU;

PROCEDURE [2] X2C_SET_LEQ(a,b: RTS.LSET; bits: CARD16): BOOLEAN;
  VAR mask: X2C.X2C_LSET_BASE;
BEGIN
  CODE("while (bits >= X2C_LSET_SIZE) {");
  CODE("  if (( *a++ & ~*b++ ) != 0) return 0;");
  CODE("  bits -= X2C_LSET_SIZE;");
  CODE("}");
  CODE("if (bits == 0) return 1;");
  CODE("mask = (1 << bits) - 1;");
  CODE("return ((( *a & mask ) & (~( *b & mask ))) == 0);");
  RETURN FALSE;
END X2C_SET_LEQ;

PROCEDURE [2] X2C_INCL(set: RTS.LSET; i: CARD32; bits: CARD16): RTS.LSET;
BEGIN
  IF i>=bits THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  CODE("set[(int)i/X2C_LSET_SIZE] |= 1L << ((int)i%X2C_LSET_SIZE);");
  RETURN set;
END X2C_INCL;

PROCEDURE [2] X2C_EXCL(set: RTS.LSET; i: CARD32; bits: CARD16): RTS.LSET;
BEGIN
  IF i>=bits THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  CODE("set[(int)i/X2C_LSET_SIZE] &= ~(1L << ((int)i%X2C_LSET_SIZE));");
  RETURN set;
END X2C_EXCL;

PROCEDURE [2] X2C_LONGSET(set: RTS.LSET; a,b: CARD32; bits: CARD16): RTS.LSET;
BEGIN
  IF (a>b) OR (a>=bits) OR (b>=bits) THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  CODE("while (a <= b) { set[(int)a/X2C_LSET_SIZE] |= 1L << ((int)a%X2C_LSET_SIZE); ++a; }");
  RETURN set;
END X2C_LONGSET;

PROCEDURE [2] X2C_INL(i: CARD32; bits: CARD16; set: RTS.LSET): BOOLEAN;
BEGIN
  IF i<bits THEN
    CODE("return (set[(int)i/X2C_LSET_SIZE] & (1L << ((int)i%X2C_LSET_SIZE))) != 0;");
  END;
  RETURN FALSE;
END X2C_INL;

PROCEDURE [2] X2C_ROTL(res: RTS.LSET; a: RTS.LSET; length: INT16; n: INT32): RTS.LSET;
  VAR i[][2],j[][2]: INT16;
BEGIN
  i:=0; j:=0;
  CODE("memset(res,0,(length+X2C_LSET_SIZE-1)/X2C_LSET_SIZE*(X2C_LSET_SIZE/8));");
  IF n >= 0 THEN
    CODE("j=(short)(n % length);");
    CODE("for (i=0; i<length; i++)");
    CODE("  if (a[i/X2C_LSET_SIZE] & (1<<(i%X2C_LSET_SIZE)))");
    CODE("	res[(i+j)%length/X2C_LSET_SIZE]|=1<<((i+j)%length%X2C_LSET_SIZE);");
  ELSE
    CODE("j=(short)(-n % length);");
    CODE("for (i=0; i<length; i++)");
    CODE("  if (a[(i+j)%length/X2C_LSET_SIZE] & (1<<((i+j)%length%X2C_LSET_SIZE)))");
    CODE("	res[i/X2C_LSET_SIZE]|=1<<(i%X2C_LSET_SIZE);");
  END;
  RETURN res;
END X2C_ROTL;

PROCEDURE [2] X2C_LSHL(res: RTS.LSET; a: RTS.LSET; length: INT16; n: INT32): RTS.LSET;
  VAR i[][2],j[][2]: INT16;
BEGIN
  i:=0; j:=0;
  CODE("memset(res,0,(length+X2C_LSET_SIZE-1)/X2C_LSET_SIZE*(X2C_LSET_SIZE/8));");
  IF n >= 0 THEN
    CODE("for (i=0; i+n<length; i++)");
    CODE("  if (a[i/X2C_LSET_SIZE] & (1<<(i%X2C_LSET_SIZE)))");
    CODE("	res[(i+n)/X2C_LSET_SIZE]|=1<<((i+n)%X2C_LSET_SIZE);");
  ELSE
    CODE("for (i=-n; i<length; i++)");
    CODE("  if (a[i/X2C_LSET_SIZE] & (1<<(i%X2C_LSET_SIZE)))");
    CODE("	res[(i+n)/X2C_LSET_SIZE]|=1<<((i+n)%X2C_LSET_SIZE);");
  END;
  RETURN res;
END X2C_LSHL;

END xrLSETs.
