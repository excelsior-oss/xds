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

IMPLEMENTATION MODULE xrcRTS;

IMPORT
  SYSTEM,
  M2EXCEPTION,
  X2C,
  XMM:=xmRTS,
  RTS:=xmRTS,
  xPOSIX;

FROM SYSTEM IMPORT
	int, size_t, void, CODE, ADDRESS,
	CARD8, CARD16, CARD32, INT8, INT16, INT32,
	ADDADR, SET32;

PROCEDURE [2] X2C_IN(i: CARD32; bits: CARD16; set: SET32): BOOLEAN;
BEGIN
  IF i<bits THEN CODE("return (((1L << (int)i) & set) != 0);") END;
  RETURN FALSE;
END X2C_IN;

PROCEDURE [2] X2C_SET(a: CARD32; b: CARD32; bits: CARD16): SET32;
BEGIN
  IF (a>b) OR (a>=bits) OR (b>=bits) THEN X2C.X2C_TRAP(ORD(M2EXCEPTION.rangeException)) END;
  CODE("return ((X2C_SET32) ((2L<<(int)b) - (1L<<(int)a)));");
  (* AC: really it is OK code even with b = 31! *)
  RETURN SET32{};
END X2C_SET;

PROCEDURE [2] X2C_PCOPY(VAR p: RTS.X2C_pVOID; size: size_t);
  VAR a: ADDRESS;
BEGIN
  XMM.X2C_ALLOCATE(a,size);
  IF a=NIL THEN X2C.X2C_TRAP(X2C.X2C_noMemoryException) END;
  xPOSIX.memcpy(a,p,size);
  p:=SYSTEM.CAST(RTS.X2C_pVOID,a);
END X2C_PCOPY;

PROCEDURE [2] X2C_PFREE(p: RTS.X2C_pVOID);
  VAR a: ADDRESS;
BEGIN
  a:=SYSTEM.CAST(ADDRESS,p);
  XMM.X2C_DEALLOCATE(a);
END X2C_PFREE;

PROCEDURE [2] X2C_PROT(): PROTECTION;
  VAR current: RTS.X2C_Coroutine;
BEGIN
  current:=RTS.X2C_GetCurrent();
  RETURN current^.prot;
END X2C_PROT;

END xrcRTS.
