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
IMPLEMENTATION MODULE xrcChecks; (*  08-19-95 02:01pm *)

IMPORT
	SYSTEM
	,xmRTS
	,M2EXCEPTION
	;

TYPE
	CARD16 = SYSTEM.CARD16;
	CARD32 = SYSTEM.CARD32;
	INT16  = SYSTEM.INT16;
	INT32  = SYSTEM.INT32;

PROCEDURE [2] X2C_CHKINX_F(i: CARD32; length: CARD16): CARD16;
BEGIN
  IF i >= length THEN xmRTS.X2C_TRAP_F(ORD(M2EXCEPTION.indexException)) END;
  RETURN VAL(CARD16,i);
END X2C_CHKINX_F;

PROCEDURE [2] X2C_CHKINXL_F(i: CARD32; length: CARD32): CARD32;
BEGIN
  IF i >= length THEN xmRTS.X2C_TRAP_F(ORD(M2EXCEPTION.indexException)) END;
  RETURN i;
END X2C_CHKINXL_F;

PROCEDURE [2] X2C_CHKS_F (i: INT16): INT16;
BEGIN
  IF i < 0 THEN xmRTS.X2C_TRAP_F(ORD(M2EXCEPTION.rangeException)) END;
  RETURN i;
END X2C_CHKS_F;

PROCEDURE [2] X2C_CHKSL_F(i: INT32): INT32;
BEGIN
  IF i < 0 THEN xmRTS.X2C_TRAP_F(ORD(M2EXCEPTION.rangeException)) END;
  RETURN i;
END X2C_CHKSL_F;

PROCEDURE [2] X2C_CHK_F (a: INT16; min: INT16; max: INT16): INT16;
BEGIN
  IF (a < min) OR (a > max) THEN xmRTS.X2C_TRAP_F(ORD(M2EXCEPTION.rangeException)) END;
  RETURN a;
END X2C_CHK_F;

PROCEDURE [2] X2C_CHKL_F(a: INT32; min: INT32; max: INT32): INT32;
BEGIN
  IF (a < min) OR (a > max) THEN xmRTS.X2C_TRAP_F(ORD(M2EXCEPTION.rangeException)) END;
  RETURN a;
END X2C_CHKL_F;

PROCEDURE [2] X2C_CHKU_F (a: CARD16; min: CARD16; max: CARD16): CARD16;
BEGIN
  IF (a < min) OR (a > max) THEN xmRTS.X2C_TRAP_F(ORD(M2EXCEPTION.rangeException)) END;
  RETURN a;
END X2C_CHKU_F;

PROCEDURE [2] X2C_CHKUL_F(a: CARD32; min: CARD32; max: CARD32): CARD32;
BEGIN
  IF (a < min) OR (a > max) THEN xmRTS.X2C_TRAP_F(ORD(M2EXCEPTION.rangeException)) END;
  RETURN a;
END X2C_CHKUL_F;

PROCEDURE [2] X2C_CHKNIL_F(p: xmRTS.X2C_pVOID): xmRTS.X2C_pVOID;
BEGIN
  IF p=NIL THEN xmRTS.X2C_TRAP_F(ORD(M2EXCEPTION.invalidLocation)) END;
  RETURN p;
END X2C_CHKNIL_F;

PROCEDURE [2] X2C_CHKPROC_F(p: PROC): PROC;
BEGIN
  IF p=NIL THEN xmRTS.X2C_TRAP_F(ORD(M2EXCEPTION.invalidLocation)) END;
  RETURN p;
END X2C_CHKPROC_F;

END xrcChecks.
