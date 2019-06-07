(* Copyright (c)1996 xTech Ltd.  All Rights Reserved *)
<* +M2EXTENSIONS *>

IMPLEMENTATION MODULE xosTTY; (* VitVit. 09.09.96 *)

IMPORT O := xOS2,
       SYSTEM;

PROCEDURE ["C"] X2C_ttyReadNE (buf :SYSTEM.ADDRESS; bsz :SYSTEM.CARD32; VAR rd :SYSTEM.CARD32) :SYSTEM.INT32;

PROCEDURE KeyPressed() :BOOLEAN;
VAR
  k :O.KBDKEYINFO;
BEGIN
  k.chChar    := 0C;
  k.chScan    := 0C;
  k.bNlsShift := 0C;
  O.KbdPeek (k, 0);
  RETURN (k.fbStatus # 0C);
END KeyPressed;

PROCEDURE RdKey() :CHAR;
VAR
  k :O.KBDKEYINFO;
  c :CHAR;
BEGIN
  O.KbdCharIn( k,0,0 );
  IF (( k.chChar = 0C ) OR ( k.chChar = CHR(0E0H) ))
    THEN RETURN 0C;
  END;
  RETURN k.chChar;
END RdKey;

CONST
  CR = 0DX;
  LF = 0AX;
TYPE
  pAC = POINTER TO ARRAY [0..255] OF CHAR;
VAR
  pc :pAC;
  i  :CARDINAL;

BEGIN
  ASSERT(bsz >= 2);

  pc := pAC (buf);
  pc^[0] := RdKey();
  rd := 1;

  WHILE KeyPressed() AND (rd # bsz ) DO
    pc^[rd] := RdKey();
    INC (rd);
  END;

  i := 0;
  WHILE (pc^[i] # 0C) AND (pc^[i] # CR) DO
    IF  (i = rd ) THEN RETURN 0; END;
    INC (i);
  END;

  (* F Key or Enter's been pressed - write <CR,LF> *)
  IF (i >= bsz-1)
    THEN DEC (rd);
         RETURN 0;  -- buffer too small
  END;
  pc^[i]   := CR;
  pc^[i+1] := LF;
  rd := i+2;
  RETURN 0;
END X2C_ttyReadNE;


PROCEDURE ["C"] X2C_ttyReadLE (buf :SYSTEM.ADDRESS; bsz :SYSTEM.CARD32; VAR rd :SYSTEM.CARD32) :SYSTEM.INT32;
VAR
  rc :SYSTEM.CARD32;
BEGIN
  rc := O.DosRead ( O.STDIN, buf, bsz, rd );
  RETURN SYSTEM.INT32 (rc);
END X2C_ttyReadLE;


PROCEDURE ["C"] X2C_ttyWrite (buf :SYSTEM.ADDRESS; cc :SYSTEM.CARD32) :SYSTEM.INT32;
VAR
  actWrote, rc :SYSTEM.CARD32;
BEGIN
  rc := O.DosWrite ( O.STDOUT, buf, cc, actWrote);
  RETURN SYSTEM.INT32 (rc);
END X2C_ttyWrite;


PROCEDURE ["C"] X2C_InitTTY() :SYSTEM.INT32;
BEGIN
  O.KbdFlushBuffer( 0 );
  RETURN 0;
END X2C_InitTTY;

END xosTTY.

