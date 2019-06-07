(* Copyright (c) xTech 1996.  All Rights Reserved *)
(* Copyright (c) Excelsior LLC, 2002.  All Rights Reserved *)
<* +M2EXTENSIONS *>
<* +M2ADDTYPES *>
IMPLEMENTATION MODULE xosTTY; (* Jek. 28 Nov 2002 *) 

IMPORT SYSTEM;

FROM termios IMPORT tcgetattr, tcsetattr, termios, 
     ICANON, ECHO, TCSANOW, TCSADRAIN, TCSAFLUSH;
FROM unistd IMPORT read, write;
FROM fcntl IMPORT open, O_RDWR, O_NOCTTY;
FROM x2cLib IMPORT get_errno;

     
VAR 
  tty : SYSTEM.int;
  
  
PROCEDURE ["C"] X2C_ttyReadNE(buf: SYSTEM.ADDRESS; bsz: SYSTEM.CARD32; VAR rd: SYSTEM.CARD32): SYSTEM.INT32;
TYPE
  parr = POINTER TO ARRAY OF CHAR;
  
VAR
  tty_attr, tty_attr_save : termios;
  buffer : parr;
  ires : INTEGER;

BEGIN
  IF (tcgetattr(tty, tty_attr_save) # 0) OR (tcgetattr(tty, tty_attr) # 0) THEN
    RETURN get_errno();
  END;

  tty_attr.c_lflag := (tty_attr.c_lflag & SYSTEM.CAST(CARDINAL, ~(ICANON AND ECHO)));
  ires := read(tty, buf, bsz-1);

  IF (tcsetattr(tty, TCSANOW, tty_attr) # 0) OR (ires = -1) THEN
    RETURN get_errno();
  END;

  rd := ires;
  buffer := SYSTEM.CAST(parr, buf);
  buffer^[rd] := 0C;

  IF (tcsetattr(tty, TCSANOW, tty_attr_save) = -1) THEN
     RETURN get_errno()
  ELSE
     RETURN 0
  END;
END X2C_ttyReadNE;


PROCEDURE ["C"] X2C_ttyReadLE(buf: SYSTEM.ADDRESS; bsz: SYSTEM.CARD32; VAR rd: SYSTEM.CARD32): SYSTEM.INT32;
TYPE
  parr = POINTER TO ARRAY OF CHAR;
  
VAR
  tty_attr, tty_attr_save : termios;
  buffer : parr;
  ires : INTEGER;

BEGIN
  IF (tcgetattr(tty, tty_attr_save) # 0) OR (tcgetattr(tty, tty_attr) # 0) THEN
    RETURN get_errno();
  END;

  tty_attr.c_lflag := (tty_attr.c_lflag OR SYSTEM.CAST(CARDINAL, ICANON AND ECHO));
  ires := read(tty, buf, bsz-1);

  IF (tcsetattr(tty, TCSANOW, tty_attr) # 0) OR (ires = -1) THEN
    RETURN get_errno();
  END;

  rd := ires;
  buffer := SYSTEM.CAST(parr, buf);
  buffer^[rd] := 0C;

  IF (tcsetattr(tty, TCSANOW, tty_attr_save) = -1) THEN
     RETURN get_errno()
  ELSE
     RETURN 0
  END;
END X2C_ttyReadLE;


PROCEDURE ["C"] X2C_ttyWrite(buf: SYSTEM.ADDRESS; cc: SYSTEM.CARD32): SYSTEM.INT32;
BEGIN
  IF write(tty, buf, cc) = -1 THEN 
    RETURN get_errno()
  ELSE
    RETURN 0
  END;
END X2C_ttyWrite;


PROCEDURE ["C"] X2C_InitTTY(): SYSTEM.INT32;
BEGIN
  tty := open("/dev/tty", O_RDWR OR O_NOCTTY);

  IF tty = -1 THEN
    RETURN get_errno()
  ELSE
    RETURN 0
  END;
END X2C_InitTTY;


END xosTTY.
