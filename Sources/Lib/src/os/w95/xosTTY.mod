(* Copyright (c) xTech 1996.  All Rights Reserved *)
<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xosTTY; (* Hady. 04.08.96 10:32 *)

IMPORT  xWin32, SYSTEM;

CONST
  inpBits = xWin32.ENABLE_LINE_INPUT+xWin32.ENABLE_ECHO_INPUT;

VAR inp,out: xWin32.HANDLE;

PROCEDURE setCharMode(VAR old: SYSTEM.SET32): SYSTEM.INT32;
BEGIN
  IF xWin32.GetConsoleMode(inp,old) &
     xWin32.SetConsoleMode(inp,old-inpBits) THEN
    RETURN 0
  END;
  RETURN xWin32.GetLastError();
END setCharMode;

PROCEDURE setLineMode(VAR old: SYSTEM.SET32): SYSTEM.INT32;
BEGIN
  IF xWin32.GetConsoleMode(inp,old) &
     xWin32.SetConsoleMode(inp,old+inpBits) THEN
    RETURN 0
  END;
  RETURN xWin32.GetLastError();
END setLineMode;

PROCEDURE restoreMode(old: SYSTEM.SET32): SYSTEM.INT32;
BEGIN
  IF xWin32.SetConsoleMode(inp,old) THEN RETURN 0 END;
  RETURN xWin32.GetLastError();
END restoreMode;

TYPE P = POINTER TO ARRAY [0..255] OF CHAR;

PROCEDURE ExpandCR(p: P; VAR len: SYSTEM.CARD32);
  CONST
    CR = 0DX;
    LF = 0AX;
  VAR i,n: SYSTEM.INT32;
BEGIN
<* PUSH *> <* CHECKINDEX- *>
   IF len = 0 THEN (* most common case - do it efficiently *)
     IF p^[0] = CR THEN p^[1]:=LF; INC(len) END;
     RETURN
   END;
   ASSERT(len > 0);
   n:=0;
   FOR i:=0 TO INT(len)-1 DO
     IF p^[i] = CR THEN INC(n) END;
   END;
   IF n > 0 THEN
     i:=INT(len)-1; INC(len,ORD(n)); n:=INT(len)-1;
     WHILE i>=0 DO
       IF p^[i] = CR THEN p^[n]:=LF; DEC(n) END;
       p^[n]:=p^[i];
       DEC(i); DEC(n);
     END;
     ASSERT((i = -1) & (n = -1));
   END;
<* POP *>
END ExpandCR;

PROCEDURE ["C"] X2C_ttyReadNE(buf: SYSTEM.ADDRESS; bsz: SYSTEM.CARD32; VAR rd: SYSTEM.CARD32): SYSTEM.INT32;
  VAR save: SYSTEM.SET32; res: SYSTEM.INT32;
BEGIN
  res:=setCharMode(save);
  IF res#0 THEN RETURN res END;
  ASSERT(bsz >= 2); (*!!! Ned *)
  IF xWin32.ReadFile(inp,buf,bsz DIV 2,rd,NIL) THEN
    ExpandCR(buf,rd);
    RETURN restoreMode(save);
  END;
  res:=xWin32.GetLastError();
  IF restoreMode(save)=0 THEN END; (* ignore result *)
  RETURN res;
END X2C_ttyReadNE;

PROCEDURE ["C"] X2C_ttyReadLE(buf: SYSTEM.ADDRESS; bsz: SYSTEM.CARD32; VAR rd: SYSTEM.CARD32): SYSTEM.INT32;
  VAR save: SYSTEM.SET32; res: SYSTEM.INT32;
BEGIN
  res:=setLineMode(save);
  IF res#0 THEN RETURN res END;
  IF xWin32.ReadFile(inp,buf,bsz,rd,NIL) THEN
    RETURN restoreMode(save);
  END;
  res:=xWin32.GetLastError();
  IF restoreMode(save)=0 THEN END; (* ignore result *)
  RETURN res;
END X2C_ttyReadLE;

PROCEDURE ["C"] X2C_ttyWrite(buf: SYSTEM.ADDRESS; cc: SYSTEM.CARD32): SYSTEM.INT32;
  VAR wr: SYSTEM.CARD32;
BEGIN
  IF xWin32.WriteFile(out,buf,cc,wr,NIL) THEN RETURN 0 END;
  RETURN xWin32.GetLastError();
END X2C_ttyWrite;

PROCEDURE ["C"] X2C_InitTTY(): SYSTEM.INT32;
  VAR name: ARRAY [0..7] OF CHAR;
BEGIN
  COPY("CONIN$",name);
  inp:=xWin32.CreateFileA(SYSTEM.ADR(name),xWin32.GENERIC_READ+xWin32.GENERIC_WRITE,
                          xWin32.FILE_SHARE_READ+xWin32.FILE_SHARE_WRITE,
                          NIL,xWin32.OPEN_EXISTING,{},0);
  IF inp=xWin32.INVALID_HANDLE_VALUE THEN RETURN xWin32.GetLastError() END;
  COPY("CONOUT$",name);
  out:=xWin32.CreateFileA(SYSTEM.ADR(name),xWin32.GENERIC_READ+xWin32.GENERIC_WRITE,
                          xWin32.FILE_SHARE_READ+xWin32.FILE_SHARE_WRITE,
                          NIL,xWin32.OPEN_EXISTING,{},0);
  IF out=xWin32.INVALID_HANDLE_VALUE THEN RETURN xWin32.GetLastError() END;
  RETURN 0;
END X2C_InitTTY;

END xosTTY.
