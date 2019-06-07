(* Copyright (c) 1996 xTech Ltd, Russia. All Rights Reserved. *)
<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xosExec; (* Hady. 28.06.96 16:57 *)

IMPORT  SYSTEM, xmRTS, xOS2, xrtsOS, X2C;

TYPE
  X2C_pCHAR = xmRTS.X2C_pCHAR;
  int = SYSTEM.int;

PROCEDURE length(s: X2C_pCHAR): CARDINAL;
  VAR l: CARDINAL; ch: CHAR;
BEGIN
  l:=0;
  WHILE s^#0C DO
    INC(l); s:=SYSTEM.ADDADR(s,1);
  END;
  RETURN l;
END length;

PROCEDURE pack(to: X2C_pCHAR; c,a: X2C_pCHAR);
BEGIN
  WHILE c^#0C DO
    to^:=c^; to:=SYSTEM.ADDADR(to,1);
    c:=SYSTEM.ADDADR(c,1);
  END;
  to^:=0C; to:=SYSTEM.ADDADR(to,1);
  WHILE a^#0C DO
    to^:=a^; to:=SYSTEM.ADDADR(to,1);
    a:=SYSTEM.ADDADR(a,1);
  END;
  to^:=0C; to:=SYSTEM.ADDADR(to,1);
  to^:=0C; to:=SYSTEM.ADDADR(to,1);
END pack;

PROCEDURE ["C"] X2C_Execute(cmd,args: xmRTS.X2C_pCHAR; overlay: SYSTEM.int; VAR exitcode: SYSTEM.CARD32): SYSTEM.INT32;
  VAR
    lc,la,bsz: CARDINAL;
    a: X2C_pCHAR;
    ErrObjName : ARRAY [0..63] OF CHAR;
    res : SYSTEM.INT32;
    rc : xOS2.APIRET;
    ret_codes : xOS2.RESULTCODES;
BEGIN
  lc:=length(cmd);
  la:=length(args);
  bsz:=lc+la+3;
  a:=xrtsOS.X2C_malloc(bsz);
  IF a=NIL THEN X2C.X2C_TRAP(X2C.X2C_noMemoryException) END;
  pack(a,cmd,args);
  ErrObjName [0] := 0X;
  rc := xOS2.DosExecPgm (ErrObjName, SIZE(ErrObjName),
             xOS2.EXEC_SYNC,
             a,             -- command line
             NIL,           -- inherit parent's environment
             ret_codes,
             cmd);          -- program name
  IF rc # xOS2.ok THEN
    res := SYSTEM.CAST(SYSTEM.INT32, rc);
  ELSE
    res      := 0;
    exitcode := SYSTEM.CAST(int, ret_codes.codeResult);
  END;
  IF overlay#0 THEN (* How to start a task replacing its parent in OS/2 ?? *)
    HALT(res);
  END;
  xrtsOS.X2C_free(a,bsz);
  RETURN res;
END X2C_Execute;

PROCEDURE ["C"] X2C_ExecuteNoWindow(cmd,args: xmRTS.X2C_pCHAR; overlay: SYSTEM.int; VAR exitcode: SYSTEM.CARD32): SYSTEM.INT32;
BEGIN
  RETURN X2C_Execute (cmd, args, overlay, exitcode);
END X2C_ExecuteNoWindow;

END xosExec.
