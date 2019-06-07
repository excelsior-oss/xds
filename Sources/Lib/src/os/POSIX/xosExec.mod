(* Copyright (c) 1996,99 XDS Ltd. All Rights Reserved. *)
<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xosExec; (* Hady. 28.06.96 16:57 *)

IMPORT  SYSTEM, xlibOS, X2C, xPOSIX, xmRTS;

TYPE
  X2C_pCHAR = xmRTS.X2C_pCHAR;
  int = SYSTEM.int;

PROCEDURE ["C"] X2C_Execute(cmd,args: xmRTS.X2C_pCHAR; overlay: SYSTEM.int; VAR exitcode: SYSTEM.CARD32): SYSTEM.INT32;
  VAR
    argv: ARRAY [0..255] OF xPOSIX.xPOSIX_PCHAR;
    i   : INTEGER;
    rc  : SYSTEM.INT32;
BEGIN
  i:=1;
  LOOP
    WHILE args^<=' ' DO
      IF args^=0C THEN EXIT END;
      args:=SYSTEM.ADDADR(args,1);
    END;
    argv[i]:=args; INC(i);
    WHILE args^>' ' DO args:=SYSTEM.ADDADR(args,1) END;
    IF args^=0C THEN EXIT END;
    args^:=0C; args:=SYSTEM.ADDADR(args,1);
  END;
  argv[i]:=NIL;
  argv[0]:=cmd;
  IF (overlay#0) THEN
    SYSTEM.CODE("#if defined(_msdos)");
    rc := xPOSIX.spawnv(xPOSIX.P_WAIT,cmd,argv);
    SYSTEM.CODE("#else");
    rc := xPOSIX.execv(cmd,argv);
    SYSTEM.CODE("#endif");
  ELSE
    rc := xPOSIX.spawnv(xPOSIX.P_WAIT,cmd,argv);
  END;
  IF rc < 0 THEN
    RETURN xPOSIX.errno
  ELSE
    exitcode := rc;
    RETURN 0;
  END
END X2C_Execute;

PROCEDURE ["C"] X2C_ExecuteNoWindow(cmd,args: xmRTS.X2C_pCHAR; overlay: SYSTEM.int; VAR exitcode: SYSTEM.CARD32): SYSTEM.INT32;
BEGIN
  RETURN X2C_Execute (cmd, args, overlay, exitcode);
END X2C_ExecuteNoWindow;


(*----------------------------------------------------------------------------*)
PROCEDURE ["C"] X2C_Command (cmd: xmRTS.X2C_pCHAR; VAR exitcode: SYSTEM.CARD32): SYSTEM.INT32;
VAR  rc: SYSTEM.INT32;
BEGIN
  rc := xPOSIX.system(cmd);
  IF rc < 0 THEN
    RETURN xPOSIX.errno
  ELSE
    exitcode := rc;
    RETURN 0;
  END
END X2C_Command;

END xosExec.
