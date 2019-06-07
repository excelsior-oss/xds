(* Copyright (c) Excelsior LLC, 2004. All Rights Reserved. *)
<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xosExec; (* Hady. 28.06.96 16:57 *)
                               (* Jek. 02.12.2002-02.07.2004 *)

IMPORT SYSTEM, xlibOS, X2C, xmRTS, spawn, xrtsOS;

FROM wait IMPORT waitpid;
FROM unistd IMPORT execv, environ;
FROM stdlib IMPORT system;
FROM types IMPORT pid_t;
FROM x2cLib IMPORT get_errno;


TYPE
  X2C_pCHAR = xmRTS.X2C_pCHAR;
  int = SYSTEM.int;


PROCEDURE WIFSTOPPED(status: SYSTEM.INT32): BOOLEAN;
BEGIN
  RETURN ((status AND 0FFH) = 07FH);
END WIFSTOPPED;


PROCEDURE WTERMSIG(status: SYSTEM.INT32): SYSTEM.INT32;
BEGIN
  RETURN (status AND 07FH);
END WTERMSIG;


PROCEDURE WIFEXITED(status: SYSTEM.INT32): BOOLEAN;
BEGIN
  RETURN WTERMSIG(status) = 0;
END WIFEXITED;


PROCEDURE WIFSIGNALED(status: SYSTEM.INT32): BOOLEAN;
BEGIN
  RETURN ((NOT WIFSTOPPED(status)) AND (NOT WIFEXITED(status)));
END WIFSIGNALED;


PROCEDURE WEXITSTATUS(status: SYSTEM.INT32): SYSTEM.INT32;
BEGIN
  RETURN SYSTEM.INT32(SYSTEM.SHIFT(SYSTEM.CAST(BITSET, (status AND 0FF00H)), -8));
END WEXITSTATUS;


PROCEDURE WSTOPSIG(status: SYSTEM.INT32): SYSTEM.INT32;
BEGIN
  RETURN WEXITSTATUS(status)
END WSTOPSIG;


PROCEDURE ["C"] X2C_Execute(cmd,args: xmRTS.X2C_pCHAR; overlay: SYSTEM.int; VAR exitcode: SYSTEM.CARD32): SYSTEM.INT32;
VAR
  argv   : ARRAY [0..255] OF xmRTS.X2C_pCHAR;
  i      : INTEGER;
  rc     : SYSTEM.INT32;
  pid    : pid_t;
  loc    : INTEGER;
  quotes : BOOLEAN;

  (* removes current symbol *)
  PROCEDURE argscut();
  VAR
    a:xmRTS.X2C_pCHAR;
  BEGIN
    a:=args;
    WHILE a^#0C DO
      a^:=X2C_pCHAR(a+1)^;
      a:=SYSTEM.ADDADR(a,1)
    END;
  END argscut;

BEGIN
  i:=1;
  LOOP
    quotes:=FALSE;
    WHILE args^<=' ' DO
      IF args^=0C THEN EXIT END;
      args:=SYSTEM.ADDADR(args,1)
    END;
    IF args^='"' THEN
      quotes:=TRUE;
      args:=SYSTEM.ADDADR(args,1)
    END;
    argv[i]:=args; INC(i);
    LOOP
      IF args^=0C THEN EXIT END;
      IF (NOT quotes) AND (args^<=' ') THEN EXIT END;
      IF args^='"' THEN
        quotes:=NOT quotes;
        argscut()
      ELSE
        args:=SYSTEM.ADDADR(args,1)
      END;
    END;
    IF args^=0C THEN EXIT END;
    args^:=0C; args:=SYSTEM.ADDADR(args,1);
  END;
  
  argv[i]:=NIL;
  argv[0]:=cmd;
  
  IF (overlay#0) THEN
    rc := execv(cmd,argv);
    
    IF rc < 0 THEN
      RETURN get_errno()
    ELSE
      exitcode := rc;
    END;
  ELSE

    rc := spawn.posix_spawn(pid, cmd, NIL, NIL, argv, environ);

    IF rc # 0 THEN
      RETURN rc (* error code *)
    END;
    
    (* wait for termination of the created process *)
    IF waitpid(pid, loc, 0) < 0 THEN
      RETURN get_errno()
    END;
      
    IF NOT WIFEXITED(loc) THEN
      -- encode the fault reason
      IF WIFSIGNALED(loc) THEN
        exitcode := SYSTEM.CAST(SYSTEM.CARD32, 
                        SYSTEM.SHIFT(SYSTEM.CAST(BITSET, WTERMSIG(loc)), 8))
      ELSIF WIFSTOPPED(loc) THEN
        exitcode := SYSTEM.CAST(SYSTEM.CARD32,
                        SYSTEM.SHIFT(SYSTEM.CAST(BITSET, WSTOPSIG(loc)), 16))
      ELSE
        RETURN -1 (* should not happen *)
      END;
    ELSE
      exitcode := SYSTEM.CAST(SYSTEM.CARD32, WEXITSTATUS(loc));
    END;
  END;

  RETURN 0;
END X2C_Execute;

PROCEDURE ["C"] X2C_ExecuteNoWindow(cmd,args: xmRTS.X2C_pCHAR; overlay: SYSTEM.int; VAR exitcode: SYSTEM.CARD32): SYSTEM.INT32;
BEGIN
  RETURN X2C_Execute (cmd, args, overlay, exitcode);
END X2C_ExecuteNoWindow;

(*----------------------------------------------------------------------------*)
PROCEDURE ["C"] X2C_Command (cmd: xmRTS.X2C_pCHAR; VAR exitcode: SYSTEM.CARD32): SYSTEM.INT32;
VAR  rc: SYSTEM.INT32;
BEGIN
  rc := system(cmd);
  IF rc < 0 THEN
    RETURN get_errno()
  ELSE
    exitcode := rc;
    RETURN 0;
  END
END X2C_Command;

END xosExec.
