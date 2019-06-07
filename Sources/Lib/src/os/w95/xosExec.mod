(* Copyright (c) 1996 xTech Ltd, Russia. All Rights Reserved. *)
<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xosExec; (* Hady. 28.06.96 16:57 *)

IMPORT  SYSTEM, X2C, xlibOS, xWin32, xmRTS, xrtsOS;

TYPE
  X2C_pCHAR = xmRTS.X2C_pCHAR;
  int = SYSTEM.int;

PROCEDURE length(s: X2C_pCHAR): CARDINAL;
  VAR l: CARDINAL;
BEGIN
  l:=0;
  WHILE s^#0C DO
    INC(l); s:=SYSTEM.ADDADR(s,1);
  END;
  RETURN l;
END length;

PROCEDURE pack(to: X2C_pCHAR; c,a: X2C_pCHAR);
BEGIN
  to^:='"'; to:=SYSTEM.ADDADR(to,1);
  WHILE c^#0C DO
    to^:=c^; to:=SYSTEM.ADDADR(to,1);
    c:=SYSTEM.ADDADR(c,1);
  END;
  to^:='"'; to:=SYSTEM.ADDADR(to,1);
  to^:=' '; to:=SYSTEM.ADDADR(to,1);
  WHILE a^#0C DO
    to^:=a^; to:=SYSTEM.ADDADR(to,1);
    a:=SYSTEM.ADDADR(a,1);
  END;
  to^:=0C;
END pack;


PROCEDURE ["C"] CreateProcess (cmd, args: X2C_pCHAR; overlay: int; no_window: BOOLEAN; 
                               VAR exitcode: SYSTEM.CARD32): SYSTEM.INT32;
  VAR
    si  : xWin32.STARTUPINFO;
    pi  : xWin32.PROCESS_INFORMATION;
    a   : X2C_pCHAR;
    bsz : CARDINAL;
    flags: SYSTEM.SET32;
    res : SYSTEM.INT32;

BEGIN
  bsz := length(cmd) + length(args) + 2 + 2;
  a:=xrtsOS.X2C_malloc(bsz);
  IF a=NIL THEN X2C.X2C_TRAP(X2C.X2C_noMemoryException) END;
  pack(a,cmd,args);

  si.cb:=SIZE(si);
  si.lpReserved0:=NIL;
  si.lpDesktop:=NIL;
  si.lpTitle:=NIL;
  si.dwFlags:={8};
  si.wcbReserved:=0;
  si.lpReserved1:=NIL;
  si.stdin:=xWin32.GetStdHandle(xWin32.STD_INPUT_HANDLE);
  si.stdout:=xWin32.GetStdHandle(xWin32.STD_OUTPUT_HANDLE);
  si.stderr:=xWin32.GetStdHandle(xWin32.STD_ERROR_HANDLE);

  IF no_window THEN
    flags := xWin32.DETACHED_PROCESS;
  ELSE
    flags := {};
  END;
  IF NOT xWin32.CreateProcess(cmd,a,NIL,NIL,TRUE,flags,NIL,NIL,si,pi) THEN
    xrtsOS.X2C_free(a,bsz);
    RETURN xWin32.GetLastError();
  END;
  xrtsOS.X2C_free(a,bsz);

(*
  IF overlay#0 THEN
    IF xWin32.CloseHandle(pi.hthr) THEN END;
    IF xWin32.CloseHandle(pi.hprs) THEN END;
    HALT;
  ELSE
*)
    IF (xWin32.WaitForSingleObject(pi.hprs,0FFFFFFFFH)=xWin32.WAIT_FAILED) OR
       NOT xWin32.GetExitCodeProcess(pi.hprs,exitcode) OR
       NOT xWin32.CloseHandle(pi.hthr) OR
       NOT xWin32.CloseHandle(pi.hprs)
    THEN 
      RETURN xWin32.GetLastError();
    END;
(*
  END;
*)
  RETURN 0;
END CreateProcess;


PROCEDURE ["C"] X2C_Execute(cmd,args: X2C_pCHAR; overlay: int; VAR exitcode: SYSTEM.CARD32): SYSTEM.INT32;
BEGIN
  RETURN CreateProcess (cmd, args, overlay, FALSE, exitcode);
END X2C_Execute;

PROCEDURE X2C_ExecuteNoWindow(cmd,args: xmRTS.X2C_pCHAR; overlay: SYSTEM.int; 
                              VAR exitcode: SYSTEM.CARD32): SYSTEM.INT32;
VAR 
  consoleNeeded : BOOLEAN;
  buffer        : ARRAY [0..1] OF CHAR;
  bytesWritten  : xWin32.DWORD;
BEGIN
  consoleNeeded := xWin32.WriteConsole (xWin32.GetStdHandle (xWin32.STD_OUTPUT_HANDLE), 
                                        buffer, 0, bytesWritten, NIL)
                   OR
                   xWin32.WriteConsole (xWin32.GetStdHandle (xWin32.STD_ERROR_HANDLE), 
                                        buffer, 0, bytesWritten, NIL);

  RETURN CreateProcess (cmd, args, overlay, NOT consoleNeeded, exitcode);
END X2C_ExecuteNoWindow;


(*----------------------------------------------------------------------------*)
PROCEDURE ["C"] X2C_Command (cmd: xmRTS.X2C_pCHAR; VAR exitcode: SYSTEM.CARD32): SYSTEM.INT32;
CONST shell_arg = "/C";
VAR shell: ARRAY [0..127] OF CHAR;
    a   : X2C_pCHAR;
    bsz : CARDINAL;
    res : SYSTEM.INT32;
BEGIN
  xlibOS.X2C_EnvString ( "ComSpec"
                       , SYSTEM.CAST(xmRTS.X2C_pCHAR,SYSTEM.ADR(shell))
                       , 127+1
                       );
  bsz := length(cmd) + LENGTH(shell_arg) + 2 + 2;
  a := xrtsOS.X2C_malloc(bsz);
  IF a=NIL THEN X2C.X2C_TRAP(X2C.X2C_noMemoryException) END;
  pack(a, SYSTEM.CAST(xmRTS.X2C_pCHAR,SYSTEM.ADR(shell_arg)), cmd);
  res := X2C_Execute(SYSTEM.ADR(shell), a, 0, exitcode);
  xrtsOS.X2C_free(a,bsz);
  RETURN res;
END X2C_Command;


END xosExec.
