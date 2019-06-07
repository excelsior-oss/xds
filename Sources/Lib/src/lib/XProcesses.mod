(* Copyright (c) 1996 xTech Ltd, Russia. All Rights Reserved. *)
IMPLEMENTATION MODULE XProcesses; (* Hady, Jul 30, 1996 *)

IMPORT  SYSTEM, xrThreads;

PROCEDURE IsGCAware(p: ProcessId): BOOLEAN;
  VAR pid: xrThreads.X2C_Process;
BEGIN
  <* IF multithread THEN *>
    pid:=SYSTEM.CAST(xrThreads.X2C_Process,p);
    RETURN NOT (xrThreads.X2C_no_GC IN pid^.tags);
  <* ELSE *>
    RETURN TRUE
  <* END *>
END IsGCAware;

PROCEDURE SetGCAware(p: ProcessId; yes: BOOLEAN);
  VAR pid: xrThreads.X2C_Process;
BEGIN
  <* IF multithread THEN *>
    pid:=SYSTEM.CAST(xrThreads.X2C_Process,p);
    IF yes THEN
      EXCL(pid^.tags,xrThreads.X2C_no_GC);
    ELSE
      INCL(pid^.tags,xrThreads.X2C_no_GC);
    END;
  <* END *>
END SetGCAware;

END XProcesses.
