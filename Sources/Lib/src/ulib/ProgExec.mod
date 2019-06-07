(* Copyright (c) 1999 XDS Ltd. All Rights Reserved. *)
IMPLEMENTATION MODULE ProgExec; (* Snowman 28-Jun-99. *)

IMPORT SYSTEM;
IMPORT xosExec;
IMPORT IOChan, StdChans;


PROCEDURE execPrologue ();
BEGIN
  IOChan.Flush (StdChans.OutChan ());
  IOChan.Flush (StdChans.ErrChan ());
END execPrologue;


PROCEDURE execEpilogue ();
BEGIN
  SYSTEM.EVAL (StdChans.Synchronize ());
END execEpilogue;



PROCEDURE Execute(program: ARRAY OF CHAR;
                     args: ARRAY OF CHAR;
                 VAR exit: CARDINAL): BOOLEAN;
VAR
  res: BOOLEAN;
BEGIN
  execPrologue ();
  res := xosExec.X2C_Execute(SYSTEM.ADR(program), SYSTEM.ADR(args), 0, exit) = 0;
  execEpilogue ();
  RETURN res;
END Execute;

PROCEDURE ExecuteNoWindow(program: ARRAY OF CHAR;
                             args: ARRAY OF CHAR;
                         VAR exit: CARDINAL): BOOLEAN;
VAR
  res: BOOLEAN;
BEGIN
  execPrologue ();
  res := xosExec.X2C_ExecuteNoWindow(SYSTEM.ADR(program), SYSTEM.ADR(args), 0, exit) = 0;
  execEpilogue ();
  RETURN res;
END ExecuteNoWindow;

PROCEDURE Command(command: ARRAY OF CHAR;
                 VAR exit: CARDINAL): BOOLEAN;
VAR
  res: BOOLEAN;
BEGIN
  execPrologue ();
  res := xosExec.X2C_Command(SYSTEM.ADR(command), exit) = 0;
  execEpilogue ();
  RETURN res;
END Command;

END ProgExec.
