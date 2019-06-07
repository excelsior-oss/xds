(* Copyright (C) 1999-2000 Excelsior. *)
(* IO : OS/2 *)

<*+M2ADDTYPES   *>
<*+M2EXTENSIONS *>

<*-IOVERFLOW    *>
<*-COVERFLOW    *>
<*-CHECKINDEX   *>
<*-CHECKRANGE   *>
<*-CHECKNIL     *>

IMPLEMENTATION MODULE xtsIO;

IMPORT SYSTEM;

PROCEDURE SetConMode(set : BOOLEAN; VAR tmp : SYSTEM.CARD32);
BEGIN
  -- used in Win32
END SetConMode;

END xtsIO.

