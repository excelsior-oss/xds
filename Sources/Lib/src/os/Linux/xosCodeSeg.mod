(* Copyright (c) Excelsior, 2005.  All Rights Reserved *)

<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xosCodeSeg;

IMPORT SYSTEM;

PROCEDURE calcCodeExtent ( mh :SYSTEM.ADDRESS; rva :CARDINAL; VAR from, to, sec :CARDINAL );
BEGIN
  from := 0;
  to := 0;
  sec := 0;
END calcCodeExtent;


END xosCodeSeg.
