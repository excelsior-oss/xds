(* Copyright (C) 1998 XDS Ltd *)

<*+M2ADDTYPES   *>
<*+M2EXTENSIONS *>

IMPLEMENTATION MODULE xtsIGraph;

IMPORT W := Windows;

(* DO NOT IMPORT TS-modules !!!!!!!!!!!!! *)

BEGIN
  proc    := NIL;
  fGrMode := FALSE;
  hConsoleInput := W.GetStdHandle(W.STD_INPUT_HANDLE);
  W.FlushConsoleInputBuffer (hConsoleInput);
END xtsIGraph.
