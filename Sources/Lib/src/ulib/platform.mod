(* Copyright (c) 1994 xTech Ltd, Russia. All Rights Reserved. *)
IMPLEMENTATION MODULE platform; (* Ned 15-Feb-94. *)

IMPORT  CharClass;

PROCEDURE IsPathDelim(c: CHAR): BOOLEAN;
BEGIN
  RETURN (c=';') OR CharClass.IsWhiteSpace(c)
END IsPathDelim;

END platform.
