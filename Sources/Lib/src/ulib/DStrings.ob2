(** Copyright (c) 1994 xTech Ltd, Russia. All Rights Reserved. *)
<*+ O2EXTENSIONS *>
MODULE DStrings; (* Ned 17-Feb-94. *)

TYPE String* = POINTER TO ARRAY OF CHAR;

PROCEDURE Assign*(s-: ARRAY OF CHAR; VAR d: String);
(** Allocates a new string and copies from "s".
  Resulting string always contains string terminator (0X).
*)
BEGIN
  NEW(d,LENGTH(s)+1);
  COPY(s,d^);
END Assign;

PROCEDURE Append*(s-: ARRAY OF CHAR; VAR d: String);
(** Append string "s", resizing "d" if necessary. *)
  VAR i,slen,dlen: LONGINT; n: String;
BEGIN
  IF d=NIL THEN dlen:=0 ELSE dlen:=LENGTH(d^) END;
  slen:=LENGTH(s);
  IF dlen=0 THEN
    NEW(d,slen+1); COPY(s,d^);
  ELSE
    IF dlen+slen >= LEN(d^) THEN
      NEW(n,dlen+slen+1); COPY(d^,n^);
      d:=n;
    END;
    FOR i:=0 TO slen-1 DO d^[dlen]:=s[i]; INC(dlen) END;
    d^[dlen]:=0X;
  END;
END Append;

END DStrings.
