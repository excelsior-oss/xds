(** Oakwood Oberon-2 library *)
(** Copyright (c) xTech 1994,95. All Rights Reserved. *)
(* Implementation based on ISO M2 Library *)
<*+ O2EXTENSIONS *>
MODULE Out; (* Ned 18-Jan-95 *)

(** Formatted output of characters, numbers and strings. *)

IMPORT  SYSTEM, STextIO, SWholeIO, RealStr, LongStr, Strings;

PROCEDURE Open*;
(** Initializes the output stream *)
BEGIN
  (* nothing to do *)
END Open;

PROCEDURE Char*(ch: CHAR);
(** Writes the character ch to the end of the output stream *)
BEGIN
  STextIO.WriteChar(ch);
END Char;

PROCEDURE String*(str-: ARRAY OF CHAR);
(** Writes the null-terminated character sequence s to the
  end of the output stream (without 0X).
*)
BEGIN
  STextIO.WriteString(str);
END String;

PROCEDURE Int*(x,n: LONGINT);
(** Writes the integer number x to the end of the output stream.
  If the textual representation of i requires m characters,
  x is right adjusted in a field Max(n,m) characters padded with
  blanks at the left end. A plus sign is not written.
*)
  VAR i: INTEGER; x0: LONGINT;
    a: ARRAY 11 OF CHAR;
BEGIN i := 0;
  IF x < 0 THEN
    IF x = MIN(LONGINT) THEN String(" -2147483648"); RETURN
    ELSE DEC(n); x0 := -x
    END
  ELSE x0 := x
  END;
  REPEAT
    a[i] := CHR(x0 MOD 10 + ORD('0')); x0 := x0 DIV 10; INC(i)
  UNTIL x0 = 0;
  WHILE n > i DO Char(" "); DEC(n) END;
  IF x < 0 THEN Char("-") END;
  REPEAT DEC(i); Char(a[i]) UNTIL i = 0;
END Int;

PROCEDURE ChangeReal(VAR s: ARRAY OF CHAR; long: BOOLEAN);
  VAR i,beg,end,len: LONGINT;
BEGIN
  len:=LENGTH(s);
  i:=0;
  WHILE (i < len) & (s[i] # "E") & (s[i] # ".") DO INC(i) END;
  IF i = len THEN Strings.Append(".0",s);
  ELSIF s[i] = "E" THEN
    IF long THEN s[i]:="D" END;
    Strings.Insert(".0",VAL(SYSTEM.CARD,i),s)
  ELSE
    ASSERT(s[i] = ".");
    INC(i); beg:=i;
    WHILE (i < len) & (s[i] # "E") DO INC(i) END;
    IF long & (s[i] = "E") THEN s[i]:="D" END;
    DEC(i); end:=i;
    WHILE (i > beg) & (s[i] = "0") DO DEC(i) END;
    IF i < end THEN 
      Strings.Delete(s,VAL(SYSTEM.CARD,i+1),VAL(SYSTEM.CARD,end-i)); 
    END;
  END;
END ChangeReal;

PROCEDURE Real*(x: REAL; n: INTEGER);
(** Writes the real number x to the end of the output stream.
  If the textual representation of x requires m characters,
  x is right adjusted in a field Max(n,m) characters padded with
  blanks at the left end. A plus sign of the mantissa is not
  written.
*)
  VAR str: ARRAY 32 OF CHAR; len: LONGINT;
BEGIN
  RealStr.RealToFloat(x,6,str);
  ChangeReal(str,FALSE);
  len:=LENGTH(str);
  WHILE n > len DO Char(" "); DEC(n) END;
  String(str);
END Real;

PROCEDURE LongReal*(x: LONGREAL; n: INTEGER);
(** Writes the long real number x to the end of the output stream.
  If the textual representation of x requires m characters,
  x is right adjusted in a field Max(n,m) characters padded with
  blanks at the left end. A plus sign of the mantissa is not
  written.
*)
  VAR str: ARRAY 32 OF CHAR; len: LONGINT;
BEGIN
  LongStr.RealToFloat(x,15,str);
  ChangeReal(str,TRUE);
  len:=LENGTH(str);
  WHILE n > len DO Char(" "); DEC(n) END;
  String(str);
END LongReal;

PROCEDURE Ln*;
(** Writes an end-of-line symbol to the end of the output stream *)
BEGIN
  STextIO.WriteLn;
END Ln;

END Out.
