(** Oakwood Oberon-2 library *)
(** Copyright (c) xTech 1994,95. All Rights Reserved. *)
(* Implementation based on ISO M2 Library *)
MODULE In; (* Ned 16-Feb-95 *)

(** Formatted input of characters, numbers and strings.

  All operations except Open require Done = TRUE and guarantee
  (Done = TRUE and the result is valid) or (Done = FALSE).

  All operations except Char skip leanding blanks, tabs or
  end-of-line characters.

*)

(** Example:

  VAR i: INTEGER; ch: CHAR; r: REAL; s,n: ARRAY 32 OF CHAR;
  ...
  In.Open;
  In.Int(i); In.Char(ch); In.Real(r); In.String(s); In.Name(n);

Input stream:

  123*1.5       "abc"   Mod.Proc

Results:

  i = 123
  ch = "*"
  r = 1.5E0
  s = "abc"
  n = "Mod.Proc"
*)

IMPORT  CharClass, IOChan, IOConsts, StdChans;

CONST EOL = 0AX;

VAR
  Done*: BOOLEAN;
  inp  : IOChan.ChanId;

PROCEDURE Open*;
(** Sets the standard input stream as input source.
  Sets Done to TRUE.
*)
BEGIN
  Done:=TRUE;
  inp:=StdChans.InChan();
END Open;

PROCEDURE Skip;
  VAR res: IOConsts.ReadResults; ch: CHAR;
BEGIN
  IF Done THEN
    LOOP
      IOChan.Look(inp,ch,res);
      CASE res OF
        |IOConsts.allRight:
          IF CharClass.IsWhiteSpace(ch) THEN IOChan.Skip(inp)
          ELSE EXIT
          END;
        |IOConsts.endOfLine:  IOChan.Skip(inp)
        |IOConsts.endOfInput: Done:=FALSE; EXIT
      END;
    END;
  END;
END Skip;

PROCEDURE Char*(VAR ch: CHAR);
(** Returns the character ch at the current position
  (LF for the end-of-line character).
*)
  VAR res: IOConsts.ReadResults;
BEGIN
  IF Done THEN
    IOChan.Look(inp,ch,res);
    CASE res OF
      |IOConsts.allRight : IOChan.Skip(inp);
      |IOConsts.endOfLine: IOChan.Skip(inp); ch:=EOL;
      |IOConsts.endOfInput: Done:=FALSE; ch:=0X;
    END;
  END;
END Char;

PROCEDURE String*(VAR str: ARRAY OF CHAR);
(** Returns the string at the current position:
        string = '"' char {char} '"'.
  The string must not contain characters less than blank,
  such as EOL or TAB.
*)
  VAR ch: CHAR; i: LONGINT;
BEGIN
  Skip;
  IF Done THEN
    Char(ch);
    IF ch # '"' THEN Done:=FALSE
    ELSE
      i:=0;
      Char(ch);
      WHILE Done & (ch # '"') DO
        IF (ch < " ") OR (i >= LEN(str)) THEN Done:=FALSE; RETURN END;
        str[i]:=ch; INC(i);
        Char(ch);
      END;
      IF Done & (i = 0) THEN Done:=FALSE END;
      IF i < LEN(str) THEN str[i]:=0X END;
    END;
  END;
END String;

PROCEDURE Name*(VAR name: ARRAY OF CHAR);
(** Returns the name at the current position according to the
  file name format of the underlying operating system (e.g.
  "lib/MyMod" under Unix).

  In the current implementation the procedure copies 
  to "name" characters before next blank, tab or
  end-of-line character.
*)
  VAR res: IOConsts.ReadResults; ch: CHAR; i: LONGINT;
BEGIN
  Skip;
  IF Done THEN
    i:=0;
    LOOP
      IOChan.Look(inp,ch,res);
      IF (res # IOConsts.allRight) OR CharClass.IsWhiteSpace(ch) THEN 
        EXIT
      END;
      IOChan.Skip(inp);
      IF i < LEN(name)-1 THEN name[i]:=ch; INC(i) END;
    END;
    IF i < LEN(name) THEN name[i]:=0X END;
    Done:=(i # 0) & (i < LEN(name));
  END;
END Name;

(*------------------ Read Integers ---------------------*)

PROCEDURE ReadInt(VAR num: LONGINT);
  VAR string: ARRAY 64 OF CHAR;
      i,k,n: INTEGER;
      hex: BOOLEAN;
      ch,dig: CHAR;
      val: LONGINT;
      res: IOConsts.ReadResults;
BEGIN
  num:=0; val:=0; hex:=FALSE; i:=0;
  LOOP
    IOChan.Look(inp,ch,res);
    IF res # IOConsts.allRight THEN EXIT END;
    ch:=CAP(ch);
    IF (ch >= '0') & (ch <= '9') THEN
      dig:=CHR(ORD(ch)-ORD('0'));
    ELSIF (ch >= 'A') & (ch <= 'F') THEN
      dig:=CHR(ORD(ch)-ORD('A') + 10);
      hex:=TRUE;
    ELSE EXIT
    END;
    IOChan.Skip(inp);
    IF (i > 0) OR (ch # "0") THEN
      IF i < LEN(string) THEN string[i]:=dig; INC(i) END;
    END;
  END;
  IF (res = IOConsts.allRight) & (ch = 'H') THEN
    IOChan.Skip(inp);
    FOR k:=0 TO i-1 DO
      n:=ORD(string[k]);
      IF val <= (MAX(LONGINT) - n) DIV 16 THEN val:=val*16+n
      ELSE Done:=FALSE; RETURN
      END;
    END;
    num:=val;
  ELSIF hex THEN Done:=FALSE; num:=0
  ELSE
    FOR k:=0 TO i-1 DO
      n:=ORD(string[k]);
      IF val <= (MAX(LONGINT) - n) DIV 10 THEN val:=val*10+n
      ELSE Done:=FALSE; RETURN
      END;
    END;
    num:=val;
  END;
END ReadInt;

PROCEDURE IsNumeric(): BOOLEAN;
  VAR res: IOConsts.ReadResults; ch: CHAR;
BEGIN
  IOChan.Look(inp,ch,res);
  RETURN (res = IOConsts.allRight) & CharClass.IsNumeric(ch)
END IsNumeric;

PROCEDURE Int*(VAR n: INTEGER);
(** Returns the integer constant at the current position:
        IntConst = digit {digit} | digit {hexDigit} "H".
*)
  VAR x: LONGINT;
BEGIN
  Skip;
  IF Done THEN
    IF IsNumeric() THEN
      ReadInt(x);
      IF Done & (x <= MAX(INTEGER)) THEN n:=SHORT(x)
      ELSE Done:=FALSE
      END;
    ELSE Done:=FALSE
    END;
  END;
END Int;

PROCEDURE LongInt*(VAR n: LONGINT);
(** Returns the long integer constant n at the current position:
        IntConst = digit {digit} | digit {hexDigit} "H".
*)
BEGIN
  Skip;
  IF Done THEN
    IF IsNumeric() THEN ReadInt(n);
    ELSE Done:=FALSE;
    END;
  END;
END LongInt;

(*------------------ Read Reals ---------------------*)

PROCEDURE Ten(e: LONGINT; VAR ovf: BOOLEAN; VAR t: LONGREAL);
  VAR pow: LONGREAL;
BEGIN
  ASSERT(e >= 0);
  ovf:=FALSE;
  t := 1.0; pow := 10.0;
  WHILE e # 0 DO
    IF ODD(e) THEN
      IF MAX(LONGREAL) / pow <= t THEN ovf:=TRUE; RETURN END;
      t := t*pow;
    END;
    e := e DIV 2;
    IF MAX(LONGREAL) / pow >= pow THEN pow := pow*pow;
    ELSE
      IF e # 0 THEN ovf:=TRUE END;
      RETURN
    END;
  END;
END Ten;

PROCEDURE ReadReal(VAR real: LONGREAL; VAR long: BOOLEAN);
(* 1[23] "." [456] [("E"|"D") ["+"|"-"] 1[23]] *)
(* 1   1  2  2   2      3         4     5      *)
(* final states are: 1, 2, 5 *)

  CONST final = {1,2,5};

  VAR
    s, n: INTEGER;
    e: LONGINT;
    r, t, p: LONGREAL;
    neg: BOOLEAN;
    ch: CHAR;
    res: IOConsts.ReadResults;
    ovf: BOOLEAN;
BEGIN
  long := FALSE; neg := FALSE; ovf:=FALSE;
  e := 0; s := 0;
  r := 0.0; p := 1.0;
  LOOP
    IOChan.Look(inp,ch,res);
    IF res # IOConsts.allRight THEN EXIT END;
    CASE ch OF
    |'+', '-':
      IF s # 3 THEN EXIT END;
      neg := (ch = '-');
      s := 4;
    |'.':
      IF s#1 THEN EXIT END;
      e := 0;
      s := 2;
    |'E','D':
      IF s # 2 THEN s := 0; EXIT END;
      long := (ch = 'D');
      s := 3;
    |'0'..'9':
      n := ORD(ch) - ORD('0');
      CASE s OF
      |0,1:
        IF r >= (MAX(LONGREAL) - n)/10.0 THEN ovf:=TRUE END;
        IF ~ ovf THEN r := r*10.0 + n END;
        s := 1;
      |2:
        IF ~ ovf THEN p := p / 10.0; r := r + p*n END;
      |3..5:
        IF e >= (MAX(LONGINT) - n) DIV 10 THEN ovf:=TRUE END;
        IF ~ ovf THEN e := e*10 + n END;
        s := 5;
      ELSE EXIT
      END;
    ELSE EXIT
    END;
    IOChan.Skip(inp);
  END;
  IF ~ ovf & (s IN final) THEN
    Ten(e,ovf,t);
    IF ovf THEN Done:=FALSE; r:=0;
    ELSE
      IF neg THEN r := r / t;
      ELSE
        IF MAX(LONGREAL)/t <= r THEN Done:=FALSE; r:=0.
        ELSE r := r*t
        END;
      END;
      real:=r;
    END;
  ELSE Done:=FALSE; real:=0.
  END;
END ReadReal;

PROCEDURE Real*(VAR x: REAL);
(** Returns the real constant at the current position:
        RealConst = digit {digit}
            [ "." {digit} [ "E" ["+"|"-"] digit {digit} ]]
*)
  VAR lr: LONGREAL; max: REAL; long: BOOLEAN;
BEGIN
  Skip;
  IF Done THEN
    IF IsNumeric() THEN
      ReadReal(lr,long);
      max := MAX(REAL); (* work around the bug in x2 *)
      IF Done & ~ long & (lr <= max) THEN x:=SHORT(lr)
      ELSE Done:=FALSE
      END;
    ELSE Done:=FALSE
    END;
  END;
END Real;

PROCEDURE LongReal*(VAR x: LONGREAL);
(** Returns the long real constant at the current position:
        LongRealConst = digit {digit}
            [ "." {digit} [ ("D"|"E") ["+"|"-"] digit {digit} ]]
*)
  VAR long: BOOLEAN;
BEGIN
  Skip;
  IF Done THEN
    IF IsNumeric() THEN ReadReal(x,long);
    ELSE Done:=FALSE
    END;
  END;
END LongReal;

BEGIN
  inp:=StdChans.InChan();
END In.

