(* Copyright (C) 1998 XDS Ltd. *)

<*+M2ADDTYPES   *>
<*+M2EXTENSIONS *>

<*-IOVERFLOW    *>
<*-COVERFLOW    *>
<*-CHECKINDEX   *>
<*-CHECKRANGE   *>
<*-CHECKNIL     *>
<*+STORAGE      *>

IMPLEMENTATION MODULE BiosIO;

IMPORT SYSTEM, IO, xtsEvQue;

(* -------------------------------------------- *)
PROCEDURE KeyPressed() : BOOLEAN ;
(* -------------------------------------------- *)
(* Check if key pressed *)
VAR char, scan : SYSTEM.CARD8;
BEGIN
  RETURN xtsEvQue.peekKey(char, scan);
END KeyPressed;



(* -------------------------------------------- *)
PROCEDURE RdKey() : CHAR ;
(* -------------------------------------------- *)
(* Return keys without echo *)
VAR
  char, scan : SYSTEM.CARD8;
BEGIN
  IF LastChar = 0C THEN
    LastChar := CHAR(0FFH);
    RETURN CHR(LastScan);
  END;

  IF xtsEvQue.getKey(char, scan, TRUE) THEN
    LastChar := CHR(char);
    LastScan := SHORTCARD(scan);
  ELSE
    LastChar := CHR(0FFH);
  END;
  RETURN LastChar;
END RdKey;



(* -------------------------------------------- *)
PROCEDURE RdChar () : CHAR ;
(* -------------------------------------------- *)
(* reads and echoes char (if not extended) *)
VAR C : CHAR ;
BEGIN
  IF LastChar = CHR(0) THEN RETURN RdKey() END ;
  C := RdKey() ;
  IF C<>CHR(0) THEN
    IO.WrChar( C );
  END ;
  RETURN C ;
END RdChar ;



(* -------------------------------------------- *)
PROCEDURE KBFlags () : KBFlagSet;
(* -------------------------------------------- *)
(* RShift, LShift, Ctrl, Alt, Scroll, Num, Cap, Ins *)
BEGIN
  RETURN KBFlagSet(xtsEvQue.getKbf());
END KBFlags;


BEGIN
  LastChar     := CHAR(0FFH);
  LastScan     := 0;
  EnhancedBIOS := TRUE;
END BiosIO.
