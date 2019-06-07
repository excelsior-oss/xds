(* Long Strings Routines ***  al 23-Oct-95  *)

MODULE LongStrs;

IMPORT	WholeStr,
  LongStr,
  ST := Strings,
  SYS := SYSTEM,
  io:= Printf;

CONST
  SPACE* = 20X;
  TAB* = 09X;
  none	* = -1;

TYPE
  INT* = SYS.INT32;
  float = LongStr.float;
  String* = POINTER TO ARRAY OF CHAR;

CONST
  segment_size = 10;
  short_string_size = 100;

TYPE
  StringHolder = POINTER TO StringHolderDesc;
  StringHolderDesc = RECORD
    str: String;
    seg_len: INT;
    next: StringHolder;
  END;

  StringStoragePart = RECORD
    putptr, getptr: StringHolder;
  END;


VAR LongStringStorage: StringHolder;
    StringStorage: ARRAY short_string_size OF StringStoragePart;
    __str: String;
    junk: StringHolder;

PROCEDURE Deallocate * (VAR s: String);
VAR sh: StringHolder;
    seg_len: INT;
BEGIN
  IF s = NIL THEN RETURN END;
  seg_len:= LEN(s^) DIV segment_size;
  IF seg_len > short_string_size THEN
    sh:= LongStringStorage;
    WHILE (sh # NIL) & (sh.str # NIL) DO sh:= sh.next END;
    IF sh = NIL THEN
      NEW(sh);
      sh.next:= LongStringStorage;
      LongStringStorage:= sh;
    END;
    sh.str:= s;
    sh.seg_len:= seg_len;
  ELSE
    IF StringStorage[seg_len-1].putptr.next = StringStorage[seg_len-1].getptr THEN
      NEW(sh);
      sh.next:= StringStorage[seg_len-1].getptr;
      StringStorage[seg_len-1].putptr.next:= sh;
    END;
    StringStorage[seg_len-1].putptr.str:= s;
    StringStorage[seg_len-1].putptr:= StringStorage[seg_len-1].putptr.next;
  END;
  s:= NIL;
END Deallocate;

PROCEDURE Allocate * (VAR s: String; len: INT);
VAR sh: StringHolder;
    seg_len, i: INT;
BEGIN
  sh:= NIL;
  seg_len:= len DIV segment_size + 1;
  IF s # NIL THEN
    IF LEN(s^) DIV segment_size >= seg_len THEN
      RETURN;
    ELSE
      Deallocate(s);
    END;
  END;
  IF seg_len > short_string_size THEN
    sh:= LongStringStorage;
    WHILE (sh # NIL) & (sh.seg_len < seg_len) DO sh:= sh.next END;
  ELSE
    i:= seg_len - 1;
    WHILE (i < short_string_size) & (StringStorage[i].getptr = StringStorage[i].putptr) DO
      INC(i);
    END;
    IF (i < short_string_size) THEN
      sh:= StringStorage[i].getptr;
      StringStorage[i].getptr:= StringStorage[i].getptr.next;
    END;
  END;
  IF sh = NIL THEN
    NEW(s, seg_len * segment_size);
  ELSE
    s:= sh.str;
    sh.str:= NIL;
    sh.seg_len:= 0;
  END;
END Allocate;


(*
   All theose routines have the property that if present
   destination parameter, it will be reallocated for having
   need size including 0X terminator.
*)

PROCEDURE Length* ( s-: ARRAY OF CHAR ): INT;
BEGIN
  RETURN LENGTH(s);
END Length;

PROCEDURE Length1* ( s: String ): INT;
BEGIN
  IF s = NIL THEN RETURN 0
  ELSE RETURN LENGTH(s^) END
END Length1;

PROCEDURE MakeDest ( VAR d: String; ReqSize: INT );
VAR x: String;
BEGIN
  x:= d;
  Allocate(d, ReqSize);
  IF x # d THEN
    d[0] := 0X;
    IF x # NIL THEN
      COPY( x^, d^ );
    END;
  END;
END MakeDest;

(*
   Copy from source to destination
*)

PROCEDURE Assign* (s-: ARRAY OF CHAR; VAR d: String );
VAR l, i: INT;
BEGIN
  IF d # NIL THEN
    l:= LEN(d^);
    i:= 0;
    WHILE (i < l) & (s[i] # 0X) DO d[i]:= s[i]; INC(i) END;
    IF i < l THEN
      d[i]:= 0X;
    ELSE
      Allocate( d, Length( s ) + 1 );
      COPY(s, d^);
    END;
  ELSE
    Allocate( d, Length( s ) + 1 );
    COPY(s, d^);
  END;
END Assign;

(*
   Copies at most numberToExtract characters from source to destination,
   starting at position startIndex in source.
*)

PROCEDURE Extract* ( s-: ARRAY OF CHAR;
		     startIndex, numberToExtract: INT; VAR d: String );
BEGIN
  MakeDest( d, numberToExtract + 1 );
  ST.Extract( s, startIndex, numberToExtract, d^ );
END Extract;

(*
   Inserts source into destination at position startIndex
*)

PROCEDURE Insert* (s-: ARRAY OF CHAR; startIndex: INT; VAR d: String);
BEGIN
  MakeDest(d, Length(s) + Length1(d) + 1);
  ST.Insert(s, startIndex, d^);
END Insert;

PROCEDURE Delete* (VAR stringVar: ARRAY OF CHAR; startIndex, numberToDelete: INT);
  (* Deletes at most numberToDelete characters from stringVar, starting at position
     startIndex.
  *)
BEGIN
  ST.Delete(stringVar, startIndex, numberToDelete);
END Delete;


(*
   Appends source to destination.
*)


PROCEDURE Append * ( s-: ARRAY OF CHAR; VAR d: String );
VAR l, k, i, j: INT;
    x: String;
BEGIN
  IF d # NIL THEN
    l:= LEN(d^); i:= 0; j:= 0;
    k:= LEN(s);
    WHILE (i < l) & (d[i] # 0X) DO INC(i) END;
    IF i < l THEN
      WHILE (i < l) & (j < k) & (s[j] # 0X) DO d[i]:= s[j]; INC(i); INC(j) END;
      IF i < l THEN
	d[i]:= 0X;
      ELSE
        Allocate(x, Length(s) + Length(d^) + 1);
	d^[l-1]:= 0X;
	COPY(d^, x^); Deallocate(d); d:= x;
	DEC(i); DEC(j);
        WHILE (j < k) & (s[j] # 0X) DO d[i]:= s[j]; INC(i); INC(j) END;
        d[i]:= 0X;
      END;
    ELSE
      io.printf("Internal error in LongStrs\n");
      HALT(1);
    END;
  ELSE
    Allocate(d, Length( s ) + 1);
    COPY(s, d^);
  END;
END Append;


(*


Old implementation


PROCEDURE Append* ( s-: ARRAY OF CHAR; VAR d: String );
TYPE pStr = POINTER TO ARRAY 10000 OF CHAR;
VAR len: INT;
    a  : SYS.ADDRESS;
    p  : pStr;
BEGIN
  len:= Length1(d);
  MakeDest(d, LENGTH(s) + len + 1);
  a:= SYS.ADDADR(SYS.ADR(d^), len);
  p:= SYS.VAL(pStr, a);
  COPY(s, p^);
  (*
  ST.Append( s, d^ );
  *)
END Append;

*)

(*
    Concatenates s2 onto s1 and copies the result into destination.
*)

PROCEDURE Concat* ( s1-, s2-: ARRAY OF CHAR; VAR d: String );
BEGIN
  MakeDest( d, Length( s1 ) + Length( s2 ) + 1 );
  ST.Concat( s1, s2, d^ );
END Concat;

(*
    Appends character to the end of string.
*)
PROCEDURE AppendChar* ( ch: CHAR; VAR d: String );
VAR str: ARRAY 2 OF CHAR;
BEGIN
  str[0]:= ch; str[1]:= 0X;
  Append(str, d);
END AppendChar;

(*
    Removes all blanks - SPACE & TABs.
*)

PROCEDURE CutSpace* ( VAR s: ARRAY OF CHAR );
VAR
  i, n: INTEGER;
BEGIN
  i := 0;
  n := 0;
  WHILE ( i < LEN( s )) & ( s[i] # 0X ) DO
    IF ( s[i] # SPACE ) & ( s[i] # TAB ) THEN s[n] := s[i]; INC( n ) END;
    INC( i )
  END;
  IF n < LEN( s ) THEN s[n] := 0X END
END CutSpace;

(*
    Applies the function CAP to each character of the string value in s.
*)

PROCEDURE Capitalize * ( VAR s: ARRAY OF CHAR );
BEGIN
  ST.Capitalize( s );
END Capitalize;

(*-----------------------------------------------------------------------*)
PROCEDURE FindPos * ( str-: ARRAY OF CHAR; ch: CHAR ): INT;
(* Find position of sumbol _ch_ in string _str_  *)
VAR pos, size: INT;
BEGIN
  pos:= 0;
  size:= Length( str );
  LOOP
    IF pos = size THEN RETURN none;
    ELSIF str[pos] = ch THEN RETURN pos;
    ELSE INC( pos );
    END;
  END;
END FindPos;

(*--------------------------------------------------------------------*)
PROCEDURE BackFindPos * ( str-: ARRAY OF CHAR; ch: CHAR): INT;
VAR size, pos: INT;
BEGIN
  size:= Length( str );
  pos:= size-1;
  LOOP
    IF pos < 0 THEN RETURN none;
    ELSIF str[pos] = ch THEN RETURN pos;
    ELSE DEC( pos );
    END;
  END;
END BackFindPos;

(*--------------------------------------------------------------------*)

PROCEDURE Fill * ( VAR str: String; ch: CHAR; number: INT );
(* Create string 'str' consisting of 'ch' sumbols 'number' time*)
BEGIN
  Assign('', str);
  IF number > 0 THEN
    WHILE number > 0 DO
      AppendChar( ch, str );
      DEC(number);
    END;
  END;
END Fill;

(*--------------------------------------------------------------------*)
PROCEDURE IntToStr * ( int: INT; VAR str: String );
VAR intStr: String;
BEGIN
  NEW( intStr, (int DIV 10)+2 );
  WholeStr.IntToStr( int, intStr^ );
  Append( intStr^, str );
END IntToStr;

PROCEDURE RealToStr * (real: float; VAR str: String);
VAR pos: INT;
    a: ARRAY 32 OF CHAR;
BEGIN
  LongStr.RealToStr(real, a);
  Assign( a, str );
  IF FindPos(str^, '.') = none THEN
    pos:= FindPos(str^, 'E');
    IF pos = none THEN  pos:= FindPos(str^, 'e')  END;
    IF pos = none THEN Append('.0', str);
    ELSE Insert('.0', pos, str);
    END;
  END;
END RealToStr;

VAR i: INT;

BEGIN
  LongStringStorage:= NIL; junk:= NIL;
  __str:= NIL;
  FOR i:= 0 TO short_string_size-1 DO
    NEW(StringStorage[i].putptr);
    NEW(StringStorage[i].putptr.next);
    StringStorage[i].putptr.next.next:= StringStorage[i].putptr;
    StringStorage[i].getptr:= StringStorage[i].putptr;
  END;
END LongStrs.

