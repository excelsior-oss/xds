(* Copyright (c) Excelsior 2003-2005. All Rights Reserved *)

(* Routines to get information from Linux' /proc filesystem. *)

<*+ M2EXTENSIONS *>

IMPLEMENTATION MODULE xrnProc; (* Jek *)


IMPORT SYSTEM, fcntl, unistd, string;


CONST
  MEMINFO = "/proc/meminfo";
  BUFSIZE = 2048;


TYPE 
  PCHAR = POINTER TO CHAR;
  pARRAY_OF_CHAR = POINTER TO ARRAY OF CHAR;


PROCEDURE is_digit(ch: CHAR): BOOLEAN;
BEGIN
  RETURN (ORD(ch) >= ORD('0')) AND (ORD(ch) <= ORD('9'));
END is_digit;


PROCEDURE val(ch: CHAR): INTEGER;
BEGIN
  RETURN ORD(ch) - ORD('0');
END val;


PROCEDURE ["C"] / X2J_MUL64 (a, b: SYSTEM.INT64): SYSTEM.INT64;


PROCEDURE ["StdCall"] StrToInt64 (str: ARRAY OF CHAR): SYSTEM.INT64;
VAR
  i, j, degree      :INTEGER;
  first_digit       :INTEGER;
  last_digit        :INTEGER;
  boundary          :INTEGER;
  power32, result32 :INTEGER;
  power64, result64 :SYSTEM.INT64;
  tmp64             :SYSTEM.INT64;
BEGIN
  i := 0;

  (* skip non-digits *)
  WHILE (str[i] # 0C) AND (NOT is_digit(str[i])) DO
    INC(i);
  END;

  IF str[i] = 0C THEN
    RETURN 0;
  END;

  first_digit := i;

  (* count digits *)
  j := i+1;

  WHILE is_digit(str[j]) DO
    INC(j);
  END;

  degree := j - i;
  
  last_digit := j-1;

  (* 9 = [log  (7FFFFFFF)]
             10
  *)

  IF degree > 9 THEN
    (* skip low 9 digits to process them later in order
       to improve performance
    
       xxxxxxLLLLLLLLL 
       ^    ^^       ^
       |    ||       |
       |    i|       |
       |   boundary  |
     first_digit   last_digit
    *)                           

    boundary := first_digit + (degree - 10) + 1; 
    i := boundary - 1;

    power64 := 1000000000; (* billion *)
    result64 := X2J_MUL64(power64, val(str[i]));

    DEC(i);

    WHILE i >= first_digit DO
      power64 := X2J_MUL64(power64, 10);
      result64 := result64 + X2J_MUL64(power64, val(str[i]));

      DEC(i);
    END;
  ELSE
    boundary := first_digit; 
    result64 := 0;
  END;

  i := last_digit;

  result32 := val(str[i]);
  power32 := 10; (* ten *)

  DEC(i);

  WHILE i >= boundary DO
    result32 := result32 + val(str[i]) * power32;
    power32 := power32 * 10;

    DEC(i);
  END;

  RETURN result64 + result32;
END StrToInt64;


VAR
  meminfoBuf :ARRAY[0..BUFSIZE-1] OF CHAR;


(* This is identcal to $JETDev/runtime/os/Linux/xrnProc::meminfo. *)
PROCEDURE meminfo(VAR info: GlobalMemInfo);
CONST
  newline = ""+12C;
VAR
  memfile : INTEGER;
  ch      : PCHAR;
  rd      : INTEGER;

  PROCEDURE is_digit(ch: CHAR): BOOLEAN;
  BEGIN
    RETURN (ORD(ch) >= ORD('0')) AND (ORD(ch) <= ORD('9'));
  END is_digit;

  PROCEDURE skip_nondigits();
  BEGIN
    WHILE (ch^ # 0C) AND (NOT is_digit(ch^)) DO
      ch:=ch+SIZE(ch^);
    END;
  END skip_nondigits;

BEGIN
  memfile := fcntl.open(MEMINFO, fcntl.O_RDONLY);
  ASSERT(memfile # -1, 96013);

  unistd.lseek(memfile, 0, unistd.SEEK_SET);
  rd := unistd.read(memfile, SYSTEM.ADR(meminfoBuf[0]), BUFSIZE);
  unistd.close(memfile);

  ASSERT(rd > 0, 96016);
  ASSERT(rd < BUFSIZE, 96017);

  meminfoBuf[rd] := 0C;

  ------------------------------------------------------------------------------
  
  ch := string.strstr(meminfoBuf, "MemTotal");
  ASSERT(ch # NIL, 96018);
  skip_nondigits;
  ASSERT(ch^ # 0C, 96019);
  info.totalPhys := StrToInt64(SYSTEM.CAST(pARRAY_OF_CHAR, ch)^);

  ------------------------------------------------------------------------------
  
  ch := string.strstr(meminfoBuf, "MemFree");
  ASSERT(ch # NIL, 96020);
  skip_nondigits;
  ASSERT(ch^ # 0C, 96021);
  info.freePhys := StrToInt64(SYSTEM.CAST(pARRAY_OF_CHAR, ch)^);

  ------------------------------------------------------------------------------
  
  ch := string.strstr(meminfoBuf, "Buffers");
  ASSERT(ch # NIL, 96022);
  skip_nondigits;
  ASSERT(ch^ # 0C, 96023);
  info.buffers := StrToInt64(SYSTEM.CAST(pARRAY_OF_CHAR, ch)^);

  ------------------------------------------------------------------------------

  ch := string.strstr(meminfoBuf, "Cached");
  ASSERT(ch # NIL, 96024);
  skip_nondigits;
  ASSERT(ch^ # 0C, 96025);
  info.cached := StrToInt64(SYSTEM.CAST(pARRAY_OF_CHAR, ch)^);

END meminfo;


PROCEDURE GetExeName(VAR name: ARRAY OF CHAR; len: INTEGER);
VAR
  stored: INTEGER;
BEGIN
  ASSERT(len > 0);

  stored := unistd.readlink("/proc/self/exe", name, len);
  
  IF stored = -1 THEN
    name[0] := 0C;
    RETURN;
  ELSIF stored >= len THEN
    stored := len-1;
  END;
  
  name[stored] := 0C;
END GetExeName;


PROCEDURE GetCmdLine(VAR cmdline: ARRAY OF CHAR; len: INTEGER);
VAR
  f, i : INTEGER;
  read : INTEGER;
BEGIN
  ASSERT(len > 0);

  f := fcntl.open("/proc/self/cmdline", fcntl.O_RDONLY);
  ASSERT(f # -1);
  
  read := unistd.read(f, SYSTEM.ADR(cmdline[0]), len-1);
  
  IF read < 0 THEN (* :-/ *)
    cmdline[0]:=0C;
    RETURN
  END;
  
  FOR i:=0 TO read-1 DO
    IF cmdline[i] = 0C THEN
      cmdline[i] := ' ';
    END;
  END;
  
  (* The last read character is 0C. *)
END GetCmdLine;

END xrnProc.
