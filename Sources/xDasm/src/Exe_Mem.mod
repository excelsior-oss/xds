IMPLEMENTATION MODULE Exe_Mem;

IMPORT 
  sys:= SYSTEM,
  kt:= KrnTypes;

PROCEDURE Get(source: kt.ADDRESS; dest: sys.ADDRESS; len: CARDINAL): BOOLEAN;
BEGIN
  ASSERT(data # NIL);
  IF (source + len - 1) < VAL(CARDINAL, LEN(data^)) THEN
    sys.MOVE(sys.ADR(data^[source]), dest, len);
    RETURN TRUE;
  ELSE
    RETURN FALSE;
  END;
END Get;

PROCEDURE GetReg(regno: CARDINAL; VAR value: kt.REG_VALUE): BOOLEAN;
BEGIN 
  RETURN FALSE; 
END GetReg;

PROCEDURE GetFlags(VAR flags: kt.FLAGS): BOOLEAN;
BEGIN
  RETURN FALSE;
END GetFlags;

END Exe_Mem.
