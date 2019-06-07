IMPLEMENTATION MODULE Krn_Dasm;

<* IF TARGET_VAX THEN *>
IMPORT dsm := Dasm_VAX;
<* END *>

<* IF TARGET_x86 THEN *>
IMPORT dsm := Dasm_x86;
<* END *>

<* IF TARGET_m68k THEN *>
IMPORT dsm := Dasm_68k;
<* END *>

<* IF TARGET_PPC THEN *>
IMPORT dsm := Dasm_PPC;
<* END *>

IMPORT kt := KrnTypes;


-- get_prefix_only - получить только ПЕРВЫЙ префикс команды
PROCEDURE Disasm (addr: kt.ADDRESS; curr: BOOLEAN; VAR asm, info: ARRAY OF CHAR; VAR len: CARDINAL; get_prefix_only := FALSE: BOOLEAN): BOOLEAN;
BEGIN
  RETURN dsm.Disasm (addr, curr, asm, info, len, get_prefix_only);
END Disasm;


PROCEDURE IsCall (addr: kt.ADDRESS): CARDINAL;
BEGIN
  RETURN dsm.IsCall (addr);
END IsCall;


<* IF DEST_XDS THEN *>

PROCEDURE IsRet(addr: kt.ADDRESS): BOOLEAN;
BEGIN
  RETURN dsm.IsRet (addr);
END IsRet;

PROCEDURE IsJmpForDll (addr: kt.ADDRESS): kt.ADDRESS;
BEGIN
  RETURN dsm.IsJmpForDll (addr);
END IsJmpForDll;

<* END *>

END Krn_Dasm.
