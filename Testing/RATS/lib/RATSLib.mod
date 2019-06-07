<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE RATSLib;

IMPORT SYSTEM;
<* IF __GEN_X86__ THEN *>
IMPORT FormOut, platform;

-- На x86 исключение для плавающих чисел вываливается, не на текущей, а
-- только на следующей плавающей команде 
VAR last_real_op: REAL;
<* END *>


PROCEDURE ok;
BEGIN
  print("RTSLib: OK\n");
END ok;

PROCEDURE Success;
BEGIN
<* IF __GEN_X86__ THEN *>
  last_real_op := last_real_op / 2.0; 
<* END *>
  ok;
  HALT(0);
END Success;

PROCEDURE fail;
BEGIN
  print("RTSLib: FAIL\n");
END fail;

PROCEDURE Failure;
BEGIN
<* IF __GEN_X86__ THEN *>
  last_real_op := last_real_op / 2.0; 
<* END *>
  fail;
  HALT(15);
END Failure;

PROCEDURE Assert(v: BOOLEAN);
BEGIN
  IF NOT v THEN Failure END;
END Assert;

PROCEDURE OkIf(v: BOOLEAN);
BEGIN
  IF v THEN Success
  ELSE Failure
  END;
END OkIf;

BEGIN
 <* IF __GEN_X86__ THEN *>
  FormOut.LineSeparator(platform.lineSep);
  FormOut.TextSeparator(platform.textSep);
  last_real_op := 0.0;
 <* END *>
END RATSLib.
