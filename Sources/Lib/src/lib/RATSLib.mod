<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE RATSLib;

IMPORT SYSTEM, FormOut, platform;
IMPORT  xrtsOS;

PROCEDURE ok;
BEGIN
  xrtsOS.X2C_StdOut("OK",2);
  xrtsOS.X2C_StdOutN;
END ok;

PROCEDURE Success;
BEGIN
  ok;
  HALT(0);
END Success;

PROCEDURE fail;
BEGIN
  xrtsOS.X2C_StdOut("*** FAIL ***",12);
  xrtsOS.X2C_StdOutN;
END fail;

PROCEDURE Failure;
BEGIN
  fail;
  HALT(1);
END Failure;

<* IF __GEN_C__ THEN *>
PROCEDURE [3] SUCCESS*; (* for sl1 *)
BEGIN
  Success;
END SUCCESS;

PROCEDURE [3] FAILURE*; (* for sl1 *)
BEGIN
  Failure;
END FAILURE;
<* END *>

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

PROCEDURE write(x: SYSTEM.ADDRESS; s-: ARRAY OF CHAR; len: INTEGER);
BEGIN
  x:=x;
  xrtsOS.X2C_StdOut(s,len);
END write;

PROCEDURE print(fmt-: ARRAY OF CHAR; SEQ x: SYSTEM.BYTE);
BEGIN
  FormOut.format(NIL,write,fmt,FormOut.default,SYSTEM.ADR(x),SIZE(x));
END print;

BEGIN
  FormOut.LineSeparator(platform.lineSep);
  FormOut.TextSeparator(platform.textSep);
END RATSLib.
