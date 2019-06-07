(* Copyright (c) xTech 1993,95. All Rights Reserved. *)
<* IF USE_CLIBS THEN *> <*+ M2EXTENSIONS *> <* END *>
IMPLEMENTATION MODULE CharClass;

(* Modifications:
   22-Mar-94 Ned: merging implementations
*)

<* IF USE_CLIBS THEN *>
  IMPORT  SYSTEM, xPOSIX;
<* ELSE *>
  CONST TAB = 11C;       (* for other targets *)
<* END *>

PROCEDURE IsNumeric(ch: CHAR): BOOLEAN;
BEGIN
  RETURN (ch>='0') & (ch<='9');
END IsNumeric;

PROCEDURE IsLetter(ch: CHAR): BOOLEAN;
BEGIN
<* IF USE_CLIBS THEN *>
  RETURN xPOSIX.isalpha(VAL(SYSTEM.INT16,ch)) # 0
<* ELSE *>
  RETURN (ch>='a') & (ch<='z') OR (ch>='A') & (ch<='Z')
<* END *>
END IsLetter;

PROCEDURE IsUpper(ch: CHAR): BOOLEAN;
BEGIN
<* IF USE_CLIBS THEN *>
  RETURN xPOSIX.isupper(VAL(SYSTEM.INT16,ch)) # 0
<* ELSE *>
  RETURN (ch>='A') & (ch<='Z')
<* END *>
END IsUpper;

PROCEDURE IsLower(ch: CHAR): BOOLEAN;
BEGIN
<* IF USE_CLIBS THEN *>
  RETURN xPOSIX.islower(VAL(SYSTEM.INT16,ch)) # 0
<* ELSE *>
  RETURN (ch>='a') & (ch<='z')
<* END *>
END IsLower;

PROCEDURE IsControl(ch: CHAR): BOOLEAN;
BEGIN
<* IF USE_CLIBS THEN *>
  RETURN xPOSIX.iscntrl(VAL(SYSTEM.INT16,ch)) # 0
<* ELSE *>
  RETURN (ORD(ch) MOD 200B)<=37B
<* END *>
END IsControl;

PROCEDURE IsWhiteSpace(ch: CHAR): BOOLEAN;
BEGIN
<* IF USE_CLIBS THEN *>
  RETURN xPOSIX.isspace(VAL(SYSTEM.INT16,ch)) # 0
<* ELSE *>
  RETURN (ch=' ') OR (ch=TAB);
<* END *>
END IsWhiteSpace;

END CharClass.
