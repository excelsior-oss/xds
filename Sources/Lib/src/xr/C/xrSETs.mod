<*- checkindex  *>
<*- checkdindex *>
<*- checknil    *>
<*- checkset    *>
<*- checkrange  *>
<*- ioverflow   *>
<*- coverflow   *>
<*- foverflow   *>
<*- gendebug    *> (* Don enable! History would not work (SYSTEM.CODE) *)
<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE xrSETs;

FROM SYSTEM    IMPORT  INT32, INT16, SET32;
FROM SYSTEM    IMPORT  CODE;

PROCEDURE [2] X2C_ASH(a[2]: INT32; b[2]: INT32): INT32;
BEGIN
  CODE("return (b >= 0) ? (a << b) : (a >> (-b));");
  RETURN 0;
END X2C_ASH;

PROCEDURE [2] X2C_ROT (a[2]: SET32; length[2]: INT16; n[2]: INT32): SET32;
  VAR m: SET32;
BEGIN
  m:=SET32{};
  CODE("m = (length==32) ? 0xFFFFFFFFl : (1l << length)-1;");
  IF n > 0 THEN
    CODE("n=n % length;");
    CODE("return ((a << n) | (a >> (length - n))) & m;");
  ELSE
    CODE("n= -n % length;");
    CODE("return ((a >> n) | (a << (length - n))) & m;");
  END;
  RETURN SET32{};
END X2C_ROT;

PROCEDURE [2] X2C_LSH (a[2]: SET32; length[2]: INT16; n[2]: INT32): SET32;
  VAR m: SET32;
BEGIN
  m:=SET32{};
  CODE("m = (length==32) ? 0xFFFFFFFFl : (1l << length)-1;");
  IF n > 0 THEN
    IF n>=length THEN RETURN SET32{} END;
    CODE("return (a << n) & m;");
  ELSE
    IF n<=-length THEN RETURN SET32{} END;
    CODE("return (a >> -n) & m;");
  END;
  RETURN SET32{};
END X2C_LSH;

END xrSETs.
