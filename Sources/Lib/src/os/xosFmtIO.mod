(* Copyright (c) xTech 1992,96.  All Rights Reserved *)
(*
  XDS RTS. Implementation of output formatting.
  OS independent.
*)
<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE xosFmtIO; (* Hady 29.05.96 19:45 *)

IMPORT  xrtsOS;
FROM  SYSTEM  IMPORT  CARD32;

CONST spaces = "        ";

PROCEDURE ["C"] outs(s-: ARRAY OF CHAR; len: CARD32);
BEGIN
  IF len=0 THEN RETURN END;
  xrtsOS.X2C_StdOut(s,len);
END outs;

PROCEDURE ["C"] X2C_StdOutS(s-: ARRAY OF CHAR; w: CARD32);
  VAR l: CARD32;
BEGIN
  l:=0; WHILE (s[l]#0C) DO INC(l) END;
  outs(s,l);
  IF w>l THEN
    WHILE (w-l>8) DO outs(spaces,8); INC(l,8) END;
    IF w>l THEN outs(spaces,w-l) END;
  END;
END X2C_StdOutS;

PROCEDURE ["C"] X2C_HexToStr(VAR s: ARRAY OF CHAR; VAR pos: CARD32; no: CARD32);
  VAR d,i: CARD32;
BEGIN
  pos:=pos+8;
  FOR i:=0 TO 7 DO
    d:=no MOD 10h;
    no:=no DIV 10h;
    DEC(pos);
    IF d>9 THEN
      s[pos]:=CHR(ORD("A")+d-10);
    ELSE
      s[pos]:=CHR(ORD("0")+d);
    END;
  END;
END X2C_HexToStr;

PROCEDURE ["C"] X2C_StdOutH(no: CARD32; w: CARD32);
  VAR buf: ARRAY [0..11] OF CHAR; pos: CARD32;
BEGIN
  pos:=0;
  X2C_HexToStr(buf,pos,no);
  IF w>8 THEN
    WHILE w>16 DO outs(spaces,8); DEC(w,8) END;
    IF w>8 THEN outs(spaces,w-8) END;
  END;
  outs(buf,8);
END X2C_StdOutH;

PROCEDURE ["C"] X2C_DecToStr(VAR s: ARRAY OF CHAR; VAR pos: CARD32; no: CARD32);
  VAR l,i: CARD32;
BEGIN
  i:=1000000000; l:=10;
  WHILE i>no DO i:=i DIV 10; DEC(l) END;
  IF l=0 THEN l:=1 END;
  pos:=pos+l;
  i:=pos;
  WHILE l>0 DO
    DEC(i);
    s[i]:=CHR(ORD("0")+(no MOD 10));
    no:=no DIV 10;
    DEC(l)
  END;
  ASSERT(no=0);
END X2C_DecToStr;

PROCEDURE ["C"] X2C_StdOutD(no: CARD32; w: CARD32);
  VAR buf: ARRAY [0..11] OF CHAR; pos: CARD32;
BEGIN
  pos:=0;
  X2C_DecToStr(buf,pos,no);
  IF w>pos THEN
    WHILE w-pos>8 DO outs(spaces,8); DEC(w,8) END;
    IF w>pos THEN outs(spaces,w-pos) END;
  END;
  outs(buf,pos);
END X2C_StdOutD;

END xosFmtIO.
