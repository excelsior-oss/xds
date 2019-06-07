(* Copyright (c) xTech 1993. All Rights Reserved. *)
IMPLEMENTATION MODULE LongIO; (* Andrew Cadach Aug 1993 *)

IMPORT IOChan, TextIO, IOConsts, LongStr, CharClass;
IMPORT ConvTypes, LongConv;

CONST ok = IOConsts.allRight;
      strHigh = 127;

TYPE float = LONGREAL;

TYPE string = ARRAY [0..strHigh] OF CHAR;

PROCEDURE ReadReal (cid: IOChan.ChanId; VAR real: float);
  VAR state: ConvTypes.ScanState;
      class: ConvTypes.ScanClass;
      res: IOConsts.ReadResults;
      conv: ConvTypes.ConvResults;
      str: string;
      ch: CHAR;
      n: CARDINAL;
      fit,read: BOOLEAN;
BEGIN
  n:=0; fit:=TRUE; read:=FALSE;
  state:=LongConv.ScanReal;
  LOOP
    IOChan.Look(cid,ch,res);
    IF res#ok THEN ch:=0C END;
    state(ch,class,state);
    CASE class OF
    |ConvTypes.padding:
       IOChan.Skip(cid); read:=TRUE;
    |ConvTypes.valid:
       IOChan.Skip(cid); read:=TRUE;
       IF n<strHigh THEN str[n]:=ch; INC(n) ELSE fit:=FALSE END;
    |ConvTypes.invalid:
      IF (res=ok) OR read THEN IOChan.SetReadResult(cid,IOConsts.wrongFormat) END;
      RETURN
    |ConvTypes.terminator:
       EXIT
    END;
  END;
  IF NOT fit THEN res:=IOConsts.wrongFormat
  ELSE
    str[n]:=0C;
    LongStr.StrToReal(str,real,conv);
    CASE conv OF
    |ConvTypes.strAllRight: res:=ok
    |ConvTypes.strOutOfRange: res:=IOConsts.outOfRange
    |ConvTypes.strWrongFormat: res:=IOConsts.wrongFormat
    END;
  END;
  IOChan.SetReadResult(cid,res);
END ReadReal;

PROCEDURE write (cid: IOChan.ChanId; VAR str: string; w: CARDINAL);
  VAR
    fill: string;
    n: CARDINAL;
BEGIN
  n := LENGTH (str);
  IF w=0 THEN w := 1;
  ELSIF n>=w THEN w := 0
  ELSE DEC (w, n)
  END;

  IF w#0 THEN
    n := w; IF n>strHigh THEN n := strHigh END;
    REPEAT DEC(n); fill[n] := ' ' UNTIL (n=0);
    n := w; IF n>=strHigh THEN n := strHigh-1 END;
    REPEAT
      IF n>w THEN n := w END; DEC(w,n); fill[n] := 0C;
      TextIO.WriteString (cid, fill);
    UNTIL (w=0);
  END;
  TextIO.WriteString (cid, str);
END write;

PROCEDURE WriteFloat (cid: IOChan.ChanId; real: float; sigFigs: CARDINAL; width: CARDINAL);
VAR str: string;
BEGIN
  LongStr.RealToFloat (real, sigFigs, str);
  write (cid, str, width);
END WriteFloat;

PROCEDURE WriteEng (cid: IOChan.ChanId; real: float; sigFigs: CARDINAL; width: CARDINAL);
VAR str: string;
BEGIN
  LongStr.RealToEng (real, sigFigs, str);
  write (cid, str, width);
END WriteEng;

PROCEDURE WriteFixed (cid: IOChan.ChanId; real: float; place: INTEGER; width: CARDINAL);
VAR str: string;
BEGIN
  LongStr.RealToFixed (real, place, str);
  write (cid, str, width);
END WriteFixed;

PROCEDURE WriteReal (cid: IOChan.ChanId; real: float; width: CARDINAL);

  PROCEDURE digsFix(VAR s: ARRAY OF CHAR): CARDINAL;
    VAR i,c: CARDINAL;
  BEGIN
    i:=0; c:=0;
    IF s[i]="-" THEN INC(i) END;
    WHILE (i<HIGH(s)) & ((s[i]="0") OR (s[i]=".")) DO INC(i) END;
    WHILE (i<HIGH(s)) & (s[i]#0C) DO
      IF s[i]#"." THEN INC(c) END;
      INC(i);
    END;
    RETURN c;
  END digsFix;

  PROCEDURE digsFlt(VAR s: ARRAY OF CHAR): CARDINAL;
    VAR i,c: CARDINAL;
  BEGIN
    i:=0; c:=0;
    IF s[i]="-" THEN INC(i) END;
    WHILE (i<HIGH(s)) & (s[i]#0C) & (s[i]#"E") DO
      IF s[i]#"." THEN INC(c) END;
      INC(i);
    END;
    RETURN c;
  END digsFlt;

  CONST sigFigs = 3;
  VAR str,fix: string; w: CARDINAL;
BEGIN
  IF ABS(real)>1.0 THEN
    IF LongConv.LengthFixedReal(real,sigFigs)<=width THEN
      LongStr.RealToFixed (real, sigFigs, str);
    ELSE
      LongStr.RealToFloat (real, sigFigs, str);
    END;
    write (cid, str, width);
  ELSE
    w:=width;
    IF real<0.0 THEN DEC(w) END;
    LongStr.RealToFixed (real, w-2, fix);
    LongStr.RealToFloat (real, w-2, str);
    IF digsFix(fix)>=digsFlt(str) THEN
      write (cid, fix, width);
    ELSE
      write (cid, str, width);
    END;
  END;
END WriteReal;

END LongIO.
