(* Copyright (c) xTech 1995,97.  All Rights Reserved *)

<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xrnShowHistory;

IMPORT
   SYSTEM
  ,xmRTS
  ,xosDLL
  ,X2C
  ,OS:=xrtsOS
  ,face:=xrHistory
  ,xrnStkScan
  ,xosMalloc
  ,cseg:=xosCodeSeg
  ;
  
FROM SYSTEM IMPORT ADDRESS;


CONST
  malloc = xosMalloc.X2C_gmalloc;
  free   = xosMalloc.X2C_gfree;

CONST
  HisFileName = "errinfo.$$$";

CONST
  can_not_create    = "#RTS: Can't create file " + HisFileName;
  his_file_created  = "File " + HisFileName + " created.";
  close_error       = "#RTS: Close file " + HisFileName + " error.";
  history_truncated = "#RTS: History stack was truncated: not enough room.";

(* File IO *)
TYPE
  FILE = OS.X2C_OSFILE;
  STRING = ARRAY [0..255] OF CHAR;

CONST
  BADFILE = FILE(NIL);

VAR errfile : FILE;

PROCEDURE X2C_OPEN_ERRINFO(name-: ARRAY OF CHAR): INTEGER;
  VAR res: INTEGER;
BEGIN
  res := OS.X2C_FileOpenWrite(errfile, name);
  IF res # 0 THEN
    errfile := BADFILE;
  END;
  RETURN res;
END X2C_OPEN_ERRINFO;

PROCEDURE X2C_CLOSE_ERRINFO;
BEGIN
  IF errfile # BADFILE THEN
    IF OS.X2C_FileClose(errfile)=0 THEN END;
    errfile := BADFILE;
  END;
END X2C_CLOSE_ERRINFO;


(* Character output *)

PROCEDURE outs(s-: ARRAY OF CHAR; len: CARDINAL);
BEGIN
  IF (len#0) & (errfile#BADFILE) THEN
    IF OS.X2C_FileWrite(errfile, SYSTEM.ADR(s),len) # 0 THEN
      IF OS.X2C_FileClose(errfile) = 0 THEN END;
      errfile := BADFILE;
    END;
  END;
END outs;

CONST
   spaces = "        ";
   crlf   = ""+15C+12C;

PROCEDURE newln;
BEGIN
  outs(crlf,2);
END newln;

PROCEDURE string(s-: ARRAY OF CHAR; w: CARDINAL);
  VAR
    l :CARDINAL;
BEGIN
  l:=0;
  WHILE (l<=HIGH(s)) & (s[l]#0C) DO INC(l) END;
  outs(s,l);
  IF w>l THEN
    WHILE (w-l>8) DO outs(spaces,8); INC(l,8) END;
    IF w>l THEN outs(spaces,w-l) END;
  END;
END string;

PROCEDURE hextostr(VAR s: ARRAY OF CHAR; pos: CARDINAL; no: CARDINAL);
  VAR d,i: CARDINAL;
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
END hextostr;

PROCEDURE hexidec(no: CARDINAL; w: CARDINAL);
  VAR buf: ARRAY [0..11] OF CHAR;
BEGIN
  hextostr(buf,0,no);
  IF w>8 THEN
    WHILE w>16 DO outs(spaces,8); DEC(w,8) END;
    IF w>8 THEN outs(spaces,w-8) END;
  END;
  outs(buf,8);
END hexidec;

(* Show History *)

PROCEDURE ["C"] X2C_show;

  CONST
    xnmlen  = 256;
  TYPE
    pSTRING  = POINTER TO STRING;

    ARGV = POINTER TO ARRAY [0..1000] OF
           POINTER TO ARRAY [0..xnmlen-1] OF CHAR;
  VAR
    i,len   :INTEGER;
    fuflo   :ARGV;
    fuflo1  :pSTRING;
    exename :ARRAY [0..xnmlen-1] OF CHAR;
    current :xmRTS.X2C_Coroutine;

BEGIN
  OS.X2C_StdOutN;
  IF X2C_OPEN_ERRINFO(HisFileName) # 0 THEN
    OS.X2C_StdOut(can_not_create, LENGTH(can_not_create));
    OS.X2C_StdOutN;
    RETURN
  END;
  OS.X2C_StdOut(his_file_created, LENGTH(his_file_created));
  OS.X2C_StdOutN;

  current := xmRTS.X2C_GetCurrent();
  WITH current^ DO
    fuflo := ARGV(X2C.X2C_argv);
    IF (fuflo # NIL) THEN
      string(fuflo^[0]^, 0); string(" ", 0);
    ELSE
      string("???", 0);
    END;
    (* NOTE: argv may not be used to substitute commang line
       because an argv entry may contain blanks in the string!!!
     *)
    IF (X2C.X2C_sysarg # NIL) THEN
      fuflo1 := pSTRING(X2C.X2C_sysarg);
      string(fuflo1^, 0);
    END;
    newln;
    IF (his_msg[0] # 0C) THEN
      string(his_msg, LENGTH(his_msg) );
      newln;
    END;
    i   := 0;
    len := his_cnt; IF len>xmRTS.X2C_HIS_LEN THEN len := xmRTS.X2C_HIS_LEN END;
    WHILE (i < len) DO
      hexidec(his[i].ofs, 8); outs ("  ", 2);           (* codeseg offset    *)
      hexidec(his[i].sec, 8); outs ("  ", 2);           (* # of code section *)
      xosDLL.X2C_GetModuleName ( his[i].hmod, exename, xnmlen );
      string(exename, 0);
      newln;
      INC(i);
    END;
    IF (his_cnt > xmRTS.X2C_HIS_LEN) THEN
      outs(history_truncated, LENGTH(history_truncated)); newln;
    END;
  END;
  X2C_CLOSE_ERRINFO();
END X2C_show;


--------- init History

TYPE
  p2regModuleRec = POINTER TO regModuleRec;

  regModuleRec   = RECORD
                     from, to :CARDINAL;  (* codeseg extent *)
                     handle   :ADDRESS;   (* of DLL or EXE  *)
                     section  :CARDINAL;  (* # of code sec into exec file *)
                     next     :p2regModuleRec;
                   END;


PROCEDURE ["C"] X2C_IS_CODESEG (     adr       :CARDINAL;
                                 VAR base, sec :CARDINAL;
                                 VAR hmod      :SYSTEM.ADDRESS
                               ) :BOOLEAN;
VAR
  p :p2regModuleRec;
BEGIN
  p := X2C.X2C_HisList;
  WHILE (p # NIL) DO
    WITH p^ DO
      IF (from+7 <= adr) & (adr <= to ) THEN
      (* !!!! from+7 - IS_call tests 7 previous bytes of an address passed *)
        base := from;
        sec  := section;
        hmod := handle;
        RETURN TRUE;
      END;
    END;
    p := p^.next;
  END;
  RETURN FALSE;
END X2C_IS_CODESEG;


PROCEDURE ["C"] X2C_HISTORY_ON ();
BEGIN
  IF (face.X2C_stkScan=NIL) THEN
    face.X2C_stkScan := xrnStkScan.X2C_STACK_SCAN_PROC;
    face.X2C_hisShow := X2C_show;
  END;
END X2C_HISTORY_ON;

PROCEDURE ["C"] X2C_HISTORY_REG ( hmod, someAddr :ADDRESS );
  VAR
    p, pPrev, pCurr :p2regModuleRec;
    fromA, toA      :CARDINAL;
    sec             :CARDINAL;
BEGIN
  pPrev := NIL;
  pCurr := X2C.X2C_HisList;
  cseg.calcCodeExtent( hmod,CARDINAL(someAddr),  fromA, toA, sec );

  LOOP
    IF (pCurr = NIL) OR (fromA < pCurr^.from) THEN
      p := malloc(SIZE(regModuleRec));
      WITH p^ DO
        from    := fromA;
        to      := toA;
        section := sec;
        handle  := hmod;
        next    := pCurr;
      END;
      IF (pPrev = NIL) THEN
        X2C.X2C_HisList := p
      ELSE
        pPrev^.next := p;
      END;
      EXIT
    END;
    pPrev := pCurr;
    pCurr := pCurr^.next;
  END;
END X2C_HISTORY_REG;

PROCEDURE ["C"] X2C_EXIT_HISTORY ( hmod :ADDRESS );
VAR
  p, pPrev,pCurr :p2regModuleRec;
BEGIN
  (* if hmod=NIL, free all *)
  pPrev := NIL;
  pCurr := X2C.X2C_HisList;
  WHILE (pCurr # NIL) DO
    IF (hmod = NIL) OR (hmod = pCurr^.handle) THEN
      IF (pPrev = NIL) THEN
        X2C.X2C_HisList := pCurr^.next
      ELSE
        pPrev^.next := pCurr^.next
      END;
      p     := pCurr;
      pCurr := pCurr^.next;
      free(p);
    ELSE
      pPrev := pCurr;
      pCurr := pCurr^.next;
    END;
  END;
END X2C_EXIT_HISTORY;


END xrnShowHistory.
