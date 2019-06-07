(* Copyright (c) 1994 xTech Ltd, Russia. All Rights Reserved. *)
<*+ M2EXTENSIONS *>
(*
  Do not enable dynamic checks!
  Infinite recursion in X2C_PROC_INP & X2C_PROC_OUT
*)
<*- GENDEBUG     *>
<*- CHECKINDEX   *>
<*- CHECKNIL     *>
<*- CHECKSET     *>
<*- CHECKRANGE   *>
<*- CHECKPROC    *>
<*- iOVERFLOW    *>

IMPLEMENTATION MODULE xrHistory;

IMPORT  SYSTEM, RTS:=xmRTS, xPOSIX;

CONST
  tag_first  = 0;
  tag_second = 1;

VAR
  profile       : RTS.X2C_Profile;
  total_time    : SYSTEM.INT32;

(* The procedure is saved here for that time when I'll implement
	 the native-code version...
									 Hady. 08-25-95 04:48pm.

PROCEDURE ["C"] X2C_show_history;

  VAR i,len: INTEGER;
   info: x2chis.searchStruct;
  current: RTS.X2C_Coroutine;

  PROCEDURE quotate(VAR s: ARRAY OF CHAR);
    VAR i: CARDINAL;
  BEGIN
    i:=LENGTH(s); ASSERT(i+2<=HIGH(s));
    s[i+2]:=0C;
    s[i+1]:='"';
    WHILE (i>0) DO
      s[i]:=s[i-1]; DEC(i);
    END;
    s[0]:='"';
  END quotate;

  PROCEDURE truncFileName(VAR s: ARRAY OF CHAR; l: CARDINAL);
    VAR i,p,len: CARDINAL;
  BEGIN
    ASSERT(l>12);
    len:=LENGTH(s);
    IF (len<=l) THEN RETURN END;
    p:=len-l;
    i:=p+3;
    WHILE (i<len) & (s[i]#"\") DO INC(i) END;
    IF s[i]="\" THEN p:=i-3 END;
    FOR i:=0 TO 2 DO s[i]:="." END;
    i:=3;
    WHILE (p<len) DO s[i]:=s[p]; INC(i); INC(p) END;
    s[i]:=0C;
  END truncFileName;

  PROCEDURE prepareNames(VAR s: x2chis.searchStruct);
  BEGIN
    IF s.filename[0]=0C THEN
      COPY("UNKNOWN",s.filename);
    ELSE
      truncFileName(s.filename,30);
      quotate(s.filename);
    END;
    IF s.procname[0]=0C THEN
      (*COPY("UNKNOWN",s.procname);*)
    ELSE
      quotate(s.procname);
    END;
  END prepareNames;

BEGIN
  i:=x2chis.X2C_OPENCVINFO(X2C.X2C_argv^);
  IF i#0 THEN
    CASE i OF
      |1: xPOSIX.printf('#RTS: File "%s" seek error\n',X2C.X2C_argv^);
      |2: xPOSIX.printf('#RTS: File "%s" read error\n',X2C.X2C_argv^);
      |3: xPOSIX.printf('#RTS: File "%s" no CodeView info\n',X2C.X2C_argv^);
      |4: xPOSIX.printf('#RTS: File "%s" bad CodeView info\n',X2C.X2C_argv^);
      |5: xPOSIX.printf('#RTS: File "%s" open error\n',X2C.X2C_argv^);
      |6: xPOSIX.printf('#RTS: File "%s" incomplete CodeView info\n',X2C.X2C_argv^);
    END;
    RETURN;
  END;
  xPOSIX.printf("%-33s Line  Offset  Function\n------------------------------------------------------\n","Source file");
  current:=RTS.X2C_GetCurrent();
  WITH current^ DO
    i:=0;
    len:=his_cnt; IF len>RTS.X2C_HIS_LEN THEN len:=RTS.X2C_HIS_LEN END;
    WHILE (i<len) DO
      x2chis.X2C_FINDHISINFO(his[i],info);
      prepareNames(info);
      xPOSIX.printf("%-33.33s %4ld %08lX %s\n",info.filename,info.lineno,info.codeofs,info.procname);
      INC(i);
    END;
    IF his_cnt>RTS.X2C_HIS_LEN THEN
      xPOSIX.printf("#RTS: History stack was truncated: not enough room.\n");
    END;
  END;
  x2chis.X2C_CLOSECVINFO();
END X2C_show_history;

*)


(* procedure is declared in EXCEPTIONS.m *)
PROCEDURE ["C"] X2C_show_history;
  VAR i: SYSTEM.int;
  current: RTS.X2C_Coroutine;
BEGIN
  current:=RTS.X2C_GetCurrent();
  WITH current^ DO
    IF (his_cnt>0) & (his_cnt<=HIGH(his)-1) THEN
      his[his_cnt-1].fln:=RTS.X2C_hline;
    END;
    IF his_cnt>HIGH(his) THEN
      xPOSIX.printf("History stack was truncated - not enough room.\n");
      his_cnt:=HIGH(his)+1;
    END;
    WHILE his_cnt>0 DO
      DEC(his_cnt);
      i:=his[his_cnt].fln;
      IF his[his_cnt].fnm=NIL THEN
        xPOSIX.printf("???\n");
      ELSE
        xPOSIX.printf("%-18.18s %4d\n",his[his_cnt].fnm,i);
      END;
    END;
  END;
END X2C_show_history;


(* procedure is declared in xmRTS.d *)
PROCEDURE ["C"] X2C_PROC_INP_F(file: ARRAY OF CHAR; line: SYSTEM.INT32);
  VAR current: RTS.X2C_Coroutine;
BEGIN
  current:=RTS.X2C_GetCurrent();
  WITH current^ DO
    IF (his_cnt>0) & (his_cnt<=HIGH(his)+1) THEN
      his[his_cnt-1].fln:=RTS.X2C_hline;
    END;
    IF his_cnt<=HIGH(his) THEN
      his[his_cnt].fnm:=SYSTEM.ADR(file);
      his[his_cnt].prf:=NIL;
      his[his_cnt].fln:=line;
      his[his_cnt].tags:=SYSTEM.SET16{};
      RTS.X2C_hline:=line;
    END;
    INC(his_cnt);
  END;
END X2C_PROC_INP_F;

(* procedure is declared in xmRTS.d *)
PROCEDURE ["C"] X2C_PROC_PRF_F(file: ARRAY OF CHAR; line: SYSTEM.INT32;
			      VAR p: RTS.X2C_Profile_STR);
  VAR current: RTS.X2C_Coroutine;
BEGIN
  current:=RTS.X2C_GetCurrent();
  WITH current^ DO
    IF (his_cnt>0) & (his_cnt<=HIGH(his)+1) THEN
      his[his_cnt-1].fln:=RTS.X2C_hline;
    END;
    IF his_cnt<=HIGH(his) THEN
      WITH his[his_cnt] DO
        fnm:=SYSTEM.ADR(file);
        prf:=SYSTEM.ADR(p);
        fln:=line;
        tags:=SYSTEM.SET16{};
        IF tag_first IN p.tags THEN INCL(tags,tag_second);
        ELSE INCL(p.tags,tag_first);
        END;
      END;
      RTS.X2C_hline:=line;
    END;
    INC(his_cnt);
  END;
  IF p.count=0 THEN
    IF profile=NIL THEN RTS.X2C_Profiler END;
    p.next:=profile;
    profile:=SYSTEM.ADR(p);
  END;
  INC(p.count);
END X2C_PROC_PRF_F;

(* procedure is declared in xmRTS.d *)
PROCEDURE ["C"] X2C_Profiler_clock;
  VAR i: INTEGER;
  current: RTS.X2C_Coroutine;
BEGIN
  current:=RTS.X2C_GetCurrent();
  WITH current^ DO
    i:=0;
    WHILE (i<his_cnt) & (i<=HIGH(his)) DO
      WITH his[i] DO
        IF (prf#NIL) & NOT (tag_second IN tags) THEN INC(prf^.total) END;
      END;
      INC(i);
    END;
    IF (his_cnt<=HIGH(his)+1) & (his[his_cnt-1].prf#NIL) THEN
      INC(his[his_cnt-1].prf^.time);
    END;
  END;
  INC(total_time);
END X2C_Profiler_clock;

(* procedure is declared in RTS.d *)
PROCEDURE ["C"] X2C_PROC_OUT_F;
  VAR current: RTS.X2C_Coroutine;
BEGIN
  current:=RTS.X2C_GetCurrent();
  WITH current^ DO
    DEC(his_cnt);
    IF (his_cnt<=HIGH(his)) THEN
      WITH his[his_cnt] DO
        IF (prf#NIL) & NOT (tag_second IN tags) THEN
          EXCL(prf^.tags,tag_first);
        END;
      END;
    END;
    IF (his_cnt>0) & (his_cnt<=HIGH(his)-1) THEN
      RTS.X2C_hline:=his[his_cnt-1].fln;
    END;
  END;
END X2C_PROC_OUT_F;

(* procedure is declared in RTS.d *)
PROCEDURE ["C"] X2C_HIS_SAVE(VAR sv: SYSTEM.INT16);
  VAR current: RTS.X2C_Coroutine;
BEGIN
  current:=RTS.X2C_GetCurrent();
  sv:=current^.his_cnt;
END X2C_HIS_SAVE;

(* procedure is declared in RTS.d *)
PROCEDURE ["C"] X2C_HIS_RESTORE(sv: SYSTEM.INT16);
  VAR current: RTS.X2C_Coroutine;
BEGIN
  current:=RTS.X2C_GetCurrent();
  WITH current^ DO
    IF (his_cnt>0) & (his_cnt<=HIGH(his)-1) THEN
      his[his_cnt-1].fln:=RTS.X2C_hline;
    END;
    his_cnt:=sv;
    IF (his_cnt>0) & (his_cnt<=HIGH(his)-1) THEN
      RTS.X2C_hline:=his[his_cnt-1].fln;
    END;
  END;
END X2C_HIS_RESTORE;

PROCEDURE ["C"] X2C_ini_profiler;
BEGIN
  profile:=NIL;
  total_time:=0;
END X2C_ini_profiler;

PROCEDURE ["C"] X2C_show_profile;
  VAR prf,s,q,r: RTS.X2C_Profile; sum: REAL;
BEGIN
  prf:=profile;
  IF prf#NIL THEN
    sum:=FLOAT(total_time);
    xPOSIX.printf("Execution profile:\n");
    r:=NIL;
    WHILE prf#NIL DO
      s:=prf; prf:=prf^.next;
      IF r=NIL THEN
        r:=s; s^.next:=NIL;
      ELSIF r^.total<s^.total THEN
        s^.next:=r; r:=s;
      ELSE
        q:=r;
        WHILE (q^.next#NIL) & (q^.next^.total>s^.total) DO q:=q^.next END;
        s^.next:=q^.next; q^.next:=s;
      END;
    END;
    IF sum=0.0 THEN sum:=1.0 END;
    REPEAT
      xPOSIX.printf("%-32.32s %10ld %6.2f %6.2f\n",
         r^.name,
         r^.count,
         FLOAT(r^.time)*100.0/sum,
         FLOAT(r^.total)*100.0/sum);
      r:=r^.next;
    UNTIL r=NIL;
  END;
END X2C_show_profile;

PROCEDURE ["C"] X2C_scanStackHistory(from, to: SYSTEM.ADDRESS; exact: BOOLEAN);
BEGIN
  (* intentionaly blank *)
END X2C_scanStackHistory;

END xrHistory.
