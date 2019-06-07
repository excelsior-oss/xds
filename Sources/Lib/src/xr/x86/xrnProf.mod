(* Copyright (c) 1999-2000 Excelsior, Russia. All Rights Reserved. *)

<* +M2EXTENSIONS *>
<* GENPROF = "none" *>

IMPLEMENTATION MODULE xrnProf;


IMPORT SYSTEM, rts := xProfRTS;
IMPORT xosCodeSeg, xosDLL, os := xrtsOS, xosEnv, xrFName;

IMPORT xp := xPerfMon;

TYPE CARD32 = xp.CARD32; INT64 = xp.INT64;

-- procedures for 64-bit integers
TYPE i64sys = SYSTEM.INT64;
PROCEDURE ["C"] / X2J_ADD64 (al, ah, bl, bh: CARD32): i64sys;
PROCEDURE ["C"] / X2J_SUB64 (al, ah, bl, bh: CARD32): i64sys;
PROCEDURE ["C"] / X2J_DIV64 (al, ah, bl, bh: CARD32): i64sys;

PROCEDURE from64(i: INT64): INTEGER;
BEGIN
  ASSERT(i.hi = 0);
  RETURN i.lo;
END from64;

PROCEDURE to64(lo, hi: CARD32): INT64;
VAR i: INT64;
BEGIN
  i.lo := lo;
  i.hi := hi;
  RETURN i;
END to64;

PROCEDURE add64(a, b: INT64): INT64;
BEGIN
  RETURN SYSTEM.CAST(INT64, X2J_ADD64(a.lo, a.hi, b.lo, b.hi));
END add64;

PROCEDURE sub64(a, b: INT64): INT64;
BEGIN
  RETURN SYSTEM.CAST(INT64, X2J_SUB64(a.lo, a.hi, b.lo, b.hi));
END sub64;

PROCEDURE div64(a, b: INT64): INT64;
BEGIN
  RETURN SYSTEM.CAST(INT64, X2J_DIV64(a.lo, a.hi, b.lo, b.hi));
END div64;

-------------------------------

PROCEDURE ComputeTailOverhead( bracket: xp.BracketProc; rec_level: INTEGER;
                               tsc_is_neg: BOOLEAN ): INT64;
VAR start_tsc, end_tsc: INT64;
    desc: xp.ProfSTR;
BEGIN
  desc.pure_dur_lo := 0;
  desc.pure_dur_hi := 0;
  desc.dirty_dur_lo := 0;
  desc.dirty_dur_hi := 0;
  desc.rec_level := rec_level;
  desc.call_list := NIL;
  xp.X2C_PROFILE_TAIL_OVERHEAD( desc, bracket, end_tsc );
  start_tsc := to64(desc.pure_dur_lo, desc.pure_dur_hi);
  IF tsc_is_neg THEN
    RETURN add64( end_tsc, start_tsc );
  ELSE
    RETURN sub64( end_tsc, start_tsc );
  END;
END ComputeTailOverhead;

PROCEDURE ComputeHeadOverhead( bracket: xp.BracketProc; rec_level: INTEGER;
                               tsc_is_neg: BOOLEAN ): INT64;
VAR start_tsc, end_tsc: INT64;
    desc: xp.ProfSTR;
BEGIN
  desc.pure_dur_lo := 0;
  desc.pure_dur_hi := 0;
  desc.dirty_dur_lo := 0;
  desc.dirty_dur_hi := 0;
  desc.rec_level := rec_level;
  desc.call_list := NIL;
  xp.X2C_PROFILE_HEAD_OVERHEAD( desc, bracket, start_tsc );
  end_tsc := to64(desc.pure_dur_lo, desc.pure_dur_hi);
  IF tsc_is_neg THEN
    RETURN sub64( to64(0, 0), add64( end_tsc, start_tsc ) );
  ELSE
    RETURN sub64( end_tsc, start_tsc );
  END;
END ComputeHeadOverhead;

PROCEDURE ComputeOverhead( bracket: xp.BracketProc; rec_level: INTEGER;
                           tsc_is_neg: BOOLEAN; is_tail: BOOLEAN ): INTEGER;
CONST N_cnt = 65536;
VAR i: INTEGER;
    o_curr, o_sum: INT64;
    o, o_min, o_max: INTEGER;
BEGIN
  o_sum := to64(0, 0);
  o_max := 0;
  o_min := MAX(INTEGER);
  FOR i := 1 TO N_cnt DO
    IF is_tail THEN
      o_curr := ComputeTailOverhead( bracket, rec_level, tsc_is_neg );
    ELSE
      o_curr := ComputeHeadOverhead( bracket, rec_level, tsc_is_neg );
    END;
    o := from64(o_curr);
    IF o < o_min THEN o_min := o; END;
    IF o > o_max THEN o_max := o; END;
    o_sum := add64(o_curr, o_sum);
  END;
  o := from64( div64(o_sum, to64(N_cnt, 0) ) );
(*  Printf.printf( "rec_level = %d, is_tail = %d, min = %d, max = %d, avr = %d\n",
                 rec_level, ORD(is_tail), o_min, o_max, o );
*)  RETURN o;
END ComputeOverhead;

PROCEDURE ComputeOverheads();
VAR pbh, pbt, peh, pet,
    nbh, nbt, neh, net,
    Rbh, Rbt, Reh, Ret: INTEGER;
BEGIN
  xp.proc_dirty_overhead_rec := 0;
  xp.proc_dirty_overhead_norec := 0;
  xp.proc_pure_overhead := 0;
  xp.nest_dirty_overhead := 0;
  xp.nest_pure_overhead := 0;

  pbh := ComputeOverhead( xp.X2C_PROFILE_PROC_START, 0, TRUE, FALSE );
  pbt := ComputeOverhead( xp.X2C_PROFILE_PROC_START, 0, TRUE, TRUE );
  peh := ComputeOverhead( xp.X2C_PROFILE_PROC_END, 1, FALSE, FALSE );
  pet := ComputeOverhead( xp.X2C_PROFILE_PROC_END, 1, FALSE, TRUE );

  Rbh := ComputeOverhead( xp.X2C_PROFILE_PROC_START, 1, TRUE, FALSE );
  Rbt := ComputeOverhead( xp.X2C_PROFILE_PROC_START, 1, TRUE, TRUE );
  Reh := ComputeOverhead( xp.X2C_PROFILE_PROC_END, 2, FALSE, FALSE );
  Ret := ComputeOverhead( xp.X2C_PROFILE_PROC_END, 2, FALSE, TRUE );

  nbh := ComputeOverhead( xp.X2C_PROFILE_NEST_START, 0, FALSE, FALSE );
  nbt := ComputeOverhead( xp.X2C_PROFILE_NEST_START, 0, FALSE, TRUE );
  neh := ComputeOverhead( xp.X2C_PROFILE_NEST_END, 0, TRUE, FALSE );
  net := ComputeOverhead( xp.X2C_PROFILE_NEST_END, 0, TRUE, TRUE );

  xp.proc_dirty_overhead_rec := Rbh + Rbt + Reh + Ret;
  xp.proc_dirty_overhead_norec := pbh + pbt + peh + pet;
  xp.proc_pure_overhead := pbt + peh;
  xp.nest_dirty_overhead := nbh + nbt + neh + net;
  xp.nest_pure_overhead := nbh + net;

  xp.brackets_sum_overhead_lo := 0;
  xp.brackets_sum_overhead_hi := 0;
END ComputeOverheads;

(* -------------------------------------------------------------------------- *)

(* identical to M2 ISO Strings.Append *)
PROCEDURE append(s: ARRAY OF CHAR; VAR d: ARRAY OF CHAR);
  VAR pos,i,len: CARDINAL;
BEGIN
  pos:=LENGTH(d);
  len:=LENGTH(s);
  IF pos+len >HIGH(d)+1 THEN len:=HIGH(d)+1 - pos END;
  i:=0;
  WHILE i<len DO d[pos]:=s[i]; INC(i); INC(pos) END;
  IF pos<=HIGH(d) THEN d[pos]:=0C END;
END append;

PROCEDURE dot_xpt_name (VAR name: ARRAY OF CHAR);
TYPE PPROGNAME = POINTER TO ARRAY [0..260] OF CHAR;
VAR progname: PPROGNAME;
BEGIN
  progname := SYSTEM.CAST(PPROGNAME,xosEnv.X2C_GetProgramName());
  xrFName.X2C_ExtractBaseName( progname^, name );
  append( ".xpt", name );
END dot_xpt_name;

TYPE FILE = os.X2C_OSFILE;
CONST BADFILE = FILE(NIL);

PROCEDURE fclose(VAR f: FILE);
BEGIN
  IF f # BADFILE THEN
    IF os.X2C_FileClose(f) = 0 THEN END;
    f := BADFILE;
  END;
END fclose;

PROCEDURE fopen_new (name-: ARRAY OF CHAR): FILE;
VAR res: INTEGER;
    f: FILE;
BEGIN
  res := os.X2C_FileOpenWrite(f, name);
  IF res # 0 THEN
    f := BADFILE;
  END;
  RETURN f;
END fopen_new;

PROCEDURE fopen_append (name-: ARRAY OF CHAR): FILE;
VAR res: INTEGER;
    f: FILE;
    ofs: SYSTEM.WORD;
BEGIN
  res := os.X2C_FileOpenRW(f, name);
  IF res # 0 THEN
    f := BADFILE;
  ELSE
    ofs := SYSTEM.CAST(SYSTEM.WORD, 0);
    IF os.X2C_FileSeek(f, ofs, os.X2C_end) # 0 THEN
      fclose(f);
    END;
  END;
  RETURN f;
END fopen_append;

VAR
  x2c_prof_mode : rts.ProfilingModeType;
  list: rts.X2C_Profile_Module;
  modNum: INTEGER;

PROCEDURE ["C"] X2C_PROFILE_BEGIN_MODULE (m: rts.X2C_Profile_Module);
BEGIN
  m^.next := list;
  list := m;
  INC(modNum);
END X2C_PROFILE_BEGIN_MODULE;


(* Character output *)

VAR xpt_file: FILE;
VAR xpt_name: ARRAY [0..255] OF CHAR;

PROCEDURE outs(s-: ARRAY OF CHAR; len: CARDINAL);
BEGIN
  IF (len # 0) & (xpt_file # BADFILE) THEN
    IF os.X2C_FileWrite(xpt_file, SYSTEM.ADR(s),len) # 0 THEN
      fclose(xpt_file);
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

PROCEDURE outhex(no: CARDINAL; sp: CARDINAL);
VAR buf: ARRAY [0..11] OF CHAR;
    pos: CARD32;
BEGIN
  pos := 0;
  os.X2C_HexToStr(buf, pos, no);
  outs(buf,8);
  IF sp>0 THEN
    WHILE sp>8 DO outs(spaces, 8); DEC(sp,8) END;
    IF sp>0 THEN outs(spaces, sp) END;
  END;
END outhex;

PROCEDURE outdec(no: CARDINAL; sp: CARDINAL);
VAR buf: ARRAY [0..11] OF CHAR;
    pos: CARD32;
BEGIN
  pos := 0;
  os.X2C_DecToStr(buf, pos, no);
  outs(buf, pos);
  IF sp>0 THEN
    WHILE sp>8 DO outs(spaces, 8); DEC(sp,8) END;
    IF sp>0 THEN outs(spaces, sp) END;
  END;
END outdec;

VAR allExecTime: INT64;

PROCEDURE ["C"] X2C_INIT_PROFILER (prof_mode: SYSTEM.INT32);
CONST magic = ARRAY OF CHAR { 0C,377C,377C,377C,377C };
BEGIN
  list := NIL;
  xp.X2C_prof_enabled := 1;
  x2c_prof_mode := VAL(rts.ProfilingModeType, prof_mode);
  dot_xpt_name( xpt_name );
  xpt_file := fopen_new( xpt_name );

  string("XDS Profiler, 1.0", 0);
  outs(magic, 5);
  newln;

  fclose(xpt_file);

  allExecTime := to64(0, 0);
  xp.X2C_Open_TSC_Scope(allExecTime);
END X2C_INIT_PROFILER;

PROCEDURE ["C"] X2C_EXIT_PROFILER (isExe: BOOLEAN);
VAR
  i             :INTEGER;
  compName      :ARRAY [0..255] OF CHAR;
  foo           :rts.X2C_CALL_TYPE_PTR;
  comhan        :SYSTEM.ADDRESS;
  from, to, sec :CARDINAL;

BEGIN
  IF xp.X2C_prof_enabled # 1 THEN RETURN END;

  IF isExe THEN xp.X2C_Close_TSC_Scope(allExecTime); END;

  dot_xpt_name( xpt_name );
  xpt_file := fopen_append( xpt_name );

  IF isExe THEN
(*    CASE X2C_profMode OF
    | rts.PROF_MODE_MIN:      Printf.fprintf(file,"MODE MIN\n");
    | rts.PROF_MODE_STANDARD: Printf.fprintf(file,"MODE STANDARD\n");
    | rts.PROF_MODE_FULL:     Printf.fprintf(file,"MODE FULL\n");
    END;
*)
    string("PROGRAM EXECUTION TIME  ", 0);
    outhex( allExecTime.hi, 1 ); -- execTimeHi
    outhex( allExecTime.lo, 0 ); -- execTimeLo
    newln;
  END;

  comhan := xosDLL.X2C_GetMyHandle();
  xosDLL.X2C_GetModuleName (comhan, compName, HIGH(compName)+1 );
  xosCodeSeg.calcCodeExtent( comhan, list^.mainAdr, from, to, sec );
--  FileName.GetName( compName, compName );
  string("COM ", 0); outhex(from, 1); outhex(to, 1);
  string(compName, 0);
  newln;
  --  Printf.fprintf(file, "  MODULES %d\n", modNum);

  WHILE list # NIL DO
    string("M ", 0);
    string(list^.moduleName^, 0);
    newln;
    --    Printf.fprintf(file, "  PROCEDURES %d\n", list^.procCount-1);

    FOR i := 0 TO list^.procCount-1 DO
      string("P ", 0);
      WITH list^.info^[i] DO
        outhex(pure_dur_hi, 1);
        outhex(pure_dur_lo, 1);
        outhex(dirty_dur_hi, 1);
        outhex(dirty_dur_lo, 1);
        outdec(total_entry_count, 1);
        outdec(norec_entry_count, 1);
      END;
      string(list^.xref^[i]^, 0);
      newln;

      foo := list^.info^[i].call_list;
      WHILE foo # NIL DO
        string("C ", 0);
        outhex(SYSTEM.CAST(CARDINAL, foo^.ip), 1);
        outdec(foo^.count, 0);
        newln;
        foo := foo^.next;
      END;

    END;
    list := list^.next;
  END;

  fclose(xpt_file);
END X2C_EXIT_PROFILER;

END xrnProf.
