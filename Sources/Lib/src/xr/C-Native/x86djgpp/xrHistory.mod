<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE xrHistory;

IMPORT SYSTEM,xmRTS,X2C,stabs:=xruSTABS,xrnStkScan,stdio:=xPOSIX;


    PROCEDURE [2]/ X2C_IS_CALL(i: CARDINAL): CARDINAL;


  PROCEDURE [2] X2C_scanStackHistory(from,to: SYSTEM.ADDRESS; exact: BOOLEAN);
  BEGIN
    xrnStkScan.X2C_STACK_SCAN_PROC(from,to,exact);
  END X2C_scanStackHistory;

    PROCEDURE [2]/ X2C_SET_CODE_EXTENT(from,to: SYSTEM.INT32);
    VAR HISTORY_WAS_INIT: BOOLEAN;

  PROCEDURE [2] X2C_INIT_HISTORY;
    VAR maps: POINTER TO stdio.FILE;
        cbeg,cend: SYSTEM.INT32;
    CONST maps_name="/proc/self/maps";
  BEGIN
    IF HISTORY_WAS_INIT THEN RETURN ELSE HISTORY_WAS_INIT:=TRUE END;
    maps:=stdio.fopen(maps_name,"rb");
    IF maps=NIL THEN 
      cbeg:=1; cend:=0;
    ELSE 
      IF 2#stdio.fscanf(maps^,"%lx-%lx",SYSTEM.ADR(cbeg),SYSTEM.ADR(cend)) THEN
        cbeg:=1; cend:=0; 
      END;
      stdio.fclose(maps^)
    END;
    X2C_SET_CODE_EXTENT(cbeg,cend);
  END X2C_INIT_HISTORY;

TYPE NAME = ARRAY [0..63] OF CHAR;

  PROCEDURE GetInfo(ip: CARDINAL; VAR fn,pn: NAME; VAR ln,off: CARDINAL);
    VAR i,last_block_value: CARDINAL;
        st: stabs.STAB;
        name,cur_fn,cur_pn: NAME;
        cur_ln,cur_off,d_off, prev_stabstr_off,stabstr_off: CARDINAL;
        Error: BOOLEAN;
  BEGIN
    cur_fn:="";
    cur_pn:="";
    d_off:=0;
    cur_ln:=0;
    cur_off:=0;
    stabstr_off:=0;
    prev_stabstr_off:=0;
    last_block_value:=0;
    Error:=FALSE;
    FOR i:=0 TO stabs.GetStabsNumber()-1 DO
      off:=cur_off;
      ln:=cur_ln;
      pn:=cur_pn;
      fn:=cur_fn;
      stabs.GetStab(prev_stabstr_off,Error,i,st,name);
      IF Error THEN ln:=0;pn:="";fn:="";off:=0; RETURN END;
      IF    st.n_type=stabs.N_UNDF  THEN
                                    prev_stabstr_off:=stabstr_off;
                                    stabstr_off:=stabstr_off+st.n_value;
      ELSIF st.n_type=stabs.N_SO    THEN
                                         cur_fn:=name;
                                         d_off:=0;
                                         cur_ln:=0;
                                         last_block_value:=st.n_value;
      ELSIF st.n_type=stabs.N_FUN   THEN
                                         cur_pn:=name;
                                         d_off:=0;
                                         cur_ln:=st.n_desc;
                                         last_block_value:=st.n_value;
      ELSIF st.n_type=stabs.N_SLINE THEN
                                         cur_ln:=st.n_desc;
                                         d_off:=st.n_value;
      END;
      cur_off:=d_off+last_block_value;
      IF cur_off>ip THEN
        RETURN;
      END;
    END;
  END GetInfo;


PROCEDURE [2] X2C_show_history;
  VAR i,len: INTEGER;
      lineno,offset: CARDINAL;
      filename,procname: NAME;
      err: BOOLEAN;
      Current: xmRTS.X2C_Coroutine;
BEGIN
  Current:=xmRTS.X2C_GetCurrent();
  len:=Current^.his_cnt; IF len>xmRTS.X2C_HIS_LEN THEN len:=xmRTS.X2C_HIS_LEN END;
  stabs.OpenMyExe(err);
  IF (NOT HISTORY_WAS_INIT) OR (err) OR (len<=0) THEN stdio.printf("#RTS: No history available.\n"); RETURN END;
  stdio.printf("             Source file              LineNo   CodeOffset     Procedure name\n");
  FOR i:=0 TO len-1 DO  
    GetInfo(Current^.his[i],filename,procname,lineno,offset);
    stdio.printf("%-37s  %5d    %.8x   %-20s\n", filename, lineno, Current^.his[i], procname);
  END;
  IF Current^.his_cnt>xmRTS.X2C_HIS_LEN THEN
    stdio.printf("#RTS: History stack was truncated: not enough room.\n");
  END;
  stabs.CloseMyExe;
END X2C_show_history;


(* All the following procedures can be only in C version *)

PROCEDURE [2] X2C_show_profile;
BEGIN END X2C_show_profile;
PROCEDURE [2] X2C_HIS_SAVE(VAR sv: SYSTEM.INT16);
BEGIN END X2C_HIS_SAVE;
PROCEDURE [2] X2C_HIS_RESTORE(sv: SYSTEM.INT16);
BEGIN END X2C_HIS_RESTORE;
PROCEDURE [2] X2C_Profiler_clock;
BEGIN END X2C_Profiler_clock;
PROCEDURE [2] X2C_PROC_INP_F(file: ARRAY OF CHAR; line: SYSTEM.INT32);
BEGIN END X2C_PROC_INP_F;
PROCEDURE [2] X2C_PROC_PRF_F(file: ARRAY OF CHAR; line: SYSTEM.INT32);
BEGIN END X2C_PROC_PRF_F;
PROCEDURE [2] X2C_PROC_OUT_F;
BEGIN END X2C_PROC_OUT_F;

END xrHistory.
