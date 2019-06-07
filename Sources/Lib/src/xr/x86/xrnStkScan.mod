<*+ m2extensions*>
<*- checkindex  *>
<*- checkdindex *>
<*- checknil    *>
<*- checkset    *>
<*- checkrange  *>
<*- ioverflow   *>
<*- coverflow   *>
<*- foverflow   *>

IMPLEMENTATION MODULE xrnStkScan; (* VitVit'n'Hady. 03.06.96 11:58 *)

IMPORT SYSTEM,
       xmRTS,
       show  := xrnShowHistory;

PROCEDURE ["C"] / X2C_IS_CALL ( a :CARDINAL) :CARDINAL;

PROCEDURE is_call(     adr           :CARDINAL;
                   VAR calladr, base :CARDINAL;
                   VAR section       :CARDINAL;
                   VAR hmod          :SYSTEM.ADDRESS
                  );
BEGIN
  IF  show.X2C_IS_CODESEG ( adr, base, section, hmod ) THEN
    calladr := X2C_IS_CALL(adr)
  ELSE
    calladr := 0
  END;
END is_call;

PROCEDURE ["C"] X2C_STACK_SCAN_PROC ( from,to :SYSTEM.ADDRESS; exact :BOOLEAN );
  VAR
    cur, base :CARDINAL;
    handle    :SYSTEM.ADDRESS;
    section   :CARDINAL;

    frame     :SYSTEM.ADDRESS;
    sync      :BOOLEAN;
    current   :xmRTS.X2C_Coroutine;
BEGIN
  to := SYSTEM.SUBADR(to,4); 
  (* do not show in history the startup call to "main"  - VitVit *)
  current := xmRTS.X2C_GetCurrent();
  current^.his_cnt := 0;
  SYSTEM.GET(SYSTEM.SUBADR(from,4),frame);
  sync := FALSE;
  IF (exact) THEN
    SYSTEM.GET((from), cur);
    IF show.X2C_IS_CODESEG ( cur, base, section, handle ) THEN
      WITH current^.his[current^.his_cnt] DO
        ofs  := cur-base;
        hmod := handle;
        sec  := section;
      END;
      current^.his_cnt := 1;
      from := SYSTEM.ADDADR(from,4);
    END;
  END;
  cur := 0;
  WHILE (CARDINAL(from)<CARDINAL(to)) DO
    IF sync THEN
      sync := FALSE;
      IF (SYSTEM.CAST(CARDINAL,frame)>SYSTEM.CAST(CARDINAL,from)) &
         (SYSTEM.CAST(CARDINAL,frame)<=SYSTEM.CAST(CARDINAL,to)) THEN
        SYSTEM.GET(SYSTEM.ADDADR(frame,4),cur);
        is_call(cur,   cur, base, section, handle);
        IF (cur # 0) THEN
          from:=SYSTEM.ADDADR(frame,8);
          SYSTEM.GET((frame),frame);
          sync:=TRUE;
        END;
      END;
    END;
    IF ~sync THEN
      LOOP
        SYSTEM.GET((from),cur);
        is_call(cur,   cur, base, section, handle);
        IF (cur # 0) THEN EXIT END;
        from:=SYSTEM.ADDADR(from,4);
        IF SYSTEM.CAST(CARDINAL,from)>=SYSTEM.CAST(CARDINAL,to) THEN EXIT END;
      END;
      IF (cur # 0) THEN
        SYSTEM.GET(SYSTEM.SUBADR(from,4),frame); sync:=TRUE;
        from:=SYSTEM.ADDADR(from,4);
      END;
    END;
    IF (cur # 0) THEN
      IF (current^.his_cnt < xmRTS.X2C_HIS_LEN) THEN
        WITH current^.his[current^.his_cnt] DO
          ofs  := cur-base;
          hmod := handle;
          sec  := section;
        END;
      END;
      INC(current^.his_cnt);
    END;
  END;
END X2C_STACK_SCAN_PROC;

END xrnStkScan.
