<* +m2extensions *>
IMPLEMENTATION MODULE xrnStkScan; (* Hady. 03.06.96 11:58 *)

IMPORT  SYSTEM, xmRTS;

PROCEDURE ["C"] / X2C_IS_CALL(a: CARDINAL): CARDINAL;

PROCEDURE ["C"] X2C_STACK_SCAN_PROC(from,to: SYSTEM.ADDRESS; exact: BOOLEAN);
  VAR cur: CARDINAL; frame: SYSTEM.ADDRESS; sync: BOOLEAN;
  current: xmRTS.X2C_Coroutine;
BEGIN
  cur := 0;
  current:=xmRTS.X2C_GetCurrent();
  current^.his_cnt:=0;
  SYSTEM.GET(SYSTEM.SUBADR(from,4),frame); sync:=FALSE;
  IF exact THEN
    SYSTEM.GET((from),current^.his[0]);
    current^.his_cnt:=1;
    from:=SYSTEM.ADDADR(from,4);
  END;
  WHILE SYSTEM.CAST(CARDINAL,from)<SYSTEM.CAST(CARDINAL,to) DO
    IF sync THEN
      sync:=FALSE;
      IF (SYSTEM.CAST(CARDINAL,frame)>SYSTEM.CAST(CARDINAL,from)) &
         (SYSTEM.CAST(CARDINAL,frame)<=SYSTEM.CAST(CARDINAL,to)) THEN
        SYSTEM.GET(SYSTEM.ADDADR(frame,4),cur);
        cur:=X2C_IS_CALL(cur);
        IF cur#0 THEN
          from:=SYSTEM.ADDADR(frame,8);
          SYSTEM.GET((frame),frame);
          sync:=TRUE;
        END;
      END;
    END;
    IF ~sync THEN
      LOOP
        SYSTEM.GET((from),cur);
        cur:=X2C_IS_CALL(cur);
        IF cur#0 THEN EXIT END;
        from:=SYSTEM.ADDADR(from,4);
        IF SYSTEM.CAST(CARDINAL,from)>=SYSTEM.CAST(CARDINAL,to) THEN EXIT END;
      END;
      IF cur#0 THEN
        SYSTEM.GET(SYSTEM.SUBADR(from,4),frame); sync:=TRUE;
        from:=SYSTEM.ADDADR(from,4);
      END;
    END;
    IF (cur # 0) THEN
      IF current^.his_cnt<xmRTS.X2C_HIS_LEN THEN
        current^.his[current^.his_cnt]:=cur;
      END;
      INC(current^.his_cnt);
    END;
  END;
END X2C_STACK_SCAN_PROC;

END xrnStkScan.
