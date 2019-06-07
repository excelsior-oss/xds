(* Copyright (c) 1991,98 XDS Ltd, Russia. All Rights Reserved. *)
(* Copyright (c) 2002 Excelsior LLC. All Rights Reserved. *)

<*+ M2ADDTYPES *>
<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE unpkTime; (* Jek 04-Dec-2002. *)


FROM xlibOS IMPORT X2C_TimeStruct;
FROM time IMPORT time_t, tm_ptr, clock, CLOCKS_PER_SEC, localtime, timezone, daylight;


PROCEDURE ["C"] X2C_UnpackTime(t : time_t; VAR res: X2C_TimeStruct);
  VAR p : tm_ptr;
BEGIN
  p := localtime(t);

  IF p = NIL THEN
    res.year := 1970;
    res.month := 1;
    res.day := 1;
    res.hour := 0;
    res.min := 0;
    res.sec := 0;
    res.fracs := 0;
    res.zone := 0;
    res.stf := FALSE;
    RETURN
  END;
  
  res.year := p^.tm_year;

  IF res.year<1900 THEN
    IF res.year<70 THEN
      INC(res.year, 2000);
    ELSE 
      INC(res.year, 1900);
    END;
  END;

  res.month := p^.tm_mon+1;
  res.day := p^.tm_mday;
  res.hour := p^.tm_hour;
  res.min := p^.tm_min;
  res.sec := p^.tm_sec;

  res.fracs := 0;

  res.zone := timezone DIV 60;
  res.stf := (daylight # 0);

END X2C_UnpackTime;

END unpkTime.

