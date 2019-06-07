(* Copyright (c) xTech 1992,96.  All Rights Reserved *)
(* Copyright (c) Excelsior LLC, 2002.  All Rights Reserved *)
<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xosTime; (* Jek. 26 Feb 2003 *)

IMPORT  SYSTEM, xlibOS, time, unpkTime;

FROM time IMPORT localtime, time_t, tm_ptr, clock, timeval, timezone_t, gettimeofday;


PROCEDURE ["C"] X2C_CanGetTime(): BOOLEAN;
BEGIN
  RETURN TRUE
END X2C_CanGetTime;


PROCEDURE ["C"] X2C_CanSetTime(): BOOLEAN;
BEGIN
  RETURN FALSE
END X2C_CanSetTime;


PROCEDURE ["C"] X2C_GetTime(VAR res: X2C_TimeStruct);
(*
VAR
  t : time_t;
BEGIN
  time.time(t);
  unpkTime.X2C_UnpackTime(t, res);
*)
VAR
  tv : timeval;
  tz : timezone_t;
BEGIN
  gettimeofday(tv, tz);
  unpkTime.X2C_UnpackTime(tv.tv_sec, res);
  res.fracs := tv.tv_usec;
END X2C_GetTime;


PROCEDURE ["C"] X2C_SetTime(VAR res: X2C_TimeStruct);
BEGIN
END X2C_SetTime;


PROCEDURE ["C"] X2C_FracsPerSec(): SYSTEM.CARD32;
BEGIN
  RETURN 1000000;
END X2C_FracsPerSec;


END xosTime.


