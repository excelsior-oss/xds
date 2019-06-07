(* Copyright (c) xTech 1992,96.  All Rights Reserved *)
<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xosTime; (* Hady. 04.06.96 11:59 *)

IMPORT  SYSTEM, xlibOS, xOS2;

PROCEDURE ["C"] X2C_CanGetTime(): BOOLEAN;
BEGIN
  RETURN TRUE
END X2C_CanGetTime;

PROCEDURE ["C"] X2C_CanSetTime(): BOOLEAN;
BEGIN
  RETURN TRUE;
END X2C_CanSetTime;

PROCEDURE ["C"] X2C_GetTime(VAR t: X2C_TimeStruct);
  VAR d: xOS2.DATETIME;
BEGIN
  IF xOS2.DosGetDateTime(d) # 0 THEN (* ?? *) END;
  t.year := d.year;
  t.month:= d.month;
  t.day  := d.day;
  t.hour := d.hours;
  t.min  := d.minutes;
  t.sec  := d.seconds;
  t.fracs:= d.hundredths;
  t.zone := d.timezone;
  t.stf  := FALSE; (* ?? *)
END X2C_GetTime;

PROCEDURE ["C"] X2C_SetTime(VAR t: X2C_TimeStruct);
  VAR rec: xOS2.DATETIME;
BEGIN
  rec.hours   := t.hour;
  rec.minutes := t.min;
  rec.seconds := t.sec;
  rec.hundredths := t.fracs;
  rec.day     := t.day;
  rec.month   := t.month;
  rec.year    := t.year;
  rec.timezone := t.zone;
  IF xOS2.DosSetDateTime(rec) # 0 THEN (* ?? *) END;
END X2C_SetTime;

PROCEDURE ["C"] X2C_FracsPerSec(): SYSTEM.CARD32;
BEGIN
  RETURN 100;
END X2C_FracsPerSec;

END xosTime.
