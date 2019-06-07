(* Copyright (c) xTech 1992,96.  All Rights Reserved *)
<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xosTime; (* Hady. 04.06.96 11:59 *)

IMPORT  SYSTEM, xlibOS, xWin32;

PROCEDURE ["C"] X2C_CanGetTime(): BOOLEAN;
BEGIN
  RETURN TRUE
END X2C_CanGetTime;

PROCEDURE ["C"] X2C_CanSetTime(): BOOLEAN;
BEGIN
(*
   On Windows NT/2000, only processes that have SE_SYSTEMTIME_NAME
   privilege are able to set system time. In the current implementation,
   X2C_CanSetTime always returns FALSE, although X2C_SetTime may succeed.
*)
  RETURN FALSE
END X2C_CanSetTime;

PROCEDURE ["C"] X2C_GetTime(VAR t: X2C_TimeStruct);
  VAR d: xWin32.TimeDate; z: xWin32.TimeZone; res: CARDINAL;
BEGIN
  xWin32.GetLocalTime(d);
  res:=xWin32.GetTimeZoneInformation(z);
  IF res=0FFFFFFFFH THEN z.bias:=0 END;
  t:=X2C_TimeStruct{
    d.year,d.month,d.day,d.hour,d.min,d.sec,d.frac,z.bias,(res=2)
    };
(*
  t.year:=d.year;
  t.month:=d.month;
  t.day:=d.day;
  t.hour:=d.hour;
  t.minute:=d.min;
  t.second:=d.sec;
  t.fracs:=d.frac;
  t.zone:=z.bias;
  t.stf:=(res=2)
*)
END X2C_GetTime;

PROCEDURE ["C"] X2C_SetTime(VAR t: X2C_TimeStruct);
VAR
  d: xWin32.TimeDate;

BEGIN
  d.year  := t.year;
  d.month := t.month;
  d.day   := t.day;
  d.hour  := t.hour;
  d.min   := t.min;
  d.sec   := t.sec;
  d.frac  := t.fracs;
(*
   On Windows NT/2000, SetLocalTime has to be called twice to ensure
   correct daylight saving time setting. For more information, see
   SetLocalTime description in Platform SDK documentation.
*)
  IF NOT xWin32.SetLocalTime(d) THEN END;
  IF NOT xWin32.SetLocalTime(d) THEN END;
END X2C_SetTime;

PROCEDURE ["C"] X2C_FracsPerSec(): SYSTEM.CARD32;
BEGIN
  RETURN 1000;
END X2C_FracsPerSec;


END xosTime.
