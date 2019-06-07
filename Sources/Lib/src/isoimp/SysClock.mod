(* Copyright (c) xTech 1993. All Rights Reserved. *)
<*+ M2EXTENSIONS *>
<*+ M2ADDTYPES *>
IMPLEMENTATION MODULE SysClock; (* Hady. *)

(* Modifications:
   22-Mar-94 Ned : merging implementations
    4-Apr-96 Hady: implementation made based on xlibOS interface.
*)
IMPORT  xlibOS;

PROCEDURE CanGetClock(): BOOLEAN;
BEGIN
  RETURN xlibOS.X2C_CanGetTime();
END CanGetClock;

PROCEDURE CanSetClock(): BOOLEAN;
BEGIN
  RETURN xlibOS.X2C_CanSetTime();
END CanSetClock;

PROCEDURE IsValidDateTime(d-: DateTime): BOOLEAN;

  PROCEDURE is_leap(y: CARDINAL): BOOLEAN;
  BEGIN
    RETURN (y MOD 4=0) & (y MOD 100#0) OR (y MOD 400=0);
  END is_leap;

  CONST m30days = {4,6,9,11};

(* Tests if the value of d is a valid *)
BEGIN
  IF (d.day<1) OR (d.day>31) THEN RETURN FALSE END;
  IF (d.month<1) OR (d.month>12) THEN RETURN FALSE END;
  IF (d.year<1) THEN RETURN FALSE END;
  IF (ORD(d.month) IN m30days) & (d.day>30) THEN RETURN FALSE END;
  IF (d.month=2) & (d.day>28+ORD(is_leap(d.year))) THEN RETURN FALSE END;
  IF (d.hour>23) OR (d.minute>59) OR (d.second>59) THEN RETURN FALSE END;
  RETURN TRUE
END IsValidDateTime;

PROCEDURE GetClock[UNINTERRUPTIBLE](VAR userData: DateTime);
  VAR t: xlibOS.X2C_TimeStruct;
BEGIN
  xlibOS.X2C_GetTime(t);
  IF t.month<1 THEN t.month:=1
  ELSIF t.month>12 THEN t.month:=12
  END;
  IF t.day<1 THEN t.day:=1
  ELSIF t.day>31 THEN t.day:=31
  END;
  IF t.hour>23 THEN t.hour:=23 END;
  IF t.min>59 THEN t.min:=59 END;
  IF t.sec>59 THEN t.sec:=59 END;
  IF t.zone<INT(MIN(UTCDiff)) THEN t.zone:=INT(MIN(UTCDiff)) END;
  IF t.zone>INT(MAX(UTCDiff)) THEN t.zone:=INT(MAX(UTCDiff)) END;
  t.fracs:=(t.fracs*(maxSecondParts+1)) DIV xlibOS.X2C_FracsPerSec();
  userData:=DateTime{
              t.year,t.month,t.day,t.hour,t.min,t.sec,t.fracs,t.zone,t.stf
            };
END GetClock;

PROCEDURE SetClock(d: DateTime);
  VAR rec: xlibOS.X2C_TimeStruct;
       fr: CARDINAL;
BEGIN
  IF NOT IsValidDateTime(d) THEN RETURN END;
  fr:=d.fractions;
  fr:=(fr*xlibOS.X2C_FracsPerSec()) DIV (maxSecondParts+1);
  rec := xlibOS.X2C_TimeStruct{
           d.year,d.month,d.day,d.hour,d.minute,d.second,fr,d.zone,d.SummerTimeFlag
         };
  xlibOS.X2C_SetTime(rec);
END SetClock;

END SysClock.
