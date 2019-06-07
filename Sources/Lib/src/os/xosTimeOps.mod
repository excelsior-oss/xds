(* (c) xTech 1997. All Rights Reserved *)
<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xosTimeOps; (* Hady, 2 Sep 1997. *)

IMPORT SYSTEM, xlibOS;

TYPE CARD32 = SYSTEM.CARD32;

CONST y001days = 365;
      y004days = 4 *y001days+1; (*   1461 *)
      y100days = 25*y004days-1; (*  36524 *)
      y400days = 4 *y100days+1; (* 146097 *)

CONST
(*
 monthDays= ARRAY OF CARDINAL {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
*)
 monthAdd = ARRAY OF CARD32 { 0,  3,  3,  6,  8, 11, 13, 16, 19, 21, 24, 26};
 monthLast= ARRAY OF CARD32 {31, 59, 90,120,151,181,212,243,273,304,334,365};

CONST
    FirstValidYear = 1;
    FirstDate = xlibOS.X2C_TimeStruct{FirstValidYear,1,1,0,0,0,0,0,FALSE};


PROCEDURE is_leap(y: CARD32): BOOLEAN;
BEGIN
  RETURN (y MOD 4=0) & (y MOD 100#0) OR (y MOD 400=0);
END is_leap;

PROCEDURE is_valid_day(y,m,d: CARD32): BOOLEAN;
  CONST m30days = {4,6,9,11};
BEGIN
  IF (d<1) OR (d>31) THEN RETURN FALSE END;
  IF (m<1) OR (m>12) THEN RETURN FALSE END;
  IF (y<1) THEN RETURN FALSE END;
  IF (m IN m30days) & (d>30) THEN RETURN FALSE END;
  IF (m=2) & (d>28+ORD(is_leap(y))) THEN RETURN FALSE END;
  RETURN TRUE;
END is_valid_day;

PROCEDURE is_valid(d-: xlibOS.X2C_TimeStruct): BOOLEAN;
(* Tests if the value of d is a valid *)
  CONST m30days = {4,6,9,11};
BEGIN
  IF is_valid_day(d.year,d.month,d.day) THEN
    IF (d.hour>23) OR (d.min>59) OR (d.sec>59) THEN RETURN FALSE END;
    RETURN TRUE
  END;
  RETURN FALSE;
END is_valid;

PROCEDURE day_of_year(y,m,d: CARD32): CARD32;
  VAR x: CARD32;
BEGIN
  DEC(m);
  x:=d+m*28+monthAdd[m];
  IF (m>=2) & is_leap(y) THEN INC(x) END;
  RETURN x;
END day_of_year;

PROCEDURE the_day(y,m,d: CARD32): CARD32;
  VAR day,year: CARD32;
BEGIN
  year:=y;
  DEC(y,FirstValidYear);
  day:=    (y DIV 400)*y400days; y:=y MOD 400;
  day:=day+(y DIV 100)*y100days; y:=y MOD 100;
  day:=day+(y DIV 004)*y004days;
  day:=day+(y MOD 004)*y001days;
  day:=day+day_of_year(year,m,d);
  RETURN day
END the_day;

PROCEDURE ["C"] X2C_TimeDayNum(y,m,d: CARD32): CARD32;
BEGIN
  IF is_valid_day(y,m,d) THEN RETURN the_day(y,m,d) END;
  RETURN 0;
END X2C_TimeDayNum;

PROCEDURE sub_days(g,l: xlibOS.X2C_TimeStruct): CARD32;
  VAR c: CARD32;
BEGIN
  c:=((l.year-FirstValidYear) DIV 400)*400;
  g.year:=g.year-c;
  l.year:=l.year-c;
  RETURN the_day(g.year,g.month,g.day)-
         the_day(l.year,l.month,l.day)
END sub_days;

PROCEDURE day_sec(h,m,s: CARD32): CARD32;
BEGIN
  RETURN s+m*60+h*3600
END day_sec;

PROCEDURE sub_secs(g-,l-: xlibOS.X2C_TimeStruct): CARD32;
  VAR days: CARD32;
BEGIN
  days:=sub_days(g,l);
  RETURN days*(3600*24)
       + day_sec(g.hour,g.min,g.sec)
       - day_sec(l.hour,l.min,l.sec)
END sub_secs;

PROCEDURE unpack_day(day: CARD32; VAR y,m,d: CARD32);
  VAR i: CARD32; leap: BOOLEAN;
BEGIN
  ASSERT(day>0,100);
  DEC(day);
  y:=400*(day DIV y400days); day:=day MOD y400days;

  i:=day DIV y100days;
  IF i=4 THEN i:=3 END; (* last 100-year interval is one day larger *)
  y:=y+i*100; day:=day-i*y100days;

  i:=day DIV y004days;  (* 4-year intarvals are of the same length  *)
  day:=day-i*y004days; y:=y+i*4;

  i:=day DIV y001days;
  IF i=4 THEN i:=3 END; (* last year is one day larger *)
  y:=y+i; day:=day-i*y001days;
  leap:=is_leap(y+FirstValidYear);

  INC(day);

  m:=day DIV 32;
  DEC(day,ORD(leap & (day>31)));
  WHILE (m<=HIGH(monthLast)) & (day>monthLast[m]) DO INC(m) END;
  d:=day;
  IF m>0 THEN d:=d-monthLast[m-1] END;
  INC(m);
  INC(d,ORD(leap & (m=2)));

END unpack_day;

PROCEDURE add_days(VAR y,m,d: CARD32; days: CARD32);
  VAR cy400s: CARD32; (* ammount of total 400-year cycles passed *)
BEGIN
  cy400s:=(y-FirstValidYear) DIV 400;
  y:=y-cy400s*400;

  cy400s:=cy400s+(days DIV y400days);
  days:=days MOD y400days;

  unpack_day(days+the_day(y,m,d),y,m,d);

  y:=y+cy400s*400+FirstValidYear;

END add_days;

PROCEDURE add_secs(VAR y,m,d,h,mi,s: CARD32; secs: CARD32);
  VAR days: CARDINAL;
BEGIN
  secs:=secs+day_sec(h,mi,s);
  days:=secs DIV (24*3600); secs:=secs MOD (24*3600);
  add_days(y,m,d,days);
  h :=secs DIV 3600; secs:=secs MOD 3600;
  mi:=secs DIV   60;
  s :=secs MOD   60;
END add_secs;

PROCEDURE ["C"] X2C_TimeCompare(dl-,dr-: xlibOS.X2C_TimeStruct): SYSTEM.INT32;
  VAR r: INTEGER;
BEGIN
  IF NOT (is_valid(dl) & is_valid(dr)) THEN RETURN 0 END;
  r:=INT(dl.year)-INT(dr.year);   IF r#0 THEN RETURN r END;
  r:=INT(dl.month)-INT(dr.month); IF r#0 THEN RETURN r END;
  r:=INT(dl.day)-INT(dr.day);     IF r#0 THEN RETURN r END;
  r:=INT(dl.hour)-INT(dr.hour);   IF r#0 THEN RETURN r END;
  r:=INT(dl.min)-INT(dr.min);     IF r#0 THEN RETURN r END;
  r:=INT(dl.sec)-INT(dr.sec);     IF r#0 THEN RETURN r END;
  RETURN INT(dl.fracs)-INT(dr.fracs);
END X2C_TimeCompare;

PROCEDURE ["C"] X2C_TimeDayInt(dl-,dr-: xlibOS.X2C_TimeStruct): CARD32;
BEGIN
  IF NOT (is_valid(dl) & is_valid(dr)) THEN RETURN 0 END;
  IF X2C_TimeCompare(dl,dr)<=0 THEN RETURN 0 END;
  RETURN sub_days(dl,dr);
END X2C_TimeDayInt;

PROCEDURE ["C"] X2C_TimeSecInt(dl-,dr-: xlibOS.X2C_TimeStruct): CARD32;
BEGIN
  IF NOT (is_valid(dl) & is_valid(dr)) THEN RETURN 0 END;
  IF X2C_TimeCompare(dl,dr)<=0 THEN RETURN 0 END;
  RETURN sub_secs(dl,dr);
END X2C_TimeSecInt;

PROCEDURE ["C"] X2C_TimeDayAdd(D-: xlibOS.X2C_TimeStruct; days: CARD32;
                          VAR res: xlibOS.X2C_TimeStruct);
  VAR y,m,d: CARD32;
BEGIN
  res:=FirstDate;
  IF NOT is_valid(D) THEN RETURN END;
  y:=D.year; m:=D.month; d:=D.day;
  add_days(y,m,d,days);
  res:=D;
  res.year:=y;
  res.month:=m;
  res.day:=d;
END X2C_TimeDayAdd;

PROCEDURE ["C"] X2C_TimeSecAdd(D-: xlibOS.X2C_TimeStruct; secs: CARD32;
                          VAR res: xlibOS.X2C_TimeStruct);
  VAR y,m,d,h,mi,s: CARD32;
BEGIN
  res:=FirstDate;
  IF NOT is_valid(D) THEN RETURN END;
  y:=D.year; m:=D.month; d:=D.day;
  h:=D.hour; mi:=D.min; s:=D.sec;
  add_secs(y,m,d,h,mi,s,secs);
  res.year:=y;
  res.month:=m;
  res.day:=d;
  res.hour:=h;
  res.min:=mi;
  res.sec:=s;
  res.fracs:=D.fracs;
  res.zone:=D.zone;
  res.stf:=D.stf;
END X2C_TimeSecAdd;

END xosTimeOps.
