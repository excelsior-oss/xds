(* (c) xTech 1992,93. All Rights Reserved *)
<*+ m2extensions *>
IMPLEMENTATION MODULE TimeConv;

IMPORT  sys:=SysClock;

CONST y001days = 365;
      y004days = 4 *y001days+1; (*   1461 *)
      y100days = 25*y004days-1; (*  36524 *)
      y400days = 4 *y100days+1; (* 146097 *)

CONST
(*
 monthDays= ARRAY OF CARDINAL {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
*)
 monthAdd = ARRAY OF CARDINAL { 0,  3,  3,  6,  8, 11, 13, 16, 19, 21, 24, 26};
 monthLast= ARRAY OF CARDINAL {31, 59, 90,120,151,181,212,243,273,304,334,365};

VAR FirstValidYear: CARDINAL;

PROCEDURE is_leap(y: CARDINAL): BOOLEAN;
BEGIN
  RETURN (y MOD 4=0) & (y MOD 100#0) OR (y MOD 400=0);
END is_leap;

PROCEDURE day_of_year(y,m,d: CARDINAL): CARDINAL;
  VAR x: CARDINAL;
BEGIN
  DEC(m);
  x:=d+m*28+monthAdd[m];
  IF (m>=2) & is_leap(y) THEN INC(x) END;
  RETURN x;
END day_of_year;

PROCEDURE the_day(y,m,d: CARDINAL): CARDINAL;
  VAR day,year: CARDINAL;
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

PROCEDURE sub_days(g,l: DateTime): CARDINAL;
  VAR c: CARDINAL;
BEGIN
  c:=((l.year-FirstValidYear) DIV 400)*400;
  g.year:=g.year-c;
  l.year:=l.year-c;
  RETURN the_day(g.year,ORD(g.month),ORD(g.day))-
         the_day(l.year,ORD(l.month),ORD(l.day))
END sub_days;

PROCEDURE day_sec(h,m,s: CARDINAL): CARDINAL;
BEGIN
  RETURN s+m*60+h*3600
END day_sec;

PROCEDURE sub_secs(g-,l-: DateTime): CARDINAL;
  VAR days: CARDINAL;
BEGIN
  days:=sub_days(g,l);
  RETURN days*(3600*24)
       + day_sec(ORD(g.hour),ORD(g.minute),ORD(g.second))
       - day_sec(ORD(l.hour),ORD(l.minute),ORD(l.second))
END sub_secs;

PROCEDURE unpack_day(day: CARDINAL; VAR y,m,d: CARDINAL);
  VAR i: CARDINAL; leap: BOOLEAN;
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

PROCEDURE add_days(VAR y,m,d: CARDINAL; days: CARDINAL);
  VAR cy400s: CARDINAL; (* ammount of total 400-year cycles passed *)
BEGIN
  cy400s:=(y-FirstValidYear) DIV 400;
  y:=y-cy400s*400;

  cy400s:=cy400s+(days DIV y400days);
  days:=days MOD y400days;

  unpack_day(days+the_day(y,m,d),y,m,d);

  y:=y+cy400s*400+FirstValidYear;

END add_days;

PROCEDURE add_secs(VAR y,m,d,h,mi,s: CARDINAL; secs: CARDINAL);
  VAR days: CARDINAL;
BEGIN
  secs:=secs+day_sec(h,mi,s);
  days:=secs DIV (24*3600); secs:=secs MOD (24*3600);
  add_days(y,m,d,days);
  h :=secs DIV 3600; secs:=secs MOD 3600;
  mi:=secs DIV   60;
  s :=secs MOD   60;
END add_secs;

PROCEDURE Compare(dl,dr: DateTime): INTEGER;
  VAR r: INTEGER;
BEGIN
  IF NOT (sys.IsValidDateTime(dl) & sys.IsValidDateTime(dr)) THEN RETURN 0 END;
  r:=INT(dl.year)-INT(dr.year);     IF r#0 THEN RETURN r END;
  r:=INT(dl.month)-INT(dr.month);   IF r#0 THEN RETURN r END;
  r:=INT(dl.day)-INT(dr.day);       IF r#0 THEN RETURN r END;
  r:=INT(dl.hour)-INT(dr.hour);     IF r#0 THEN RETURN r END;
  r:=INT(dl.minute)-INT(dr.minute); IF r#0 THEN RETURN r END;
  r:=INT(dl.second)-INT(dr.second); IF r#0 THEN RETURN r END;
  RETURN INT(dl.fractions)-INT(dr.fractions);
END Compare;

PROCEDURE SubDateDays(dl,dr: DateTime): CARDINAL;
  VAR r: INTEGER;
BEGIN
  IF NOT (sys.IsValidDateTime(dl) & sys.IsValidDateTime(dr)) THEN RETURN 0 END;
  r:=Compare(dl,dr);
  IF r<=0 THEN RETURN 0 END;
  RETURN sub_days(dl,dr);
END SubDateDays;

PROCEDURE SubDateSecs(dl,dr: DateTime): CARDINAL;
  VAR r: INTEGER;
BEGIN
  IF NOT (sys.IsValidDateTime(dl) & sys.IsValidDateTime(dr)) THEN RETURN 0 END;
  r:=Compare(dl,dr);
  IF r<=0 THEN RETURN 0 END;
  RETURN sub_secs(dl,dr);
END SubDateSecs;

VAR FirstDate,SysFirstDate: DateTime; (* should be constants *)

PROCEDURE AddDateDays(D: DateTime; days: CARDINAL; VAR res: DateTime);
  VAR y,m,d: CARDINAL;
BEGIN
  res:=FirstDate;
  IF NOT sys.IsValidDateTime(D) THEN RETURN END;
  y:=D.year; m:=D.month; d:=D.day;
  add_days(y,m,d,days);
  res:=D;
  res.year:=y;
  res.month:=m;
  res.day:=d;
END AddDateDays;

PROCEDURE AddDateSecs(D: DateTime; secs: CARDINAL; VAR res: DateTime);
  VAR y,m,d,h,mi,s: CARDINAL;
BEGIN
  res:=FirstDate;
  IF NOT sys.IsValidDateTime(D) THEN RETURN END;
  y:=D.year; m:=ORD(D.month); d:=ORD(D.day);
  h:=ORD(D.hour); mi:=ORD(D.minute); s:=ORD(D.second);
  add_secs(y,m,d,h,mi,s,secs);
  res.year:=y;
  res.month:=m;
  res.day:=d;
  res.hour:=h;
  res.minute:=mi;
  res.second:=s;
  res.fractions:=D.fractions;
  res.zone:=D.zone;
  res.SummerTimeFlag:=D.SummerTimeFlag;
END AddDateSecs;

PROCEDURE TheDayNumber(d: DateTime): CARDINAL;
BEGIN
  IF NOT sys.IsValidDateTime(d) THEN RETURN 0 END;
  RETURN the_day(d.year,ORD(d.month),ORD(d.day));
END TheDayNumber;

PROCEDURE WeekDay(d: DateTime): CARDINAL;
BEGIN
  RETURN TheDayNumber(d) MOD 7
END WeekDay;

PROCEDURE TheFractionNumber(d: DateTime): CARDINAL;
  VAR r: CARDINAL;
BEGIN
  IF NOT sys.IsValidDateTime(d) THEN RETURN 0 END;
  r:=ORD(d.hour)*3600+ORD(d.minute)*60+ORD(d.second);
  RETURN r*(sys.maxSecondParts+1)+d.fractions;
END TheFractionNumber;

PROCEDURE time(): CARDINAL;
  VAR c: DateTime;
BEGIN
  IF NOT sys.CanGetClock() THEN RETURN 0 END;
  sys.GetClock(c);
  RETURN SubDateSecs(c,SysFirstDate);
END time;

PROCEDURE millisecs(): CARDINAL;
  VAR c: DateTime; r: CARDINAL;
BEGIN
  IF NOT sys.CanGetClock() THEN RETURN 0 END;
  sys.GetClock(c);
  r:=ORD(c.hour)*3600+ORD(c.minute)*60+ORD(c.second);
  IF (sys.maxSecondParts+1)>=1000 THEN
    RETURN r*1000+c.fractions*((sys.maxSecondParts+1) DIV 1000)
  ELSE
    RETURN ((r*(sys.maxSecondParts+1))+c.fractions)*
           (1000 DIV (sys.maxSecondParts+1));
  END;
END millisecs;

PROCEDURE unpack(VAR d: DateTime; secs: CARDINAL);
BEGIN
  AddDateSecs(SysFirstDate,secs,d);
END unpack;

PROCEDURE pack(d: DateTime; VAR secs: CARDINAL);
  VAR r: INTEGER;
BEGIN
  secs:=0;
  IF NOT sys.IsValidDateTime(d) THEN RETURN END;
  r:=Compare(d,SysFirstDate);
  IF r<=0 THEN RETURN END;
  d.zone:=0; d.fractions:=0; d.SummerTimeFlag:=FALSE;
  secs:=SubDateSecs(d,SysFirstDate);
END pack;

PROCEDURE weekday(time: CARDINAL): CARDINAL;
  VAR d: DateTime;
BEGIN
  unpack(d,time); RETURN WeekDay(d);
END weekday;

BEGIN
  FirstDate.month:=1;
  FirstDate.day:=1;
  FirstDate.hour:=0;
  FirstDate.minute:=0;
  FirstDate.second:=0;
  FirstDate.fractions:=0;
  FirstDate.zone:=0;
  FirstDate.SummerTimeFlag:=FALSE;
  FirstValidYear:=0;
  FirstDate.year:=FirstValidYear;
  IF NOT sys.IsValidDateTime(FirstDate) THEN
    FirstValidYear:=1; FirstDate.year:=FirstValidYear;
    ASSERT(sys.IsValidDateTime(FirstDate),101)
  END;

  SysFirstDate.year:=1970;
  SysFirstDate.month:=1;
  SysFirstDate.day:=1;
  SysFirstDate.hour:=0;
  SysFirstDate.minute:=0;
  SysFirstDate.second:=0;
  SysFirstDate.fractions:=0;
  SysFirstDate.zone:=0;
  SysFirstDate.SummerTimeFlag:=FALSE;
  ASSERT(sys.IsValidDateTime(SysFirstDate),102);
END TimeConv.