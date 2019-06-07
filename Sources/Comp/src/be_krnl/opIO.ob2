MODULE opIO;
IMPORT pc := pcK,
      env := xiEnv,
--     tune := opTune,
             xcStr,
             TextIO,
             StdChans,
             SYSTEM;

TYPE INT = LONGINT;

VAR needed*: BOOLEAN;   (* to toggle debugg printing on/off *)
    label: LONGINT;

(*
PROCEDURE print*(fmt-: ARRAY OF CHAR; SEQ x: SYSTEM.BYTE);
BEGIN                                (* this procedure flushes output buffer *)
  IF needed THEN
    env.config.print (fmt, x);
  END;
END print;
*)

PROCEDURE print*(fmt-: ARRAY OF CHAR; SEQ x: SYSTEM.BYTE);
  VAR msg: env.String;        (* this procedure does not flush output buffer *)
BEGIN
  IF needed THEN
    xcStr.dprn_txt(msg,fmt,x);
    TextIO.WriteString(StdChans.OutChan(),msg^);
  END;
END print;

PROCEDURE printLabel*;
BEGIN
    print("  ~~%d\n", label);
    INC(label);
END printLabel;

PROCEDURE print_pos*(tpos: pc.TPOS);
 VAR fname: pc.STRING;
    line, col: LONGINT;
BEGIN
  IF NOT tpos.IsNull() THEN
    tpos.unpack(fname, line, col);
    print("[%d:%d]", line, col);
  END;
END print_pos;

PROCEDURE note*(name-: ARRAY OF CHAR; o: pc.OBJECT);
  VAR mno: pc.Mno;
BEGIN
  print('%s("', name);
  mno := o.mno;
--  IF mno = tune.x2c_mno THEN print('X2C');
--  ELSE
    IF (mno < pc.ZEROMno) & ( SYSTEM.PRED(-mno) < LEN(pc.sys_mods^) ) THEN
      IF pc.sys_mods[SYSTEM.PRED(-mno)] # NIL THEN
        print("%s", pc.sys_mods[SYSTEM.PRED(-mno)].name^);
      END;
    ELSIF (pc.ZEROMno <= mno) & (mno < LEN(pc.mods^)) & (pc.mods[o.mno] # NIL) THEN
      print("%s", pc.mods[o.mno].name^);
    ELSE
      print("?");
    END;
--  END;
  print('(%d).%s")\n', o.mno, o.name^);
END note;

BEGIN
  needed := FALSE;
  label := 0;
END opIO.
