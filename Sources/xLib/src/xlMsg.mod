(** Copyright (c) 1995 xTech Ltd, Russia. All Rights Reserved. *)
(** XDS Librarian. Messages output. *)
<*+ M2EXTENSIONS *>
IMPLEMENTATION MODULE xlMsg; (* Hady. 20.10.95 16:28 *)

IMPORT
  SYSTEM
  ,FormOut
  ,IOChan
  ,StdChans
  ,TextIO
  ,platform
  ;

PROCEDURE write(x: SYSTEM.ADDRESS; s-: ARRAY OF CHAR; l: INTEGER);
  VAR cid: IOChan.ChanId;
BEGIN
  cid:=SYSTEM.CAST(IOChan.ChanId,x);
  IOChan.TextWrite(cid,SYSTEM.ADR(s),l);
END write;

PROCEDURE print(f: ARRAY OF CHAR; SEQ x: SYSTEM.BYTE);
  VAR out: StdChans.ChanId;
BEGIN
  out:=StdChans.OutChan();
  FormOut.format(SYSTEM.CAST(SYSTEM.ADDRESS,out),write,f,FormOut.text,SYSTEM.ADR(x),SIZE(x));
END print;

PROCEDURE perror(f: ARRAY OF CHAR; SEQ x: SYSTEM.BYTE);
  VAR err: StdChans.ChanId;
BEGIN
  err:=StdChans.ErrChan();
  FormOut.format(SYSTEM.CAST(SYSTEM.ADDRESS,err),write,f,FormOut.text,SYSTEM.ADR(x),SIZE(x));
  IOChan.Flush(err);
END perror;

PROCEDURE error(fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.BYTE);
  VAR cid: StdChans.ChanId;
BEGIN
  cid:=StdChans.ErrChan();
  TextIO.WriteString(cid,"Error: ");
  perror(fmt,arg);
  TextIO.WriteLn(cid);
END error;

PROCEDURE warning(fmt: ARRAY OF CHAR; SEQ arg: SYSTEM.BYTE);
  VAR cid: StdChans.ChanId;
BEGIN
  cid:=StdChans.OutChan();
  TextIO.WriteString(cid,"Warning: ");
  print(fmt,arg);
  TextIO.WriteLn(cid);
END warning;

BEGIN
  FormOut.LineSeparator(platform.lineSep);
  FormOut.TextSeparator(platform.textSep);
END xlMsg.
