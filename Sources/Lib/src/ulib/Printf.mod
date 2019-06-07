(* Copyright (c) 1996 xTech Ltd, Russia. All Rights Reserved. *)
<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE Printf;

IMPORT FormOut, IOChan, StdChans, SYSTEM;

PROCEDURE write_file(x: SYSTEM.ADDRESS; s-: ARRAY OF CHAR; l: INTEGER);
  VAR cid: IOChan.ChanId;
BEGIN
  cid:=SYSTEM.CAST(ChanId,x);
  IOChan.TextWrite(cid,SYSTEM.ADR(s),l);
END write_file;

PROCEDURE printf(f: ARRAY OF CHAR; SEQ x: SYSTEM.BYTE);
  VAR out: StdChans.ChanId;
BEGIN
  out:=StdChans.OutChan();
  FormOut.format(SYSTEM.CAST(SYSTEM.ADDRESS,out),write_file,f,FormOut.text,SYSTEM.ADR(x),SIZE(x));
END printf;

PROCEDURE fprintf(cid: ChanId; f: ARRAY OF CHAR; SEQ x: SYSTEM.BYTE);
BEGIN
  FormOut.format(SYSTEM.CAST(SYSTEM.ADDRESS,cid),write_file,f,FormOut.text,SYSTEM.ADR(x),SIZE(x));
END fprintf;

TYPE str_buf = RECORD
       adr: SYSTEM.ADDRESS;
       len: CARDINAL;
       pos: CARDINAL;
     END;
     str_buf_ptr = POINTER TO str_buf;

PROCEDURE write_str(x: SYSTEM.ADDRESS; s-: ARRAY OF CHAR; l: INTEGER);
  VAR buf: str_buf_ptr; i: CARDINAL;
BEGIN
  buf:=SYSTEM.CAST(str_buf_ptr,x); i:=0;
  WHILE (l>0) & (buf^.pos<buf^.len) DO
    SYSTEM.PUT(buf^.adr,s[i]);
    INC(buf^.pos); DEC(l); INC(i);
    buf^.adr:=SYSTEM.ADDADR(buf^.adr,SIZE(CHAR));
  END;
END write_str;

PROCEDURE sprintf(VAR o: ARRAY OF CHAR; f: ARRAY OF CHAR; SEQ x: SYSTEM.BYTE);
  VAR buf: str_buf;
BEGIN
  buf.adr:=SYSTEM.ADR(o); buf.pos:=0; buf.len:=HIGH(o)+1;
  FormOut.format(SYSTEM.ADR(buf),write_str,f,FormOut.text,SYSTEM.ADR(x),SIZE(x));
  IF buf.pos<buf.len THEN o[buf.pos]:=0C END;
END sprintf;

END Printf.
