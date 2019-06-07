(* Copyright (c) xTech 1993. All Rights Reserved. *)
IMPLEMENTATION MODULE StdChans;

IMPORT  SYSTEM, IOChan, IOLink, ChanConsts, IOConsts;
IMPORT  xDevData, xlibOS;

(*----------------------------------------------------------------*)

VAR stdinp, stdout, stderr: ChanId;
    inp,out,err: ChanId;
    null: ChanId;
    did, sdid: IOLink.DeviceId;

PROCEDURE StdInChan(): ChanId;
BEGIN
  RETURN stdinp
END StdInChan;

PROCEDURE StdOutChan(): ChanId;
BEGIN
  RETURN stdout
END StdOutChan;

PROCEDURE StdErrChan(): ChanId;
BEGIN
  RETURN stderr
END StdErrChan;

(*----------------------------------------------------------------*)

PROCEDURE InChan(): ChanId;
BEGIN
  RETURN inp
END InChan;

PROCEDURE OutChan(): ChanId;
BEGIN
  RETURN out
END OutChan;

PROCEDURE ErrChan(): ChanId;
BEGIN
  RETURN err
END ErrChan;

(*----------------------------------------------------------------*)
(*
PROCEDURE redirect(new: ChanId; what: SYSTEM.int);
  VAR f: xDevData.DevData;
      x: xDevData.Object;
BEGIN
  IF IOLink.IsDevice(new,did) THEN
    x:=IOLink.DeviceTablePtrValue(new,did,IOChan.notAvailable,"");
  ELSIF IOLink.IsDevice(new,sdid) THEN
    x:=IOLink.DeviceTablePtrValue(new,sdid,IOChan.notAvailable,"");
  ELSE
    RETURN
  END;
  f:=xDevData.GetDevData(x);
  IF xlibOS.X2C_fSetStd(f^.cf,what)#0 THEN END;
END redirect;
*)
PROCEDURE SetInChan(cid: ChanId);
BEGIN
  inp:=cid;
(*  redirect(cid,xlibOS.X2C_fStdIn); *)
END SetInChan;

PROCEDURE SetOutChan(cid: ChanId);
BEGIN
  out:=cid;
(*  redirect(cid,xlibOS.X2C_fStdOut); *)
END SetOutChan;

PROCEDURE SetErrChan(cid: ChanId);
BEGIN
  err:=cid;
(*  redirect(cid,xlibOS.X2C_fStdErr); *)
END SetErrChan;

(*----------------------------------------------------------------*)

TYPE Object = xDevData.Object;

PROCEDURE NullChan(): ChanId;
BEGIN
  RETURN null
END NullChan;

PROCEDURE look(x: Object; VAR c: CHAR; VAR r: IOConsts.ReadResults);
BEGIN
  x^.result:=IOConsts.endOfInput;
  r:=x^.result;
  c:=0C;
END look;

PROCEDURE skip(x: Object);
BEGIN
  x^.result:=IOConsts.endOfInput;
END skip;

<*$< WOFF301+ *>
PROCEDURE read(x: Object; a: SYSTEM.ADDRESS; max: CARDINAL; VAR n: CARDINAL);
BEGIN
  x^.result:=IOConsts.endOfInput;
END read;

PROCEDURE write(x: Object; a: SYSTEM.ADDRESS; max: CARDINAL);
END write;
<*$>*>

PROCEDURE Ini(x: Object);
BEGIN
  x^.flags:=ChanConsts.read+ChanConsts.write;
  x^.doLook:=look;
  x^.doSkip:=skip;
  x^.doSkipLook:=look;
  x^.doTextRead:=read;
  x^.doTextWrite:=write;
  x^.doLnWrite:=skip;
  x^.doRawRead:=read;
  x^.doRawWrite:=write;
END Ini;

PROCEDURE Open(VAR cid: ChanId; what: SYSTEM.int; flags: ChanConsts.FlagSet);
  VAR f: xlibOS.X2C_OSFHANDLE;
     fn: xDevData.FileName;
    res: ChanConsts.OpenResults;
BEGIN
  IF xlibOS.X2C_fGetStd(f,what)#0 THEN
    cid:=null; RETURN
  END;
  xDevData.MakeName(fn,"",res);
  IOLink.MakeChan(sdid,cid);
  IF cid#IOChan.InvalidChan() THEN
    xDevData.Open(IOLink.DeviceTablePtrValue(cid,sdid,IOChan.notAvailable,""),
                  f,fn,flags,xDevData.bmLine,res);
  END;
END Open;

PROCEDURE OpenChannels;
  VAR ops: ChanConsts.FlagSet;
BEGIN
  IF xlibOS.X2C_IsMixAllowed() THEN ops:=ChanConsts.raw+ChanConsts.text
  ELSE ops:=ChanConsts.text
  END;
  Open(stdinp,xlibOS.X2C_fStdIn ,ChanConsts.read+ops);
  Open(stdout,xlibOS.X2C_fStdOut,ChanConsts.write+ops);
  Open(stderr,xlibOS.X2C_fStdErr,ChanConsts.write+ops);
  inp:=stdinp;
  out:=stdout;
  err:=stderr;
END OpenChannels;

(* Sets current position of stdout/stderr channels according to the real 
   position in the stdout/stderr channels.
   Returns true on success or false on errors.

   This usage scenario is the following:
   1) one calls Flush on the stdout/stderr objects
   2) executes several processes that may write something to stdout/stderr streams
   3) call to Synchronize ()
*)
PROCEDURE Synchronize (): BOOLEAN;
VAR
  res0, res1: BOOLEAN;
BEGIN
  res0 := xDevData.SynchronizePos (
             IOLink.DeviceTablePtrValue (out,sdid,IOChan.notAvailable,""));

  res1 := xDevData.SynchronizePos (
             IOLink.DeviceTablePtrValue (err,sdid,IOChan.notAvailable,""));

  RETURN res0 OR res1;
END Synchronize;


BEGIN
  IOLink.AllocateDeviceId(sdid);
  IOLink.MakeChan(sdid,null);
  IF null#IOChan.InvalidChan() THEN
    Ini(IOLink.DeviceTablePtrValue(null,sdid,IOChan.notAvailable,""));
  END;
  OpenChannels;
END StdChans.
