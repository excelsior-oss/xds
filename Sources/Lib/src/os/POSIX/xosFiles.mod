(* Copyright (c) xTech 1992,96.  All Rights Reserved *)
<* +M2EXTENSIONS *>
<* +NOHEADER *>
<* -gendebug *>
IMPLEMENTATION MODULE xosFiles; (* Hady. 19.06.96 14:33 *)

IMPORT  SYSTEM, xPOSIX, xlibOS, xrInt64, ChanConsts;

(*
 * On non-UNIX platforms, the dup2 function, referenced from a 
 * SYSTEM.CODE call in X2C_Dup2, is defined in <io.h>.
 * Although in C declaring a function prototype is not essential to call it,
 * in IBM C Set++ for OS/2 header files this function is mapped
 * to another using compiler-specific pragmas. So this module can
 * not be compiled with C Set++ unless it contains #include <io.h> line. 
 *
 * Since a fake <io.h> including unistd.h is also provided, this should work
 * on POSIX platforms as well.
 *)

(*IMPORT io;*)

TYPE
  INT32 = SYSTEM.INT32;
  File = POINTER TO xPOSIX.FILE;
  pFile = POINTER TO File;
  int = SYSTEM.int;

  ADDRESS = SYSTEM.ADDRESS;
  CARD32 = SYSTEM.CARD32;
  OpenResults = ChanConsts.OpenResults;

(*
  X2C_FPOS = xlibOS.X2C_FPOS;
  CARD32 = SYSTEM.CARD32;
  INT32 = SYSTEM.INT32;
  int = SYSTEM.int;
  OpenResults = xlibOS.OpenResults;
*)

PROCEDURE ["C"] X2C_IsMixAllowed(): BOOLEAN;
BEGIN
  SYSTEM.CODE("#ifdef _unix");
  SYSTEM.CODE("return (X2C_BOOLEAN)1;");
  SYSTEM.CODE("#endif");
  RETURN FALSE;
END X2C_IsMixAllowed;

PROCEDURE ["C"] X2C_fGetXAttrs(VAR data: xlibOS.X2C_FXATTRS);
BEGIN
  (* intentionally empty - nothing to controll here *)
END X2C_fGetXAttrs;

PROCEDURE ["C"] X2C_fSetXAttrs(VAR data: xlibOS.X2C_FXATTRS);
BEGIN
  (* intentionally empty - nothing to controll here *)
END X2C_fSetXAttrs;

PROCEDURE ["C"] X2C_fOpen(VAR f: xlibOS.X2C_OSFHANDLE; name: ARRAY OF CHAR; tags: BITSET): OpenResults;

  CONST mix = X2C_fModeRaw+X2C_fModeText;

  VAR mode: ARRAY [0..3] OF CHAR; c: CARDINAL; cf: File;
BEGIN
  IF (tags*mix=mix) & NOT X2C_IsMixAllowed() THEN
    RETURN ChanConsts.noMixedOperations;
  END;
  c:=ORD(tags*X2C_fModeText#{}) +
     ORD(tags*X2C_fAccessRead#{})*2 +
     ORD(tags*X2C_fAccessWrite#{})*4 +
     ORD(tags*X2C_fModeNew#{})*8;
  CASE c OF
    |0,2  : mode:="rb";
    |1,3  : mode:="r";
    |4,6  : mode:="r+b";
    |5,7  : mode:="r+";
    |8,12 : mode:="wb";
    |9,13 : mode:="w";
    |10,14: mode:="w+b";
    |11,15: mode:="w+";
  END;
  cf:=xPOSIX.fopen(name,mode);
  IF cf=NIL THEN
    SYSTEM.CODE("#ifdef ENOENT");
    IF xPOSIX.errno=xPOSIX.ENOENT THEN RETURN ChanConsts.noSuchFile END;
    SYSTEM.CODE("#endif");

    SYSTEM.CODE("#ifdef ENOENT");
    IF xPOSIX.errno=xPOSIX.ENOENT THEN RETURN ChanConsts.noSuchFile END;
    SYSTEM.CODE("#endif");

    SYSTEM.CODE("#ifdef EMFILE");
    IF xPOSIX.errno=xPOSIX.EMFILE THEN RETURN ChanConsts.tooManyOpen END;
    SYSTEM.CODE("#endif");

    SYSTEM.CODE("#ifdef EACCES");
    IF xPOSIX.errno=xPOSIX.EACCES THEN RETURN ChanConsts.wrongPermissions END;
    SYSTEM.CODE("#endif");

    SYSTEM.CODE("#ifdef ENOSPC");
    IF xPOSIX.errno=xPOSIX.ENOSPC THEN RETURN ChanConsts.noRoomOnDevice END;
    SYSTEM.CODE("#endif");

    SYSTEM.CODE("#ifdef EISDIR");
    IF xPOSIX.errno=xPOSIX.EISDIR THEN RETURN ChanConsts.wrongFileType END;
    SYSTEM.CODE("#endif");

    RETURN ChanConsts.otherProblem

  END;
  f:=SYSTEM.CAST(xlibOS.X2C_OSFHANDLE,cf);
  RETURN ChanConsts.opened;
END X2C_fOpen;


PROCEDURE ["C"] X2C_fClose(VAR f: xlibOS.X2C_OSFHANDLE): int;
  VAR cf: File;
BEGIN
  cf:=SYSTEM.CAST(File,f);
  RETURN xPOSIX.fclose(cf^);
END X2C_fClose;

PROCEDURE ["C"] X2C_fRead(f: xlibOS.X2C_OSFHANDLE; buf: ADDRESS; size: CARD32; VAR rd: CARD32): int;
  VAR cf: File;
BEGIN
  cf:=SYSTEM.CAST(File,f);
  rd:=xPOSIX.fread(buf,1,size,cf^);
  IF (rd<size) & (xPOSIX.feof(cf^)=0) THEN RETURN xPOSIX.errno END;
  RETURN 0;
END X2C_fRead;

PROCEDURE ["C"] X2C_fWrite(f: xlibOS.X2C_OSFHANDLE; buf: ADDRESS; size: CARD32; VAR wr: CARD32): int;
  VAR cf: File;
BEGIN
  cf:=SYSTEM.CAST(File,f);
  wr:=xPOSIX.fwrite(buf,1,size,cf^);
  IF wr<size THEN RETURN xPOSIX.errno END;
  RETURN 0;
END X2C_fWrite;

PROCEDURE ["C"] X2C_fSeek(f: xlibOS.X2C_OSFHANDLE; VAR ofs: xlibOS.X2C_FPOS; how: int): int;
  VAR pos: INT32;
      cf: File;
BEGIN
  cf:=SYSTEM.CAST(File,f);
  IF xrInt64.X2C_64TOINT(pos,ofs) THEN RETURN 1 END;
  IF xPOSIX.fseek(cf^,pos,how)#0 THEN RETURN xPOSIX.errno END;
  pos:=xPOSIX.ftell(cf^);
  IF pos=-1 THEN RETURN xPOSIX.errno END;
  xrInt64.X2C_INTTO64(ofs,pos);
  RETURN 0;
END X2C_fSeek;

PROCEDURE ["C"] X2C_fTell(f: xlibOS.X2C_OSFHANDLE; VAR ofs: xlibOS.X2C_FPOS): int;
  VAR pos: INT32; cf: File;
BEGIN
  cf:=SYSTEM.CAST(File,f);
  pos:=xPOSIX.ftell(cf^);
  IF pos=-1 THEN RETURN xPOSIX.errno END;
  xrInt64.X2C_INTTO64(ofs,pos);
  RETURN 0;
END X2C_fTell;

PROCEDURE ["C"] X2C_fSize(f: xlibOS.X2C_OSFHANDLE; VAR siz: xlibOS.X2C_FPOS): int;
  VAR cp,eof: INT32; cf: File;
BEGIN
  cf:=SYSTEM.CAST(File,f);
  cp:=xPOSIX.ftell(cf^);
  IF cp=-1 THEN RETURN xPOSIX.errno END;
  IF xPOSIX.fseek(cf^,0,xPOSIX.SEEK_END)#0 THEN RETURN xPOSIX.errno END;
  eof:=xPOSIX.ftell(cf^);
  IF eof=-1 THEN RETURN xPOSIX.errno END;
  IF xPOSIX.fseek(cf^,cp,xPOSIX.SEEK_SET)#0 THEN RETURN xPOSIX.errno END;
  xrInt64.X2C_INTTO64(siz,eof);
  RETURN 0;
END X2C_fSize;

PROCEDURE ["C"] X2C_fFlush(f: xlibOS.X2C_OSFHANDLE): int;
  VAR cf: File;
BEGIN
  cf:=SYSTEM.CAST(File,f);
  IF xPOSIX.fflush(cf^)=0 THEN RETURN 0 END;
  RETURN xPOSIX.errno;
END X2C_fFlush;

PROCEDURE ["C"] / X2C_ChSize(f: File; sz: INT32): int;
  (* implemented in "x2clib.c" *)

PROCEDURE ["C"] X2C_fChSize(f: xlibOS.X2C_OSFHANDLE): int;
  VAR cf: File; pos: INT32;
BEGIN
  cf:=SYSTEM.CAST(File,f);
  IF xPOSIX.fflush(cf^)#0 THEN RETURN xPOSIX.errno  END;
  pos:=xPOSIX.ftell(cf^);
  IF pos=-1 THEN RETURN xPOSIX.errno END;
  IF X2C_ChSize(cf,pos)=0 THEN RETURN 0 END;
  RETURN xPOSIX.errno;
END X2C_fChSize;

PROCEDURE ["C"] X2C_fGetStd(VAR f: xlibOS.X2C_OSFHANDLE; what: int): int;
BEGIN
  CASE what OF
    |X2C_fStdIn : f:=SYSTEM.CAST(xlibOS.X2C_OSFHANDLE,xPOSIX.stdin);
    |X2C_fStdOut: f:=SYSTEM.CAST(xlibOS.X2C_OSFHANDLE,xPOSIX.stdout);
  ELSE
    f:=SYSTEM.CAST(xlibOS.X2C_OSFHANDLE,xPOSIX.stderr);
  END;
  RETURN 0;
END X2C_fGetStd;

PROCEDURE X2C_dup2(VAR f1["C"]: xPOSIX.FILE; VAR f2["C"]: SYSTEM.ADDRESS): int;
BEGIN
  SYSTEM.CODE("#if defined(_unix) || defined(_msdos)");
  SYSTEM.CODE("  return dup2(fileno(f1),fileno(*(FILE**)f2));");
  SYSTEM.CODE("#else");
    SYSTEM.CODE("(*f2)=(X2C_ADDRESS)f1;");
    RETURN  0;
  SYSTEM.CODE("#endif");
END X2C_dup2;

PROCEDURE ["C"] X2C_fSetStd(new: xlibOS.X2C_OSFHANDLE; which: int): int;
  VAR cf: File; yf: File;
BEGIN
  cf:=SYSTEM.CAST(File,new);
  CASE which OF
    |X2C_fStdIn : yf:=SYSTEM.CAST(SYSTEM.ADDRESS,xPOSIX.stdin);
    |X2C_fStdOut: yf:=SYSTEM.CAST(SYSTEM.ADDRESS,xPOSIX.stdout);
  ELSE
    yf:=SYSTEM.CAST(SYSTEM.ADDRESS,xPOSIX.stderr);
  END;
  RETURN X2C_dup2(cf^,yf);
END X2C_fSetStd;

(*
PROCEDURE ["C"] X2C_fNoBuffer(f: xlibOS.X2C_OSFHANDLE): BOOLEAN;
BEGIN
  RETURN FALSE
END X2C_fNoBuffer;
*)

PROCEDURE ["C"] / X2C_GetFileType(f: xlibOS.X2C_OSFHANDLE): SYSTEM.int;
(* Implemented in "x2cLib.c" *)

PROCEDURE ["C"] X2C_fGetFileType(f: xlibOS.X2C_OSFHANDLE): SYSTEM.int;
BEGIN
  RETURN X2C_GetFileType(f);
END X2C_fGetFileType;

PROCEDURE ["C"] X2C_GetModuleName ( hmod      :SYSTEM.ADDRESS;
                                    VAR mname :ARRAY OF CHAR;
                                    nmlen     :SYSTEM.CARD32 );
BEGIN
(* intentionally empty *)
END X2C_GetModuleName;  

END xosFiles.
