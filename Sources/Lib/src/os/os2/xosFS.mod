(* Copyright (c) XDS Ltd. 1992,96,98.  All Rights Reserved. *)
<* +M2EXTENSIONS *>
IMPLEMENTATION MODULE xosFS; (* Hady. 31.05.96 17:18 *)

IMPORT  SYSTEM, xmRTS, xlibOS, xOS2, OS2, TimeConv;

PROCEDURE ["C"] X2C_Exists(fname: X2C_pCHAR): BOOLEAN;
VAR
  buf: xOS2.FILESTATUS3;
  rc : xOS2.APIRET;
BEGIN
  rc := xOS2.DosQueryPathInfo(SYSTEM.CAST(xOS2.PSZ,fname), 
                              xOS2.FIL_STANDARD,
                              SYSTEM.ADR(buf), SIZE(buf));
  RETURN (rc = 0)
END X2C_Exists;

PROCEDURE ["C"] X2C_ModifyTime(  fname: X2C_pCHAR;
                        VAR time: CARD32;
                       VAR exist: BOOLEAN);
VAR
  buf: xOS2.FILESTATUS3;
  h  : xOS2.HFILE;
  cnt: CARDINAL;
  DT : TimeConv.DateTime;
  dt : xOS2.FDATE;
  tm : xOS2.FTIME;
  rc : xOS2.APIRET;
BEGIN
  rc := xOS2.DosQueryPathInfo(SYSTEM.CAST(xOS2.PSZ,fname), 
                              xOS2.FIL_STANDARD,
                              SYSTEM.ADR(buf), SIZE(buf));
  exist := (rc = 0);
  IF exist THEN
    dt := buf.fdateLastWrite;
    tm := buf.ftimeLastWrite;
    DT.second:=(tm MOD 32)*2; tm:=tm DIV 32;
    DT.minute:=tm MOD 64;
    DT.hour  :=tm DIV 64;
    DT.day   :=dt MOD 32;     dt:=dt DIV 32;
    DT.month :=dt MOD 16;
    DT.year  :=dt DIV 16 + 1980;
    TimeConv.pack (DT, time);
  END;
END X2C_ModifyTime;

PROCEDURE ["C"] X2C_Remove(fname: xmRTS.X2C_pCHAR): SYSTEM.int;
BEGIN
  RETURN xOS2.DosDelete(SYSTEM.CAST(xOS2.PSZ,fname));
END X2C_Remove;

PROCEDURE ["C"] X2C_Rename(oldname, newname: xmRTS.X2C_pCHAR): SYSTEM.int;
BEGIN
  RETURN xOS2.DosMove(SYSTEM.CAST(xOS2.PSZ,oldname),SYSTEM.CAST(xOS2.PSZ,newname));
END X2C_Rename;

PROCEDURE ["C"] X2C_FullName(full: X2C_pCHAR;
                             len: SYSTEM.CARD32;
                             name: X2C_pCHAR);
TYPE CSTRING = POINTER ["C"] TO ARRAY [0..1000H-1] OF CHAR;
VAR
  src,dest: CSTRING;
  s,d     : CARDINAL;
BEGIN
  src  := SYSTEM.CAST(SYSTEM.ADDRESS,name);
  dest := SYSTEM.CAST(SYSTEM.ADDRESS,full);
  s := 0; 
  WHILE src^[s] <> '' DO INC(s) END;
  IF (s >= 3) AND
     (((src^[1] = ':') AND (src^[2] = '\')) OR
      ((src^[0] = '\') AND (src^[1] = '\'))) THEN 
    d := 0;
  ELSIF src^[0] = '\' THEN
    xlibOS.X2C_GetCDName(full,len);
    d := 2;
  ELSE
    xlibOS.X2C_GetCDName(full,len);
    d := 0;
    WHILE dest^[d] <> '' DO INC(d) END;
    dest^[d] := '\'; INC(d);
  END;
  s := 0;
  WHILE src^[s] <> '' DO
    IF d >= len THEN dest^[0] := ''; RETURN END;
    dest^[d] := src^[s];
    INC(s); INC(d);
  END;
  IF d < len THEN dest^[d] := '' END;
END X2C_FullName;

PROCEDURE diskNum(drive: CHAR): OS2.ULONG;
BEGIN
  drive := CAP(drive);
  IF drive < "A" THEN RETURN 0 END;
  RETURN ORD(drive)-ORD("A")+1;
END diskNum;

PROCEDURE ["C"] X2C_GetDrive(VAR drive: CHAR): SYSTEM.INT32;
VAR
  rc: OS2.APIRET;
  disknum: OS2.ULONG;
  logical: OS2.ULONG;
BEGIN
  rc := OS2.DosQueryCurrentDisk(disknum, logical);
  IF rc = OS2.NO_ERROR THEN
    drive := CHR(ORD("A")+disknum-1)
  END;
  RETURN rc
END X2C_GetDrive;

PROCEDURE ["C"] X2C_SetDrive(drive: CHAR): SYSTEM.INT32;
BEGIN
  RETURN OS2.DosSetDefaultDisk(diskNum(drive));
END X2C_SetDrive;

PROCEDURE ["C"] X2C_GetDriveCDNameLength(drive: CHAR; VAR len: SYSTEM.CARD32): SYSTEM.INT32;
VAR
  Buf: ARRAY [0..OS2.CCHMAXPATH-1] OF CHAR;
  rc : OS2.APIRET;
BEGIN
  len := SIZE(Buf);
  rc := OS2.DosQueryCurrentDir(diskNum(drive),Buf,len);
  IF rc = OS2.ERROR_BUFFER_OVERFLOW THEN rc := OS2.NO_ERROR END;
  RETURN rc;
END X2C_GetDriveCDNameLength;

PROCEDURE ["C"] X2C_GetDriveCDName(drive: CHAR; dir: xmRTS.X2C_pCHAR; len: SYSTEM.CARD32): SYSTEM.INT32;
BEGIN
  RETURN OS2.DosQueryCurrentDir(diskNum(drive),dir^,len)
END X2C_GetDriveCDName;

PROCEDURE ["C"] X2C_GetLabel(drive: CHAR; label: xmRTS.X2C_pCHAR; len: SYSTEM.CARD32): SYSTEM.INT32;
VAR
  fsinfo: OS2.FSINFO;
  rc: OS2.APIRET;
  i: CARDINAL;
BEGIN
  rc := OS2.DosQueryFSInfo(diskNum(drive),
                           OS2.FSIL_VOLSER,
                           SYSTEM.ADR(fsinfo),
                           SIZE(fsinfo));
  IF rc = OS2.NO_ERROR THEN
    i := 0;  
    LOOP
      IF i = fsinfo.vol.cch THEN EXIT END;
      IF i = len THEN RETURN OS2.ERROR_BUFFER_OVERFLOW END;
      label^ := fsinfo.vol.szVolLabel[i];
      label  := SYSTEM.ADDADR(label,SIZE(CHAR));
      INC(i);
    END;
    IF i < len THEN label^ := '' END;
  END;
  RETURN rc;
END X2C_GetLabel;

END xosFS.
