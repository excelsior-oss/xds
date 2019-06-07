(* Copyright (C) 1999,2000 Excelsior. *)
(* FIO : OS/2 *)

<*+M2ADDTYPES   *>
<*+M2EXTENSIONS *>
<*-IOVERFLOW    *>
<*-COVERFLOW    *>
<*-CHECKINDEX   *>
<*-CHECKRANGE   *>
<*-CHECKNIL     *>

IMPLEMENTATION MODULE xtsFIO;

IMPORT SYSTEM, IOChan, SysClock, ChanConsts, Strings,
       OS2, IOChanExt := OS2IOChan;
IMPORT FIO;


(* extended attrs handling to open file *)
VAR
  attrSav :IOChanExt.XtdOpenAttrs;
--
PROCEDURE setShM(denyRd, denyWr: BOOLEAN);
VAR
  attr :IOChanExt.XtdOpenAttrs;
BEGIN
  IOChanExt.GetOpenAttrs ( attrSav );
  attr := attrSav;
  IF (denyRd) THEN
    IF (denyWr) THEN attr.shareMode := OS2.OPEN_SHARE_DENYREADWRITE;
    ELSE             attr.shareMode := OS2.OPEN_SHARE_DENYREAD; END;
  ELSE
    IF (denyWr) THEN attr.shareMode := OS2.OPEN_SHARE_DENYWRITE;
    ELSE             attr.shareMode := OS2.OPEN_SHARE_DENYNONE; END;
  END;
  IOChanExt.SetOpenAttrs ( attr );
END setShM;
--
PROCEDURE restoreShM;
BEGIN
  IOChanExt.SetOpenAttrs ( attrSav );
END restoreShM;


PROCEDURE mk_chan(cid :IOChan.ChanId; fh :LONGCARD; flags :ChanConsts.FlagSet; VAR res :ChanConsts.OpenResults);
BEGIN
  IOChanExt.MakeChannel ( cid, fh, "", flags, res );
END mk_chan;


PROCEDURE StrToC ( S- :ARRAY OF CHAR; VAR D :ARRAY OF CHAR);
BEGIN
  D[0] := 0C;
  IF HIGH(D)>=LENGTH(S)
   THEN Strings.Insert(S,0,D);
  END;
END StrToC;

VAR
  CPath :ARRAY [0..OS2.CCHMAXPATH-1] OF CHAR;

(*----------------------------------- Drive operations --------------------------------------*)

PROCEDURE GetSpaceInfo (drive :LONGCARD; VAR info :SpaceInfo; VAR res :CARDINAL);
VAR
  FSInfoBuf :ARRAY[0..4] OF LONGCARD;
BEGIN
  res := OS2.DosQueryFSInfo (drive,                 -- Drive number
                             OS2.FSIL_ALLOC,        -- Level 1 allocation info
                             SYSTEM.ADR(FSInfoBuf), -- Buffer
                             SIZE(LONGCARD) * 5 );  -- Size of buffer

  IF res = OS2.NO_ERROR
   THEN info.clusterSize   := FSInfoBuf[1] * SYSTEM.CARD16(FSInfoBuf[4]);
        info.totalClusters := FSInfoBuf[2];
        info.freeClusters  := FSInfoBuf[3];
  END;
END GetSpaceInfo;


(* Set current drive: n must be 1 for 'A:', 2 for 'B:', etc. *)
PROCEDURE SetDrive (drive :LONGCARD; VAR res :CARDINAL);
BEGIN
  res := OS2.DosSetDefaultDisk( drive );
END SetDrive;


(* Get number of the current drive *)
PROCEDURE GetDrive() :LONGCARD;
VAR
  DriveNum, DriveMap :LONGCARD;
BEGIN
  OS2.DosQueryCurrentDisk (DriveNum, DriveMap );
  RETURN DriveNum;
END GetDrive;


(* Get volume label on the drive specified *)
PROCEDURE GetLabel( drive :LONGCARD; VAR label :ARRAY OF CHAR; VAR res :CARDINAL );
TYPE
  FSINFOBUF = RECORD
                n   :LONGCARD;
                vol :OS2.VOLUMELABEL;
              END;
VAR
  VolumeInfo :FSINFOBUF;
BEGIN
  res := OS2.DosQueryFSInfo(drive,
                             OS2.FSIL_VOLSER,        -- Request volume information
                             SYSTEM.ADR(VolumeInfo), -- Buffer for information
                             SIZE(FSINFOBUF) );      -- Size of buffer

  Strings.Assign ( VolumeInfo.vol.szVolLabel, label );
END GetLabel;


(*------------------------------ directory operations --------------------------------------*)


(* Ghange the current directory *)
PROCEDURE ChgDir (path :ARRAY OF CHAR; VAR res :CARDINAL);
BEGIN
  StrToC (path, CPath);
  res := OS2.DosSetCurrentDir(CPath);
END ChgDir;


(* Make the directory specified in path *)
PROCEDURE CreateDir (path :ARRAY OF CHAR; VAR res :CARDINAL);
BEGIN
  StrToC (path, CPath);
  res := OS2.DosCreateDir(CPath , NIL);
END CreateDir;


(* Get path to the current directory *)
PROCEDURE GetDir (drive :LONGCARD; VAR path :ARRAY OF CHAR; VAR res :CARDINAL);
VAR
  sz :ARRAY[0..OS2.CCHMAXPATH] OF CHAR;
  l  :OS2.ULONG;
BEGIN
  l  := OS2.CCHMAXPATH;
  res := OS2.DosQueryCurrentDir(drive, sz, l);
  IF (res = 0) THEN
    l := LENGTH(sz) + 1;
    IF (l > HIGH(path)) THEN
      res := OS2.ERROR_BUFFER_OVERFLOW;
    ELSE
      SYSTEM.MOVE(SYSTEM.ADR(sz), SYSTEM.ADR(path[1]), l);
      path[0] := '\';
    END;
  END;
END GetDir;


(* Remove the directory specified in path *)
PROCEDURE RmvDir (path :ARRAY OF CHAR; VAR res :CARDINAL);
BEGIN
  StrToC (path, CPath);
  res := OS2.DosDeleteDir(CPath);
END RmvDir;



(*--------------------------------- Directory scanning ------------------------------------*)

CONST
 ResBufLen = SIZE(OS2.FILEFINDBUF3);

VAR
  FindBuffer :OS2.FILEFINDBUF3; -- Returned from FindFirst/Next
  FindCount  :LONGCARD;

 PROCEDURE TransfRes (VAR info :FIO.DirEntry);
 BEGIN
   Strings.Assign ( FindBuffer.achName, info.name );
   info.datePkd := FindBuffer.fdateLastWrite;
   info.timePkd := FindBuffer.ftimeLastWrite;
   info.attr    := SYSTEM.CAST(FIO.FileAttr, FindBuffer.attrFile);
   info.sizeH   := 0;
   info.sizeL   := FindBuffer.cbFile;
 END TransfRes;


PROCEDURE ScanFirst ( path :ARRAY OF CHAR; attr: FIO.FileAttr; VAR info :FIO.DirEntry; VAR res :CARDINAL);
VAR
  at :LONGCARD;
BEGIN
  StrToC (path, CPath);
  info.dirHandle := OS2.HDIR_SYSTEM;
  FindCount      := 1;                                 -- Look for 1 file at a time
  at             := OS2.FILE_ARCHIVED + OS2.FILE_READONLY;
  IF (FIO.system    IN attr) THEN at := at + OS2.FILE_SYSTEM;    END;
  IF (FIO.hidden    IN attr) THEN at := at + OS2.FILE_HIDDEN;    END;
  IF (FIO.directory IN attr) THEN at := at + OS2.FILE_DIRECTORY; END;

  res := OS2.DosFindFirst( path,              -- File pattern
                           info.dirHandle,    -- Directory search handle
                           at,                     -- Search attribute
                           SYSTEM.ADR(FindBuffer), -- Result buffer
                           ResBufLen,              -- Result buffer length
                           FindCount,              -- Number of entries to find
                           OS2.FIL_STANDARD);      -- Return level 1 file info

  IF res = OS2.NO_ERROR THEN TransfRes (info); END;
END ScanFirst;


PROCEDURE ScanNext  ( VAR info :FIO.DirEntry; VAR res :CARDINAL);
BEGIN
  FindCount := 1;
  res := OS2.DosFindNext( info.dirHandle,         -- Directory handle
                          SYSTEM.ADR(FindBuffer), -- Result buffer
                          ResBufLen,              -- Result buffer length
                          FindCount);             -- Number of entries to find

  IF res = OS2.NO_ERROR THEN TransfRes (info); END;
END ScanNext;


PROCEDURE ScanClose ( VAR info :FIO.DirEntry );
BEGIN
  OS2.DosFindClose(info.dirHandle);   -- Close directory handle
END ScanClose;

PROCEDURE makeDriveStr ( drive :LONGCARD; VAR drvStr :ARRAY OF CHAR );
BEGIN
  drvStr [0] := CHR ( ORD('A') + drive - 1 );
  drvStr [1] := ':';
  drvStr [2] := 0C;
END makeDriveStr;

PROCEDURE makeRootStr ( drive :LONGCARD; VAR drvStr :ARRAY OF CHAR );
BEGIN
  makeDriveStr ( drive, drvStr );
  drvStr[2] := '\';
  drvStr[3] := 0C;
END makeRootStr;

PROCEDURE SetFileDate( f : IOChan.ChanId; b :SysClock.DateTime);
VAR
  info :OS2.FILESTATUS3;
  dt   :OS2.FDATE;
  tm   :OS2.FTIME;
BEGIN
  tm := b.hour*800H+
        b.minute*20H+
        b.second DIV 2;
  dt := (b.year-1980)*200H+
        b.month*20H+
        b.day;
  IF OS2.DosQueryFileInfo ( IOChanExt.GetSysHandle ( f ), 
                          OS2.FIL_STANDARD, 
                          SYSTEM.ADR(info),
                          SIZE(info) ) <> OS2.NO_ERROR THEN
    RETURN 
  END;
  WITH info DO
    fdateCreation   := 0; (* 0 = no change *)
    ftimeCreation   := 0;
    fdateLastAccess := 0;  
    ftimeLastAccess := 0; 
    fdateLastWrite  := dt;
    ftimeLastWrite  := tm;
  END;
  IF OS2.DosSetFileInfo ( IOChanExt.GetSysHandle ( f ), 
                        OS2.FIL_STANDARD, 
                        SYSTEM.ADR(info),
                        SIZE(info) ) <> OS2.NO_ERROR THEN
    RETURN 
  END;
END SetFileDate;

PROCEDURE GetFileStamp(F :IOChan.ChanId ; VAR b :SysClock.DateTime) :BOOLEAN;
VAR
  info :OS2.FILESTATUS3;
  dt   :OS2.FDATE;
  tm   :OS2.FTIME;
BEGIN
  IF OS2.DosQueryFileInfo ( IOChanExt.GetSysHandle ( F ), 
                          OS2.FIL_STANDARD, 
                          SYSTEM.ADR(info),
                          SIZE(info) ) <> OS2.NO_ERROR THEN
    RETURN FALSE
  END;
  SysClock.GetClock(b); (* To fill zone and SummerTimeFlag fields *)
(* !!! The following unpack routine could be called from xosFS *)      
  dt := info.fdateLastWrite;
  tm := info.ftimeLastWrite;
  b.fractions := 0;
  b.second := (tm MOD 32)*2; tm:=tm DIV 32;
  b.minute := tm MOD 64;
  b.hour   := tm DIV 64;
  b.day    := dt MOD 32;     dt:=dt DIV 32;
  b.month  := dt MOD 16;
  b.year   := dt DIV 16 + 1980;
  RETURN TRUE;
END GetFileStamp;


PROCEDURE Assign(VAR ShareCompat,
                     ShareDenyRW,
                     ShareDenyRD,
                     ShareDenyWR,
                     ShareDenyNone :LONGCARD);
BEGIN
  ShareCompat    := OS2.OPEN_SHARE_DENYNONE;
  ShareDenyRW    := OS2.OPEN_SHARE_DENYREADWRITE;
  ShareDenyRD    := OS2.OPEN_SHARE_DENYREAD;
  ShareDenyWR    := OS2.OPEN_SHARE_DENYWRITE;
  ShareDenyNone  := OS2.OPEN_SHARE_DENYNONE;
END Assign;

BEGIN
END xtsFIO.

