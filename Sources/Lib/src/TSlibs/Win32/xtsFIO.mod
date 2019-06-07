(* Copyright (C) 1999-2000 Excelsior. *)
(* FIO : Win32 *)

<*+M2ADDTYPES   *>
<*+M2EXTENSIONS *>
<*-IOVERFLOW    *>
<*-COVERFLOW    *>
<*-CHECKINDEX   *>
<*-CHECKRANGE   *>
<*-CHECKNIL     *>

IMPLEMENTATION MODULE xtsFIO;

IMPORT SYSTEM;
IMPORT FIO;
IMPORT IOChan, SysClock, ChanConsts, Strings, 
       FileSys, SysErr,
       W := Windows, Win32IOChan;

(* extended attrs handling to open file *)

VAR
  attrSav :Win32IOChan.XtdOpenAttrs;

PROCEDURE setShM(denyRd, denyWr: BOOLEAN);
VAR
  attr :Win32IOChan.XtdOpenAttrs;
BEGIN
  Win32IOChan.GetOpenAttrs(attr);
  attrSav := attr;
  IF denyRd THEN
    attr.shareMode := attr.shareMode-W.FILE_SHARE_READ;
  ELSE 
    attr.shareMode := attr.shareMode+W.FILE_SHARE_READ;
  END;
  IF denyWr THEN
    attr.shareMode := attr.shareMode-W.FILE_SHARE_WRITE;
  ELSE
    attr.shareMode := attr.shareMode+W.FILE_SHARE_WRITE;
  END;
  Win32IOChan.SetOpenAttrs(attr);
END setShM;


PROCEDURE restoreShM;
BEGIN
  Win32IOChan.SetOpenAttrs ( attrSav );
END restoreShM;


PROCEDURE mk_chan(cid :IOChan.ChanId; fh :LONGCARD; flags :ChanConsts.FlagSet; VAR res :ChanConsts.OpenResults);
BEGIN
  Win32IOChan.MakeChannel ( cid, W.HANDLE(fh), "", flags, res );
  W.SetEndOfFile ( W.HANDLE(fh) );
END mk_chan;

PROCEDURE StrToC ( S- :ARRAY OF CHAR; VAR D :ARRAY OF CHAR);
BEGIN
  D[0] := 0C;
  IF HIGH(D)>=LENGTH(S)
   THEN Strings.Insert(S,0,D);
  END;
END StrToC;

TYPE
  FileName = ARRAY[0..255] OF CHAR;

VAR
  CPath :FileName;


(*----------------------------------- Drive operations --------------------------------------*)

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

(* Set current drive: n must be 1 for 'A:', 2 for 'B:', etc. *)
PROCEDURE SetDrive (drive :LONGCARD; VAR res :CARDINAL);
VAR
  drvStr :ARRAY [0..2] OF CHAR;
BEGIN
  makeDriveStr ( drive, drvStr );
  IF NOT W.SetCurrentDirectory ( drvStr)
    THEN res := W.GetLastError()
    ELSE res := W.NO_ERROR
  END;
END SetDrive;


(* Get number of the current drive *)
PROCEDURE GetDrive () :LONGCARD;
BEGIN
  W.GetCurrentDirectory( W.MAX_PATH, CPath );
  RETURN (ORD(CPath[0]) - ORD('A') +1 );
END GetDrive;


PROCEDURE GetSpaceInfo (drive :LONGCARD; VAR info :SpaceInfo; VAR res :CARDINAL);
VAR
  drvStr    :ARRAY [0..3] OF CHAR;
  FSInfoBuf :ARRAY[0..4] OF LONGCARD;
  bperSect,
  sectperClust :LONGCARD;
BEGIN
  makeDriveStr ( drive, drvStr );
  drvStr[2] := '\';
  drvStr[3] := 0C;
  IF NOT W.GetDiskFreeSpace( drvStr, bperSect, sectperClust, info.freeClusters, info.totalClusters )
    THEN res := W.GetLastError()
    ELSE res := W.NO_ERROR;
         info.clusterSize := bperSect*sectperClust;
  END;
END GetSpaceInfo;


VAR
  FSF    :W.FILESYSTEM_FLAGS;

PROCEDURE GetLabel( drive :LONGCARD; VAR label :ARRAY OF CHAR; VAR res :CARDINAL );
VAR
  drvStr :ARRAY [0..3] OF CHAR;
  zzz    :CARDINAL;
  fsN    :ARRAY [0..10] OF CHAR;
BEGIN
  makeRootStr ( drive, drvStr );
  IF NOT W.GetVolumeInformation ( drvStr, label, HIGH(label)+1, NIL,
                                                         zzz, FSF, fsN, 7 )
    THEN res := W.GetLastError()
    ELSE res := W.NO_ERROR
  END;
END GetLabel;


(*------------------------------ directory operations --------------------------------------*)


(* Ghange the current directory *)
PROCEDURE ChgDir (path- :ARRAY OF CHAR; VAR res :CARDINAL);
BEGIN
  StrToC (path, CPath);
  IF NOT W.SetCurrentDirectory (CPath)
    THEN res := W.GetLastError()
    ELSE res := W.NO_ERROR
  END;
END ChgDir;

(* Get path to the current directory *)
PROCEDURE GetDir (drive :LONGCARD; VAR path :ARRAY OF CHAR; VAR res :CARDINAL);
VAR 
  d    : ARRAY [0..2] OF CHAR;
  dummy: W.PSTR;
  ret  : W.DWORD;
  l    : CARDINAL;
BEGIN
  IF (drive>0) THEN
    d[0] := CHR(ORD('A')+drive-1);
    d[1] := ':';
    d[2] := '';
    IF FileSys.Exists(d) THEN
      ret := W.GetFullPathName(d,HIGH(path)+1,path,dummy);
    ELSE
      res := SysErr.invalidDrive;
      RETURN;
    END;
  ELSE
    ret := W.GetCurrentDirectory(HIGH(path)+1,path) 
  END;
  IF ret = 0 THEN
    res := W.GetLastError()
  ELSIF ret > HIGH(path)+1 THEN
    res := SysErr.otherProblem;
  ELSE
    res := W.NO_ERROR
  END;
  IF res <> W.NO_ERROR THEN RETURN END;
  Strings.Delete(path,0,2);
  l := LENGTH(path);
  IF (l > 1) AND
     ((path[l-1] = '\') OR (path[l-1] = '/')) THEN
    Strings.Delete(path, l, 1);
  END;
END GetDir;


(* Make the directory specified in path *)
PROCEDURE CreateDir (path- :ARRAY OF CHAR; VAR res :LONGCARD);
BEGIN
  StrToC (path, CPath);
  IF NOT W.CreateDirectory (CPath , NIL)
    THEN res := W.GetLastError()
    ELSE res := W.NO_ERROR
  END;
END CreateDir;


(* Remove the directory specified in path *)
PROCEDURE RmvDir (path- :ARRAY OF CHAR; VAR res :LONGCARD);
BEGIN
  StrToC (path, CPath);
  IF NOT W.RemoveDirectory (CPath)
    THEN res := W.GetLastError()
    ELSE res := W.NO_ERROR
  END;
END RmvDir;


(*--------------------------------- Directory scanning ------------------------------------*)

VAR
  FindBuffer :W.WIN32_FIND_DATA; -- Returned from FindFirst/Next

 PROCEDURE TransfRes (VAR info :FIO.DirEntry);
 VAR
   st :W.SYSTEMTIME;
 BEGIN
   Strings.Assign ( FindBuffer.cFileName, info.name );
   info.attr    := SYSTEM.CAST(FIO.FileAttr, FindBuffer.dwFileAttributes);
   info.sizeL   := FindBuffer.nFileSizeLow;
   info.sizeH   := FindBuffer.nFileSizeHigh;

   W.FileTimeToLocalFileTime (FindBuffer.ftLastWriteTime, FindBuffer.ftLastWriteTime);
   W.FileTimeToDosDateTime ( FindBuffer.ftLastWriteTime, info.datePkd, info.timePkd );
 END TransfRes;


PROCEDURE ScanFirst ( path- :ARRAY OF CHAR; attr: FIO.FileAttr; VAR info :FIO.DirEntry; VAR res :CARDINAL);
VAR
  at, mhe :LONGCARD;
BEGIN
  SYSTEM.FILL (SYSTEM.ADR (info), 0, SIZE(info));

  StrToC (path, CPath);
  info.dirHandle := SYSTEM.CARD32 (W.FindFirstFile ( CPath, FindBuffer ));

  IF (W.HANDLE(info.dirHandle) = W.INVALID_HANDLE_VALUE)
    THEN res := W.GetLastError();
         RETURN;
  END;

  info.ia := attr;
  LOOP;
    IF ( (W.FILE_ATTRIBUTE__SYSTEM    IN FindBuffer.dwFileAttributes) AND NOT (FIO.system    IN attr) ) OR
       ( (W.FILE_ATTRIBUTE__HIDDEN    IN FindBuffer.dwFileAttributes) AND NOT (FIO.hidden    IN attr) ) OR
       ( (W.FILE_ATTRIBUTE__DIRECTORY IN FindBuffer.dwFileAttributes) AND NOT (FIO.directory IN attr) )
    THEN
      IF NOT W.FindNextFile (W.HANDLE(info.dirHandle), FindBuffer ) THEN
        res := W.GetLastError();
        RETURN;
      END;
    ELSE
      res := W.NO_ERROR;
      EXIT;
    END;
  END;
  TransfRes (info);
END ScanFirst;


PROCEDURE ScanNext  ( VAR info :FIO.DirEntry; VAR res :CARDINAL);
BEGIN
  LOOP;
    IF NOT W.FindNextFile (W.HANDLE(info.dirHandle), FindBuffer )
      THEN res := W.GetLastError();
           RETURN;
    END;

    IF ( (W.FILE_ATTRIBUTE__SYSTEM    IN FindBuffer.dwFileAttributes) AND NOT (FIO.system    IN info.ia) ) OR
       ( (W.FILE_ATTRIBUTE__HIDDEN    IN FindBuffer.dwFileAttributes) AND NOT (FIO.hidden    IN info.ia) ) OR
       ( (W.FILE_ATTRIBUTE__DIRECTORY IN FindBuffer.dwFileAttributes) AND NOT (FIO.directory IN info.ia) )
    THEN
    ELSE
      res := W.NO_ERROR;
      EXIT;
    END;
  END;
  TransfRes (info);
END ScanNext;


PROCEDURE ScanClose ( VAR info :FIO.DirEntry );
BEGIN
  W.FindClose(W.HANDLE(info.dirHandle));   -- Close directory handle
END ScanClose;


PROCEDURE SetFileDate( f : IOChan.ChanId ; b :SysClock.DateTime ) ;
VAR
  UTCtime   :W.FILETIME;
  localtime :W.FILETIME;
  systime   :W.SYSTEMTIME;
BEGIN
  systime.wYear   := b.year;
  systime.wMonth  := b.month;
  systime.wDay    := b.day;
  systime.wHour   := b.hour;
  systime.wMinute := b.minute;
  systime.wSecond := b.second;
  systime.wMilliseconds := b.fractions * (1000 DIV (SysClock.maxSecondParts+1));
  IF NOT W.SystemTimeToFileTime(systime, localtime) THEN RETURN END;
  IF NOT W.LocalFileTimeToFileTime(localtime, UTCtime) THEN RETURN END;
  W.SetFileTime(Win32IOChan.GetSysHandle ( f ), NIL, NIL, UTCtime);
END SetFileDate;


PROCEDURE GetFileStamp(F :IOChan.ChanId ; VAR b :SysClock.DateTime) :BOOLEAN;
VAR
  UTCtime   :W.FILETIME;
  localtime :W.FILETIME;
  systime   :W.SYSTEMTIME;
BEGIN
  IF NOT W.GetFileTime ( Win32IOChan.GetSysHandle ( F ), 
                         NIL, NIL, UTCtime ) THEN
    RETURN FALSE
  END;
  IF NOT W.FileTimeToLocalFileTime( UTCtime, localtime ) THEN
    RETURN FALSE
  END;
  IF NOT W.FileTimeToSystemTime( localtime, systime ) THEN
    RETURN FALSE
  END;
  SysClock.GetClock(b); (* To fill zone and SummerTimeFlag fields *)
  b.year   := systime.wYear;
  b.month  := systime.wMonth;
  b.day    := systime.wDay;
  b.hour   := systime.wHour;
  b.minute := systime.wMinute;
  b.second := systime.wSecond;
  b.fractions := systime.wMilliseconds DIV (1000 DIV (SysClock.maxSecondParts+1));
  RETURN TRUE;
END GetFileStamp;



END xtsFIO.

