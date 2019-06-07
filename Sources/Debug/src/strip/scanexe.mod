-- Предоставляет функции для настройки обработчиков
-- отладочной информации указанной программы

IMPLEMENTATION MODULE ScanExe;

<* Storage+ *>

IMPORT sys := SYSTEM;
IMPORT dll := dllRTS;
IMPORT ioc := IOChan;
IMPORT rio := RawIO;
IMPORT rf  := RndFile;
IMPORT io  := IOChan;
IMPORT xfp := xFilePos;
IMPORT fmt := FormStr;

IMPORT dt  := DI_Types;
IMPORT bld := DI_Build;
IMPORT dri := DI_Read;
IMPORT kt  := KrnTypes;
IMPORT re  := ReadExp;
IMPORT fil := File;
IMPORT Dll;


<* IF env_target = "x86os2" THEN *>

IMPORT OS2;

PROCEDURE FillDebugInfo(VAR info: kt.EXEC_INFO; VAR raw: dt.RAW_DEBUG_INFO): BOOLEAN;

VAR
  file : rf.ChanId;
  res  : rf.OpenResults;

  PROCEDURE read(VAR buf: ARRAY OF sys.LOC): BOOLEAN;
    VAR x: CARDINAL;
  BEGIN
    x:=SIZE(buf);
    io.RawRead(file, sys.ADR(buf), x, x);
    RETURN x=SIZE(buf);
  END read;

VAR
  entry: RECORD
           name: ARRAY [0..3] OF CHAR;
           offs: CARDINAL;
         END;
  fp   : xfp.FilePos;
  ps   : CARDINAL;

BEGIN
  rf.OpenOld(file, info.full_name, rf.raw, res);
  IF res # rf.opened THEN RETURN FALSE END;
  fp := rf.EndPos(file);


  IF NOT xfp.PosToCard(ps, fp) THEN RETURN FALSE END;
  IF ps <= 8 THEN RETURN FALSE END;
  xfp.CardToPos(fp, ps-8);
  rf.SetPos(file, fp);

  IF NOT read(entry) THEN RETURN FALSE END;

  IF entry.offs < ps THEN
    COPY(entry.name, info.DebugInfoTag);
    info.DebugInfoStart := ps - entry.offs;
    info.DebugInfoSize := entry.offs;

    xfp.CardToPos(fp, info.DebugInfoStart);
    rf.SetPos(file, fp);
    IF NOT read(entry) OR (entry.name # info.DebugInfoTag) THEN
      info.DebugInfoTag  := '';
      info.DebugInfoSize := 0;
    END;
  ELSE
    info.DebugInfoSize := 0;
  END;
  IF info.DebugInfoSize # 0 THEN
    NEW(raw, info.DebugInfoSize);
    xfp.CardToPos(fp, info.DebugInfoStart);
    rf.SetPos(file, fp);
    IF NOT read(raw^) THEN DISPOSE(raw); RETURN FALSE END;
  END;
  rf.Close(file);
  RETURN TRUE;
END FillDebugInfo;


PROCEDURE BuildObjects (full_name-: ARRAY OF CHAR; VAR info: kt.EXEC_INFO): BOOLEAN;
VAR
  file : rf.ChanId;
  res  : rf.OpenResults;

  PROCEDURE fseek(pos: CARDINAL);
  VAR
    fp: rf.FilePos;
  BEGIN
    xfp.CardToPos(fp, pos);
    rf.SetPos(file, fp);
  END fseek;

TYPE
  DB = sys.CARD8;
  DW = sys.CARD16;
  DD = sys.CARD32;

  LX_HEADER = RECORD
    signature: ARRAY [0..1] OF CHAR;
    border     : DB;     (* byte ordering                             *)
    worder     : DB;     (* word ordering                             *)
    lefl       : DD;     (* Linear EXE Format Level                   *)
    cpu        : DW;     (* Module CPU type                           *)
    ostype     : DW;     (* Module OS type                            *)
    mver       : DD;     (* Version of LX module                      *)
    mflags     : BITSET; (* Flag bits for the module                  *)
    mpages     : DD;     (* Number of pages in module                 *)
    eipobj     : DD;     (* The Object number to which the Entry      *)
                         (* Address is relative                       *)
    eip        : DD;     (*  Entry address of module                  *)
    espobj     : DD;     (* The Object number to  which the ESP is    *)
                         (*  relative                                 *)
    esp        : DD;     (* Starting stack address of module          *)
    psize      : DD;     (* The size of one page for this system      *)
    poffshift  : DD;     (* The shift left  bits  for  page offsets   *)
    fupssize   : DD;     (* Total  size  of  the  fixup information   *)
                         (* in bytes                                  *)
    fupschsum  : DD;     (* Checksum for fixup information            *)
    lderssize  : DD;     (* Size  of  memory  resident tables         *)
    lderchsum  : DD;     (* Checksum   for  loader  section           *)
    objtoffs   : DD;     (* Object Table offset                       *)
    objinmod   : DD;     (* Object Table Count                        *)
    objptoffs  : DD;     (* Object Page Table offset                  *)
    objipoff   : DD;     (* Object  Iterated  Pages  offset           *)
    restoffs   : DD;     (* Resource Table offset                     *)
    restentr   : DD;     (* Number  of entries  in Resource Table     *)
    resntoffs  : DD;     (* Resident Name Table offset                *)
    enttoffs   : DD;     (* Entry Table offset                        *)
    moddoffs   : DD;     (* Module  Format Directives Table offset    *)
    moddirvs   : DD;     (* Number of Module Format Directives in     *)
                         (* the Table                                 *)
    fupptoffs  : DD;     (* Fixup Page Table offset                   *)
    fuptroffs  : DD;     (* Fixup Record Table Offset                 *)
    impmodoffs : DD;     (* Import  Module Name Table offset          *)
    impmodentr : DD;     (* The number of entries in the Import       *)
                         (* Module Name Table                         *)
    impprtoffs : DD;     (* Import Procedure  Name Table offset       *)
    ppchkoffs  : DD;     (* Per-Page  Checksum  Table offset          *)
    dpoffs     : DD;     (* Data Pages Offset                         *)
    numprelp   : DD;     (* Number of Preload pages for this module   *)
    nonrestoffs: DD;     (* Non-Resident  Name  Table offset          *)
    nonrestlen : DD;     (* Number of bytes in the Non-resident       *)
                         (* name table                                *)
    nrestchksum: DD;     (* Non-Resident  Name  Table Checksum        *)
    autodsobj  : DD;     (* The Auto Data  Segment Object number      *)
    debinfoffs : DD;     (* Debug Information offset                  *)
    debinflen  : DD;     (* Debug Information length                  *)
    ninstpp    : DD;     (* Instance  pages  in  preload section      *)
    ninstdem   : DD;     (* Instance  pages  in  demand section       *)
    heapsize   : DD;     (* Heap size added to the Auto DS Object     *)
  END;

  OBJ    = RECORD
             vSize: DD;
             rAddr: DD;
             Flags: DD;
             pInd : DD;
             pEntr: DD;
             Reserved: DD;
           END;

VAR
  offs, i: CARDINAL;
  hdr    : LX_HEADER;
  obj    : OBJ;

BEGIN
  info.Objects := NIL;
  info.N_Objects := 0;

  rf.OpenOld(file, full_name, rf.raw, res);
  IF res # rf.opened THEN RETURN FALSE; END;

  fseek(03CH);
  rio.Read (file, offs);
  fseek (offs);
  rio.Read(file, hdr);
  IF (hdr.signature # 'LX') THEN RETURN FALSE END;

  WITH info DO
    IF {8, 9} <= hdr.mflags THEN
      app_type := kt.windowed
    ELSE
      app_type := kt.console
    END;

    N_Objects := hdr.objinmod;
    IF hdr.eipobj # 0 THEN
      Code_Object :=  hdr.eipobj - 1;
    ELSE
      Code_Object :=  MAX(CARDINAL);
    END;

    NEW(Objects, N_Objects);

    fseek(offs+hdr.objtoffs);
    FOR i := 1 TO N_Objects DO
      rio.Read(file, obj);
      WITH Objects^[i-1] DO
        Attributes := kt.ATTRIBS{};
        RelocationBase    := obj.rAddr;
        IF 0 IN BITSET(obj.Flags) THEN
          INCL( Attributes, kt.read);
        END;
        IF 1 IN BITSET(obj.Flags) THEN
          INCL( Attributes, kt.write);
        END;
        IF 2 IN BITSET(obj.Flags) THEN
          INCL( Attributes, kt.execute);
          IF Code_Object = MAX(CARDINAL) THEN Code_Object := i-1; END;
        END;
        IF 13 IN BITSET(obj.Flags) THEN
          INCL( Attributes, kt.bit_32);
        END;
        IF (4 IN BITSET(obj.Flags)) THEN
          Begin := MAX(INTEGER);
          End := MAX(INTEGER);
        ELSIF i # Code_Object+1 THEN
          Begin := RelocationBase;
          End := Begin + obj.vSize - 1;
        ELSE
          Begin := 0;
          End := obj.vSize - 1;
        END;
      END;
    END;
  END;
  rf.Close(file);
  RETURN TRUE;
EXCEPT
  IF info.Objects # NIL THEN DISPOSE(info.Objects); END;
  info.N_Objects := 0;
  RETURN FALSE;
END BuildObjects;


<* ELSIF env_target = "x86nt" THEN *>

IMPORT win := Windows;
IMPORT wio:= Win32IOChan;

PROCEDURE FillDebugInfo(VAR info: kt.EXEC_INFO; VAR raw: dt.RAW_DEBUG_INFO): BOOLEAN;

VAR
  file : rf.ChanId;
  res  : rf.OpenResults;

  PROCEDURE read(VAR buf: ARRAY OF sys.LOC): BOOLEAN;
    VAR x: CARDINAL;
  BEGIN
    x:=SIZE(buf);
    io.RawRead(file, sys.ADR(buf), x, x);
    RETURN x=SIZE(buf);
  END read;

VAR
  entry: RECORD
           name: ARRAY [0..3] OF CHAR;
           offs: CARDINAL;
         END;
  fp   : xfp.FilePos;
  ps   : CARDINAL;

  lpFile : win.PVOID;
  hMapFile: win.HANDLE;

BEGIN
  rf.OpenOld(file, info.full_name, rf.raw, res);
  IF res # rf.opened THEN RETURN FALSE END;
  fp := rf.EndPos(file);


  IF NOT xfp.PosToCard(ps, fp) THEN RETURN FALSE END;
  IF ps <= 8 THEN RETURN FALSE END;
  xfp.CardToPos(fp, ps-8);
  rf.SetPos(file, fp);

  IF NOT read(entry) THEN RETURN FALSE END;

  IF entry.offs < ps THEN
    COPY(entry.name, info.DebugInfoTag);
    info.DebugInfoStart := ps - entry.offs;
    info.DebugInfoSize := entry.offs;

    xfp.CardToPos(fp, info.DebugInfoStart);
    rf.SetPos(file, fp);
    IF NOT read(entry) OR (entry.name # info.DebugInfoTag) THEN
      info.DebugInfoTag  := '';
      info.DebugInfoSize := 0;
    END;
  ELSE
    info.DebugInfoSize := 0;
  END;
  raw:= NIL;
  IF info.DebugInfoSize # 0 THEN
    hMapFile := win.CreateFileMapping (wio.GetSysHandle(file), NIL, win.PAGE_READONLY, 0, 0, NIL);
    ASSERT(hMapFile # NIL);
    (* map view of entire file *)
    lpFile := win.MapViewOfFile (hMapFile, win.FILE_MAP_READ, 0, 0, 0);
    ASSERT(lpFile # NIL);
    raw:= sys.ADDRESS(lpFile)+info.DebugInfoStart;
  END;
  rf.Close(file);
  RETURN TRUE;
END FillDebugInfo;


PROCEDURE BuildObjects (full_name-: ARRAY OF CHAR; VAR info: kt.EXEC_INFO): BOOLEAN;
TYPE
  A = ARRAY [0..15] OF CARDINAL;

VAR
  hMapFile      : win.HANDLE;
  lpFile        : win.PVOID;
  plpFile       : POINTER TO win.USHORT;
  Header        : POINTER TO win.IMAGE_NT_HEADERS;
  psh           : POINTER TO ARRAY [0..03FFEH] OF win.IMAGE_SECTION_HEADER;
  hfile         : win.HFILE;
  ReOpenBuff    : win.OFSTRUCT;
  nSections     : CARDINAL;
  i             : CARDINAL;
  ImageHdrOffset: CARDINAL;
  pA            : POINTER TO A;

  MODULE Exit;
  IMPORT win, hfile, lpFile, hMapFile;
  BEGIN
  FINALLY
    (* clean up before exiting *)
    IF hfile # 0 THEN win._lclose (hfile); END;
    IF lpFile # NIL THEN win.UnmapViewOfFile (lpFile); END;
    IF hMapFile # NIL THEN win.CloseHandle (hMapFile); END;
  END Exit;

BEGIN
  info.Objects := NIL;
  info.N_Objects := 0;
  hMapFile := NIL;
  lpFile := NIL;

  hfile := win.OpenFile (full_name, ReOpenBuff, win.OF_READ);
  IF ReOpenBuff.nErrCode # 0 THEN RETURN FALSE; END;

  hMapFile := win.CreateFileMapping (win.HANDLE(hfile), NIL, win.PAGE_READONLY, 0, 0, NIL);
  lpFile := win.MapViewOfFile (hMapFile, win.FILE_MAP_READ, 0, 0, 0);

  plpFile := sys.ADDRESS(lpFile);
  IF plpFile^ # win.IMAGE_DOS_SIGNATURE THEN RETURN FALSE; END;

  pA := sys.ADDRESS(lpFile);
  ImageHdrOffset := pA^[15]+SIZE(CARDINAL);

  Header := sys.ADDADR(lpFile, ImageHdrOffset-SIZE(CARDINAL));
  IF Header^.Signature # win.IMAGE_NT_SIGNATURE THEN RETURN FALSE; END;

  IF Header^.FileHeader.SizeOfOptionalHeader = 0 THEN RETURN FALSE; END;

  nSections := Header^.FileHeader.NumberOfSections;
  IF nSections = 0 THEN RETURN FALSE; END;

  psh := sys.ADDADR(sys.ADR(Header^.OptionalHeader), Header^.FileHeader.SizeOfOptionalHeader);

  NEW(info.Objects, Header^.FileHeader.NumberOfSections);
  info.N_Objects := Header^.FileHeader.NumberOfSections;
  info.Code_Object :=  MAX(CARDINAL);

  FOR i := 1 TO Header^.FileHeader.NumberOfSections DO
    WITH info.Objects^[i-1] DO
      RelocationBase := Header^.OptionalHeader.ImageBase+psh^[i-1].VirtualAddress;
      Begin := 0;
      End := Begin + psh^[i-1].VirtualSize - 1;
      Attributes := kt.ATTRIBS{};

      IF BITSET(win.IMAGE_SCN_MEM_EXECUTE)*BITSET(psh^[i-1].Characteristics) # {} THEN
        INCL(Attributes, kt.execute);
        IF info.Code_Object = MAX(CARDINAL) THEN
          info.Code_Object := i-1;
        END;
      END;
      IF BITSET(win.IMAGE_SCN_MEM_READ)*BITSET(psh^[i-1].Characteristics) # {} THEN
        INCL(Attributes, kt.read);
      END;
      IF BITSET(win.IMAGE_SCN_MEM_WRITE)*BITSET(psh^[i-1].Characteristics) # {} THEN
        INCL(Attributes, kt.write);
      END;
      IF BITSET(win.IMAGE_SCN_MEM_16BIT)*BITSET(psh^[i-1].Characteristics) = {} THEN
        INCL(Attributes, kt.bit_32);
      END;
    END;
  END;

  IF info.Code_Object = MAX(CARDINAL) THEN
    info.Code_Object := 0;
  END;

  RETURN TRUE;
EXCEPT
  IF info.Objects # NIL THEN DISPOSE(info.Objects); END;
  info.N_Objects := 0;
  RETURN FALSE;
END BuildObjects;

<* END *>


PROCEDURE OpenExe (f_name-: ARRAY OF CHAR; com: CARDINAL; VAR Component: dt.COMPONENT): BOOLEAN;
VAR
  exp: kt.EXPORTS;
  i  : CARDINAL;
  add: BOOLEAN;
  pub: dt.PUBLIC;
BEGIN
  WITH Component DO
    WITH EI DO
      COPY(f_name, full_name);
      fil.ExtractFileName (f_name, short_name);

  <* IF env_target = "x86os2" THEN *>
      IF NOT BuildObjects (f_name, EI) THEN
        -- Почему-то не получилось получить обьекты:
        -- пусть будет на всю память и RB= от нашего линкера
        N_Objects := 2;
        NEW(Objects,2);
        Objects^[0].Begin := 0;
        Objects^[0].End   := 500000H;
        Objects^[0].RelocationBase := 10000H;
        Objects^[1].Begin := 500001H;
        Objects^[1].End   := MAX(CARDINAL);
        Objects^[1].RelocationBase := 20000H;
        Code_Object := 0;
      END;
  <* ELSIF env_target = "x86nt" THEN *>
      IF NOT BuildObjects (f_name, EI) THEN
        -- Почему-то не получилось получить обьекты:
        -- пусть будет на всю память и RB= от нашего линкера
        N_Objects := 1;
        NEW(Objects, 1);
        Objects^[0].Begin := 0;
        Objects^[0].End   := MAX(CARDINAL);
        Objects^[0].RelocationBase := 401000H;
        Code_Object := 0;
      END;
  <* END *>

      DebugInfoTag  := '';
      DebugInfoSize := 0;
    END;
    IF NOT FillDebugInfo (EI, raw) THEN RETURN FALSE; END;
    IF EI.DebugInfoSize = 0 THEN
      IF re.ReadExport (f_name, exp) THEN
        FOR i := 0 TO HIGH(exp^) DO
          WITH exp^[i] DO
            add := (obj > 0) AND (obj <= EI.N_Objects) AND
                   (offset <= MAX(CARDINAL)-EI.Objects^[obj-1].Begin);
           <* IF TARGET_x86 THEN *>
            add := add AND (kt.ATTRIBS{kt.bit_32} <= EI.Objects^[obj-1].Attributes);
           <* END *>
            IF add THEN
              pub.code := kt.execute IN EI.Objects^[obj-1].Attributes;
              pub.addr := offset + EI.Objects^[obj-1].Begin;
              pub.len  := 0;
              pub.name := bld.AddName( name);
              bld.AddPublic(com, pub);
            END;
          END;
        END;
        DISPOSE(exp);
      END;
      RETURN FALSE;
    ELSE
      RETURN dri.CheckDebugInfoVersion (Component.EI);
    END;
  END;
END OpenExe;


END ScanExe.
