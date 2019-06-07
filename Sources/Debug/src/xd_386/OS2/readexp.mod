<* Storage+ *>
<* ALIGNMENT="1" *>

IMPLEMENTATION MODULE ReadExp;

IMPORT sys := SYSTEM;
IMPORT rf  := RndFile;
IMPORT xfp := xFilePos;
IMPORT ioc := IOChan;
IMPORT rio := RawIO;

IMPORT kt  := KrnTypes;


IMPORT srt := Sort;


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

  NAME = RECORD
           ord   : sys.CARD16;
           name  : kt.ENTRY_NAME;
         END;
  NAMES = POINTER TO ARRAY OF NAME;

VAR
  file     : ioc.ChanId;
  Names    : NAMES;
  curr_name: CARDINAL;
  Key      : POINTER TO ARRAY OF CARDINAL;

PROCEDURE Name_Compare(i, j: CARDINAL): BOOLEAN;
BEGIN
  RETURN Names^[Key^[i]].ord > Names^[Key^[j]].ord;
END Name_Compare;

PROCEDURE Name_Shake(i, j: CARDINAL);
VAR
  tmp: CARDINAL;
BEGIN
  tmp     := Key^[i];
  Key^[i] := Key^[j];
  Key^[j] := tmp;
END Name_Shake;


PROCEDURE read_name(len: CARDINAL): BOOLEAN;
CONST
  HN = 256;
VAR
  x  : CARDINAL;
  tmp: NAMES;
BEGIN
  IF Names = NIL THEN
    NEW(Names, HN);
    curr_name := 0;
  ELSIF curr_name >= HIGH(Names^) THEN
    NEW(tmp, HIGH(Names^)+1+HN);
    sys.MOVE(sys.ADR(Names^), sys.ADR(tmp^), SIZE(Names^));
    DISPOSE(Names);
    Names := tmp;
  END;
  WITH Names^[curr_name] DO
    ioc.RawRead(file, sys.ADR(name), len, x);
    name[x] := 0C;
  END;
  INC(curr_name);
  RETURN x=len;
END read_name;

PROCEDURE ReadExport (full_name-: ARRAY OF CHAR; VAR exp: kt.EXPORTS): BOOLEAN;
VAR
  res  : rf.OpenResults;

  PROCEDURE fseek(pos: CARDINAL);
  VAR
    fp: rf.FilePos;
  BEGIN
    xfp.CardToPos(fp, pos);
    rf.SetPos(file, fp);
  END fseek;

  PROCEDURE read(VAR buf: ARRAY OF sys.LOC): BOOLEAN;
    VAR x: CARDINAL;
  BEGIN
    x:=SIZE(buf);
    ioc.RawRead(file, sys.ADR(buf), x, x);
    RETURN x=SIZE(buf);
  END read;

VAR
  offs, i, offs32: CARDINAL;
  cnt, type, flg: sys.CARD8;
  hdr    : LX_HEADER;
  object, offs16, callgate: sys.CARD16;
  ordinal: CARDINAL;
  Export : kt.EXPORTS;
  pos    : CARDINAL;

BEGIN
  rf.OpenOld(file, full_name, rf.raw, res);
  IF res # rf.opened THEN RETURN FALSE END;

  fseek(03CH);
  rio.Read(file, offs);
  fseek(offs);
  rio.Read(file, hdr);
  IF ( hdr.signature # 'LX') THEN RETURN FALSE END;

  fseek(offs+hdr.resntoffs);
  curr_name := 0;
  REPEAT
    rio.Read(file, cnt);
    IF cnt > 0 THEN
      ASSERT(read_name(cnt));
      WITH Names^[curr_name-1] DO
        ASSERT(read(ord));
        IF ord = 0 THEN
          DEC(curr_name);
        END;
      END;
    END;
  UNTIL cnt = 0;
  IF hdr.nonrestoffs # 0 THEN
    fseek(hdr.nonrestoffs);
    REPEAT
      rio.Read(file, cnt);
      IF cnt > 0 THEN
        ASSERT(read_name(cnt));
        WITH Names^[curr_name-1] DO
          ASSERT(read(ord));
          IF ord = 0 THEN
            DEC(curr_name);
          END;
        END;
      END;
    UNTIL cnt = 0;
  END;
  IF curr_name <= 1 THEN RETURN FALSE END;
  NEW(Key, curr_name);
  FOR i := 0 TO curr_name-1 DO
    Key^[i] := i;
  END;
  srt.Shell(curr_name, Name_Compare, Name_Shake);

  NEW(Export, curr_name-1);
  pos := 0;

  fseek(offs+hdr.enttoffs);
  ordinal := 1;
  REPEAT
    rio.Read(file, cnt);
    rio.Read(file, type);
    object := 0;
    CASE type OF
    | 00H:
      INC(ordinal, cnt);
    | 01H, 02H, 03H, 04H:
      rio.Read(file, object);
    ELSE
    END;
    IF type # 0 THEN
      FOR i := 1 TO cnt DO
        IF pos < HIGH(Key^) THEN
          CASE type OF
          | 01H:
            rio.Read(file, flg);
            rio.Read(file, offs16);
            IF Names^[Key^[pos+1]].ord = ordinal THEN
              Export^[pos].name    := Names^[Key^[pos+1]].name;
              IF 0 IN sys.SET8(flg) THEN
                Export^[pos].obj     := object;
              ELSE
                Export^[pos].obj     := 0;
              END;
              Export^[pos].offset  := offs16;
              INC(pos);
            END;
          | 02H:
            rio.Read(file, flg);
            rio.Read(file, offs16);
            rio.Read(file, callgate);
            IF Names^[Key^[pos+1]].ord = ordinal THEN
              Export^[pos].name    := Names^[Key^[pos+1]].name;
              Export^[pos].obj     := 0;
              Export^[pos].offset  := offs16;
              INC(pos);
            END;
          | 03H:
            rio.Read(file, flg);
            rio.Read(file, offs32);
            IF Names^[Key^[pos+1]].ord = ordinal THEN
              Export^[pos].name    := Names^[Key^[pos+1]].name;
              IF 0 IN sys.SET8(flg) THEN
                Export^[pos].obj     := object;
              ELSE
                Export^[pos].obj     := 0;
              END;
              Export^[pos].offset  := offs32;
              INC(pos);
            END;
          | 04H:
            rio.Read(file, flg);
            rio.Read(file, offs16);
            rio.Read(file, offs32);
            IF Names^[Key^[pos+1]].ord = ordinal THEN
              Export^[pos].name    := Names^[Key^[pos+1]].name;
              Export^[pos].obj     := 0;
              Export^[pos].offset  := offs16;
              INC(pos);
            END;
          ELSE
            ASSERT(FALSE, type);
          END;
          INC(ordinal);
        END;
      END;
    END;
  UNTIL cnt = 0;

  exp := Export;

  rf.Close(file);
  RETURN TRUE;
--  EXCEPT
--    RETURN FALSE;
END ReadExport;

BEGIN
  Names := NIL;
  Key   := NIL;


END ReadExp.

