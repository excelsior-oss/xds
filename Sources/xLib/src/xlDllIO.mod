(** Copyright (c) 1997 xTech Ltd, Russia. All Rights Reserved. *)
(** XDS Librarian. "PE" and "LX" DLL export section parser. *)

<*+ M2EXTENSIONS *>

IMPLEMENTATION MODULE xlDllIO; (* VitVit'n'Hady. 17.01.97 15:54 *)

IMPORT
   RndFile
  ,RawIO
  ,IOResult
  ,SYSTEM
  ,Storage
<* IF DEBUG THEN *>
  ,dbg:=xlMsg
<* END *>

<* IF env_target = 'x86os2' THEN *>
  ,xldLX
<* ELSIF (env_target = 'x86nt') OR (env_target = 'x86linux') THEN *>
  ,xldPE
<* END *>
  ;

(* I/O routines *)

PROCEDURE ReadBytes ( cid: ChanId; ofs: CARDINAL; VAR buf: ARRAY OF SYSTEM.LOC ) :BOOLEAN;
BEGIN
  RndFile.SetPos(cid,RndFile.NewPos(cid,ofs,1,RndFile.StartPos(cid)));
  RawIO.Read(cid,buf);
  RETURN IOResult.ReadResult(cid)=IOResult.allRight;
END ReadBytes;


<* IF env_target = 'x86os2' THEN *>

(* in LX all strings are stored in the < count, string > format *)

PROCEDURE ReadString ( cid :ChanId; ofs :CARDINAL; VAR buf :ARRAY OF CHAR; VAR len :CARDINAL) :BOOLEAN;
VAR
  l    :BYTE;
  ch   :CHAR;
  bufH :CARDINAL;
  i    :CARDINAL;
BEGIN
  RndFile.SetPos(cid,RndFile.NewPos(cid,ofs,1,RndFile.StartPos(cid)));
  bufH := HIGH(buf);
  RawIO.Read ( cid, l);
  l := l MOD 127;         -- the most-signif bit is a tag, i.e. length is in [0..127]

  len := VAL ( CARDINAL, l );
  i := 0;
  WHILE ( l>0 ) DO
    RawIO.Read (cid, ch);
    IF (IOResult.ReadResult(cid) # IOResult.allRight) THEN RETURN FALSE END;
    IF (i<=bufH) THEN buf[i] := ch; END;
    INC(i); DEC (l);
  END;
  IF (i<=bufH) THEN buf[i]:=0C END;
  RETURN TRUE;
END ReadString;


PROCEDURE OpenExport ( cid :ChanId; VAR desc :PEExportDesc) :INTEGER;
VAR
  sig     :WORD;
  bt      :BYTE;
  lxo     :CARDINAL;
  modType :BITSET;
BEGIN
  WITH desc DO
    nameDll := 0; -- DLL's Name offset
    nameOfs := 0; -- export table offset

    IF ( NOT ReadBytes(cid,0,sig) ) THEN RETURN readIO END;
    IF ( sig # xldLX.MZSIG ) THEN RETURN readNotExe END;

    IF NOT ReadBytes( cid, xldLX.ofs_LXHeader, lxo)  THEN RETURN readIO END;
    IF NOT ReadBytes( cid, lxo, sig ) THEN RETURN readIO END;
    IF ( sig # xldLX.LXSIG ) THEN RETURN readNotPE  END;

    IF NOT ReadBytes( cid, lxo+xldLX.LXofs_ModuleTypeFlags, modType ) THEN RETURN readIO END;
    IF NOT (xldLX.DLLmodule IN modType*xldLX.Module_type_mask) THEN
      RETURN readNoExport  --  it's not DLL => has no export
    END;

    IF NOT ReadBytes( cid, lxo+xldLX.LXofs_RsdNamesTable, nameOfs ) THEN RETURN readIO END;
    INC ( nameOfs, lxo );                       -- this is LX header related offset
    nameDll := nameOfs;
    IF NOT ReadBytes( cid, nameOfs, bt ) THEN RETURN readIO END;
    INC ( nameOfs, VAL(CARDINAL, bt MOD 127)+3 );

    IF NOT ReadBytes( cid, nameOfs, bt ) THEN RETURN readIO END; -- skip DLL's name
    IF ( bt # 0 ) THEN RETURN readOK END; -- non empty resident name table?

    IF NOT ReadBytes( cid, lxo+xldLX.ofs_NonRsdNamesTable, nameOfs ) THEN RETURN readIO END;
    IF ( nameOfs = 0 ) THEN RETURN readNoExport END;

    RETURN readOK;
  END;
END OpenExport;


PROCEDURE NextEntry ( cid :ChanId; VAR desc :PEExportDesc; no :INTEGER; VAR r :ExportEntry ) :INTEGER;
VAR
  o :WORD;
  l :BYTE;
BEGIN
  r.nameofs := desc.nameOfs;

  IF NOT ReadBytes ( cid, desc.nameOfs, l ) THEN RETURN readIO END;
  IF ( l = 0 ) THEN RETURN readEndLXExp END;
  l := l MOD 127;

  INC ( desc.nameOfs, l+1 );
  IF NOT ReadBytes (cid, desc.nameOfs, o ) THEN RETURN readIO END;
  r.ordinal := VAL( INTEGER, o );
  INC ( desc.nameOfs, SIZE (WORD) );

  RETURN readOK;
END NextEntry;



<* ELSIF (env_target = 'x86nt') OR (env_target = 'x86linux') THEN *> -----------------------------------------------------------------



TYPE
  PEHeader          = xldPE.FileHeader;
  PEDataDirEntry    = xldPE.DataDirectory;
  PEOptionalHeader  = xldPE.OptionalHeader;
  PESectionHeader   = xldPE.SectionHeader;
  PEExportDirectory = xldPE.ExportDirectory;


PROCEDURE ReadString(cid: ChanId; ofs: CARDINAL; VAR buf: ARRAY OF CHAR; VAR len: CARDINAL): BOOLEAN;
  VAR
    ch :CHAR;
    l  :CARDINAL;
BEGIN
  RndFile.SetPos(cid,RndFile.NewPos(cid,ofs,1,RndFile.StartPos(cid)));
  l:=0;
  LOOP
    RawIO.Read(cid,ch);
    IF IOResult.ReadResult(cid)#IOResult.allRight THEN RETURN FALSE END;
    IF ch=0C THEN EXIT END;
    IF l<=HIGH(buf) THEN buf[l]:=ch; END; INC(l);
  END;
  len:=l;
  IF l<=HIGH(buf) THEN buf[l]:=0C END;
  RETURN TRUE;
END ReadString;


PROCEDURE SearchExport(cid: ChanId; VAR rva,ofs: CARDINAL): INTEGER;
  CONST
    MZSIG     = xldPE.MZSIG;
    PESIG     = xldPE.PESIG;
    PEOHMagic = xldPE.NTOptionalHdrMagic;
  VAR
    sig,i :WORD;
    o     :CARDINAL;
    ih    :PEHeader;
    oh    :PEOptionalHeader;
    sh    :PESectionHeader;
BEGIN
  IF NOT ReadBytes(cid,0,sig) THEN RETURN readIO END;
  <* IF DEBUG THEN *> dbg.print("MZ = %04x\n",sig); <* END *>

  IF (sig # MZSIG) THEN RETURN readNotExe END;
  IF NOT ReadBytes(cid,60,o)  THEN RETURN readIO END;
  <* IF DEBUG THEN *> dbg.print("PEofs = %d\n",o); <* END *>
  
  IF NOT ReadBytes(cid,o,sig) THEN RETURN readIO END;
  <* IF DEBUG THEN *> dbg.print("PE = %04x\n",sig); <* END *>
  
  IF (sig # PESIG) THEN RETURN readNotPE  END;
  o:=o+4;
  IF NOT ReadBytes(cid,o,ih) THEN RETURN readIO END;
  IF (ih.SizeOfOptionalHeader<SIZE(oh)) OR NOT (1 IN ih.Characteristics) THEN
    RETURN readBadExe (* Bad optional header size or not executable image *)
  END;
  o:=o+SIZE(ih);
  IF NOT ReadBytes(cid,o,oh) THEN RETURN readIO END;
  IF (oh.Magic # PEOHMagic) THEN RETURN readBadExe END;
  <* IF DEBUG THEN *>
    dbg.print("Optional Header Values\n");
    dbg.print("  linker vers %d.%d\n",oh.MajorLinkerVersion,oh.MinorLinkerVersion);
    dbg.print("  OS vers     %d.%d\n",oh.MajorOperatingSystemVersion,oh.MinorOperatingSystemVersion);
    dbg.print("  image vers  %d.%d\n",oh.MajorImageVersion, oh.MinorImageVersion);
    dbg.print("  sectn align %08x\n",oh.SectionAlignment);
    dbg.print("  file  align %08X\n",oh.FileAlignment);

  <* END *>

  rva:=oh.DataDir[xldPE.DirEntryExport].virtAddr;
  <* IF DEBUG THEN *> dbg.print("ESRVA = %08X\n",rva); <* END *>
  
  IF (rva=0) THEN RETURN readNoExport END;
  o := o+ih.SizeOfOptionalHeader;
  <* IF DEBUG THEN *> dbg.print("Sctions No = %d\n",ih.NumberOfSections); <* END *>

  FOR i:=1 TO ih.NumberOfSections DO
    IF NOT ReadBytes(cid,o,sh) THEN RETURN readIO END;
    <* IF DEBUG THEN *> dbg.print("Segment %s\n",sh.Name);
       dbg.print("  raw data ptr  %08X\n",sh.PointerToRawData);
       dbg.print("  raw data size %08X\n",sh.SizeOfRawData);
       dbg.print("  virt addr     %08X\n",sh.VirtualAddress);
    <* END *>
    IF (sh.VirtualAddress<=rva) & (sh.VirtualAddress+sh.SizeOfRawData>rva) THEN
      ofs:=sh.PointerToRawData+(rva-sh.VirtualAddress);
      RETURN readOK;
    END;
    o:=o+SIZE(sh);
  END;
  RETURN readBadExe;
END SearchExport;


PROCEDURE OpenExport(cid: ChanId; VAR desc: PEExportDesc): INTEGER;
VAR
 ofs,rva,add :CARDINAL;
 ed          :PEExportDirectory;
 res         :INTEGER;
BEGIN
  desc.nameDll:=0;
  desc.ordBase:=0;
  desc.funcs  :=0;
  desc.names  :=0;
  desc.nameOfs:=0;
  desc.ordOfs :=0;
  res := SearchExport (cid, rva, ofs);
  IF (res # readOK) THEN RETURN res END;
  IF (ofs = 0) THEN RETURN readNoExport END;
--  ASSERT(rva >= ofs);
  add:=rva-ofs;
<* IF DEBUG THEN *> dbg.print("RVA correction %08X\n",add); <* END *>
  IF NOT ReadBytes(cid,ofs,ed) THEN RETURN readIO END;
<* IF DEBUG THEN *> dbg.print("ed.names: %08X\n",ed.AddressOfNames); <* END *>
<* IF DEBUG THEN *> dbg.print("ed.ords : %08X\n",ed.AddressOfNameOrdinals); <* END *>
  IF (ed.NumberOfNames = 0) THEN RETURN readNoExport END;
  desc.nameDll:=ed.Name-add;
  desc.ordBase:=ed.Base;
  desc.funcs  :=ed.NumberOfFunctions;
  desc.names  :=ed.NumberOfNames;
  desc.nameOfs:=ed.AddressOfNames-add;
  desc.ordOfs :=ed.AddressOfNameOrdinals-add;
  desc.rvaOfs :=ed.AddressOfFunctions-add;
  desc.adjust :=add;
<* IF DEBUG THEN *> dbg.print("%d export entries\n",desc.names); <* END *>
<* IF DEBUG THEN *> dbg.print("names: %08X\n",desc.nameOfs); <* END *>
<* IF DEBUG THEN *> dbg.print("ords : %08X\n",desc.ordOfs); <* END *>
  RETURN readOK;
END OpenExport;

PROCEDURE NextEntry(cid: ChanId; VAR desc: PEExportDesc; no: INTEGER; VAR r: ExportEntry): INTEGER;
  VAR
    o        :SYSTEM.CARD16;
    rva, ofs :CARDINAL;
    i,l      :CARDINAL; 
    s        :ARRAY [0..255] OF CHAR;
BEGIN
  ASSERT((no>=0) & (no<desc.names));
  IF NOT ReadBytes(cid,desc.ordOfs+2*VAL(CARDINAL,no),o)  THEN RETURN readIO END;
  IF NOT ReadBytes(cid,desc.nameOfs+4*VAL(CARDINAL,no),ofs) THEN RETURN readIO END;
  IF NOT ReadBytes(cid,desc.rvaOfs+4*VAL(CARDINAL,o),rva)  THEN RETURN readIO END;

  IF (rva = 0) THEN RETURN EOExport END;

  r.nameofs := ofs-desc.adjust;
  r.ordinal := VAL(INTEGER,desc.ordBase+VAL(CARDINAL,o));
  RETURN readOK;
END NextEntry;

<* END *>

PROCEDURE ReadName(cid: ChanId; ofs: CARDINAL; VAR buf: ARRAY OF CHAR; VAR len: INTEGER) :INTEGER;
VAR
  l :CARDINAL;
BEGIN
  IF ReadString ( cid, ofs, buf, l ) THEN
    len:=l;
    RETURN readOK
  END;
  RETURN readIO;
END ReadName;

END xlDllIO.
