(** Copyright (c) 1997 xTech Ltd, Russia. All Rights Reserved. *)
(** XDS librarian. "PE" & "LX" DLL export section parser. *)
MODULE xlDll; (* VitVit'n'Hady, Oct 20 1995, revised Jan 17 1997 *)

IMPORT
  xlK
  ,io:=xlDllIO
  ,err:=xlMsg
  ,xlStrings
  ,RndFile
<* IF DEBUG THEN *>
  ,dbg:=xlMsg
<* END *>
  ;

TYPE String = xlStrings.String;

PROCEDURE showPEerror(fname-: ARRAY OF CHAR; res: LONGINT);
BEGIN
  ASSERT(res#io.readOK);
  CASE res OF
    |io.readIO      : err.error('file "%s" read error',fname);
    |io.readNotExe  : err.error('file "%s" is not executable',fname);
    |io.readNotPE   : <* IF env_target = 'x86os2' THEN *>
                        err.error('file "%s" is not "LX" executable',fname);
                      <* ELSIF env_target = 'x86nt' THEN *>
                        err.error('file "%s" is not "PE" executable',fname);
                      <* END *>
    |io.readBadExe  : err.error('file "%s" is corrupted',fname);
    |io.readNoExport: err.error('file "%s" has no export',fname);
  END;
END showPEerror;

PROCEDURE ReadName(cid: RndFile.ChanId; ofs: LONGINT; VAR name: String): LONGINT;
VAR
  buf     :ARRAY 1024 OF CHAR;
  len,res :LONGINT;
BEGIN
  res := io.ReadName ( cid, ofs, buf, len );
  IF (res # io.readOK) THEN RETURN res END;
  NEW(name,len+1);
  IF len>LEN(buf) THEN
    res := io.ReadName ( cid, ofs, name^, len );
  ELSE
    COPY(buf,name^);
  END;
  RETURN res;
END ReadName;


<* IF env_target = 'x86os2' THEN *>

PROCEDURE ReadDllExport* ( cid    :RndFile.ChanId;
                           fname- :ARRAY OF CHAR;
                           useord :BOOLEAN;
                       VAR result :xlK.Dll);
VAR
  entry    :io.ExportEntry;
  desc     :io.PEExportDesc;
  dll      :xlK.Dll;
  l, lprev :xlK.Entry;
  res      :LONGINT;
BEGIN
  result := NIL;
  res    := io.OpenExport ( cid, desc );
  IF (res # io.readOK) THEN showPEerror(fname,res); RETURN END;

  NEW(dll);
  dll.list := NIL;
  lprev    := NIL;
  res := ReadName( cid, desc.nameDll, dll.name );
  IF (res # io.readOK) THEN showPEerror(fname,res); RETURN END;

  LOOP
    res := io.NextEntry ( cid, desc, 0, entry );
    IF (res = io.readEndLXExp) THEN
      EXIT
    ELSIF (res # io.readOK) THEN
      showPEerror(fname,res); RETURN
    END;

    NEW(l);
    res := ReadName ( cid, entry.nameofs, l.ext );
    IF (res # io.readOK) THEN showPEerror(fname,res); RETURN END;

    l.int    := l.ext;
    IF (useord) THEN
      l.ord  := entry.ordinal;
    ELSE
      l.ord  := -1;
    END;
    l.next   := NIL;
    IF ( lprev = NIL ) THEN
      dll.list := l;
    ELSE
      lprev.next := l;
    END;
    lprev := l;
  END;
  result := dll;
END ReadDllExport;

<* ELSIF (env_target = 'x86nt') OR (env_target = 'x86linux') THEN *> --------------------

PROCEDURE ReadDllExport* ( cid    :RndFile.ChanId;
                           fname- :ARRAY OF CHAR;
                           useord :BOOLEAN;
                       VAR result :xlK.Dll);
VAR
  entry   :io.ExportEntry;
  desc    :io.PEExportDesc;
  dll     :xlK.Dll;
  l,lprev :xlK.Entry;
  res     :LONGINT;
  cc      :LONGINT;
BEGIN
  result := NIL;
  res    := io.OpenExport ( cid, desc );
  IF (res # io.readOK) THEN showPEerror(fname,res); RETURN END;
  NEW(dll);
  dll.list := NIL;
  lprev    := NIL;
  res := ReadName( cid, desc.nameDll, dll.name );
  IF (res # io.readOK) THEN showPEerror(fname,res); RETURN END;
  cc := 0;
  LOOP
    IF (cc >= desc.names) THEN EXIT END;

    res := io.NextEntry ( cid, desc, cc, entry );
    IF (res # io.readOK) THEN
      IF (res = io.EOExport) THEN
        EXIT
      ELSE
        showPEerror(fname,res);
        RETURN;
      END;
    END;

    NEW(l);
    res := ReadName ( cid, entry.nameofs, l.ext );
    IF (res # io.readOK) THEN showPEerror(fname,res); RETURN END;
    l.int    := l.ext;
    IF (useord) THEN
      l.ord  := entry.ordinal;
    ELSE
      l.ord  := -1;
    END;
    l.next   := NIL;
    IF ( lprev = NIL ) THEN
      dll.list := l;
    ELSE
      lprev.next := l;
    END;
    lprev := l;
    INC (cc);
  END;
  result := dll;
END ReadDllExport;

<* END *>

END xlDll.
