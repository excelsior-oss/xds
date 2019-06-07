(** Copyright (c) 1999-2000 Excelsior. *)
(** XDS librarian. Main module. *)
<*+ MAIN *>
MODULE xlib;

IMPORT
   SYSTEM
  ,xlK
  ,xlOMF
  ,xldOMF
  ,xlStrings
  ,xlFiles
  ,xlDll
  ,xlPars
  ,xlArgs
  ,xlMsg
  ,ChanConsts
  ,RndFile
  ,TimeConv
  ,Strings
  ,WholeStr
  ,dbg:=xlMsg
  ;

CONST (* Options names *)
  (* Common options *)
  OptNOBAK  = "NOBAK";
  OptNODICK = "NODIC";
  OptUSEORD = "USEORD";
  OptIMPLIB = "IMPLIB";
  OptEDF    = "EDF";
  OptNOLOGO = "NOLOGO";
  OptHELP   = "HELP";
  OptLIST   = "LIST";
  (* librarian options *)
  OptNEW    = "NEW";
  (* implib options *)
  OptPREFIX = "PREFIX";
  OptPOSTFIX= "POSTFIX";
  OptXOMF   = "XOMF";

TYPE
  String = xlStrings.String;
  Command = POINTER TO CommandDesc;
  CommandDesc = RECORD
    arg : String;
    flg : SET;
    next: Command;
  END;
  ShowNames = RECORD (xlK.Iterator)
  END;

CONST (* Command.flg *)
  fAppend  = 0;
  fRemove  = 1;
  fExtract = 2;

VAR
  logo    :BOOLEAN;
  prefix  :String;
  postfix :String;

PROCEDURE openError(name-: ARRAY OF CHAR; res: ChanConsts.OpenResults);
  VAR msg: ARRAY 64 OF CHAR;
BEGIN
  IF res=ChanConsts.opened THEN RETURN END;
  CASE res OF
    |ChanConsts.wrongNameFormat:  COPY("wrong name format"                         ,msg);
    |ChanConsts.wrongFlags:       COPY("access dinied"                             ,msg);
    |ChanConsts.tooManyOpen:      COPY("too many open files"                       ,msg);
    |ChanConsts.outOfChans:       COPY("too many open files"                       ,msg);
    |ChanConsts.wrongPermissions: COPY("wrong permissions"                         ,msg);
    |ChanConsts.noRoomOnDevice:   COPY("device full"                               ,msg);
    |ChanConsts.noSuchFile:       COPY("file not found"                            ,msg);
    |ChanConsts.fileExists:       COPY("file already exists"                       ,msg);
    |ChanConsts.wrongFileType:    COPY("wrong file type"                           ,msg);
    |ChanConsts.noTextOperations: COPY("text operations not supported"             ,msg);
    |ChanConsts.noRawOperations:  COPY("raw operations not supported"              ,msg);
    |ChanConsts.noMixedOperations:COPY("unsupported mix of text and raw operations",msg);
    |ChanConsts.alreadyOpen:      COPY("already open"                              ,msg);
  ELSE
    COPY("other problem",msg);
  END;
  xlMsg.perror('File "%s": %s\n',name,msg);
END openError;


PROCEDURE defaultExt (VAR fname: String; iname-,dext-: ARRAY OF CHAR);
  VAR dir,name,ext: String;
BEGIN
  xlFiles.Split(iname,dir,name,ext);
  IF ext^="" THEN ext:=xlStrings.Make(dext) END;
  xlFiles.Merge(fname,dir^,name^,ext^);
END defaultExt;

---------------------------------------   IMPLIB  ----------------------------------------


PROCEDURE prepareImport(list: xlK.Dll);
VAR
  e      :xlK.Entry;
  name   :String;
  lenadd :LONGINT;
BEGIN
  lenadd:=LENGTH(prefix^)+LENGTH(postfix^);
  WHILE list#NIL DO
    e:=list.list;
    WHILE (e # NIL) DO
      IF lenadd>0 THEN
        NEW(name,lenadd+LENGTH(e.int^)+1);
        COPY(prefix^,name^);
        Strings.Append(e.int^,name^);
        Strings.Append(postfix^,name^);
        e.int:=name;
      END;
      e:=e.next;
    END;
    list:=list.next;
  END;
END prepareImport;

PROCEDURE oneImport (lib: xlK.Library; iname-: ARRAY OF CHAR; VAR err: BOOLEAN);
VAR
  import :xlK.Dll;
  file   :xlFiles.File;
  l      :xlK.Library;
  m      :xlK.Module;
  res    :ChanConsts.OpenResults;
  fname  : String;
  dir,name,
  ext    : String;
BEGIN
  xlFiles.Split(iname,dir,name,ext);
  IF ext^="" THEN ext:=xlStrings.Make("DLL") END;
  xlFiles.Merge(fname,dir^,name^,ext^);
  xlStrings.ToUpper(ext^);
  IF ext^="IDF" THEN
    xlFiles.Open(file,fname^,TRUE,res);
    IF res=ChanConsts.opened THEN
      xlPars.ParseDef( file.cid, fname^, import, err);
      file.Close(FALSE);
    ELSE
      openError(fname^,res); HALT(1);
    END;
  ELSE
    xlFiles.Open ( file, fname^, FALSE, res );
    IF (res = ChanConsts.opened) THEN
      xlDll.ReadDllExport( file.cid, fname^, xlArgs.Option(OptUSEORD), import );
      file.Close ( FALSE );
      IF (import = NIL) THEN err := TRUE END;
    ELSE
      openError ( iname, res ); HALT(1);
    END;
  END;
  IF ~err THEN
    prepareImport(import);
    xlK.dir.MakeImportLibrary ( l, import );
    WHILE (l.list # NIL) DO
      m := l.list;
      l.Remove (m);
      lib.Add (m);
    END;
  END;
END oneImport;

PROCEDURE responceArgs ( VAR err :BOOLEAN );
VAR
  a,n   :xlArgs.Arg;
  res   :ChanConsts.OpenResults;
  fname :String;
  file  :xlFiles.File;
  opts  :BOOLEAN;
BEGIN
  a := xlArgs.args;
  WHILE (a # NIL) DO
    IF ( a.body[0]="@" ) THEN
      fname:=xlStrings.Make(a.body^);
      Strings.Delete(fname^,0,1);
      xlFiles.Open(file,fname^,TRUE,res);
      IF ( res=ChanConsts.opened ) THEN
        opts := FALSE;
        xlArgs.Parse(file.cid,fname^,opts,err);
        file.Close(FALSE);
      ELSE
        err:=TRUE;
        openError(fname^,res);
      END;
    END;
    a := a.next;
  END;
  IF err THEN RETURN END;
  a := xlArgs.args;
  err := TRUE;
  WHILE ( a # NIL) DO
    n := a.next;
    IF ( a.body[0]="@" ) THEN
      xlArgs.DeleteArg(a)
    ELSE
      err := FALSE;            -- at least one import/export source ( dll, idf ) must be defined
    END;
    a := n;
  END;
END responceArgs;

PROCEDURE impexpArgs (VAR err: BOOLEAN);
BEGIN
  xlArgs.DefineStr(OptPREFIX,"");
  xlArgs.DefineStr(OptPOSTFIX,"");
  xlArgs.TakeOptions(err);
  responceArgs(err);
END impexpArgs;

PROCEDURE impLib(name-: ARRAY OF CHAR);
VAR
  file  :xlFiles.File;
  lib   :xlK.Library;
  res   :ChanConsts.OpenResults;
  err   :BOOLEAN;
BEGIN
  err := FALSE;
  impexpArgs(err);
  IF err THEN
    xlMsg.perror('%s\n','Import source not found');
    HALT(1)
  END;
  prefix  := xlArgs.Equation(OptPREFIX);
  postfix := xlArgs.Equation(OptPOSTFIX);
  xldOMF.IsXOMF := xlArgs.Option(OptXOMF);
  lib     := xlK.dir.NewLibrary(TimeConv.time());
  WHILE (xlArgs.args # NIL) DO
    oneImport ( lib, xlArgs.args.body^, err );
    xlArgs.DeleteArg(xlArgs.args);
  END;
  IF err THEN RETURN END;
  xlFiles.Create(file,name,FALSE,~xlArgs.Option(OptNOBAK),res);
  IF res=ChanConsts.opened THEN
    err:=FALSE;
    lib.Prepare(err);
    IF err THEN
      file.Close(FALSE);
      HALT(1);
    END;
    lib.Write(file.cid);
    file.Close(TRUE);
  ELSE
    openError(name,res); HALT(1);
  END;
END impLib;

----------------------------------- EDF-file generation --------------------------------


PROCEDURE edfGen ( edfname- :ARRAY OF CHAR );
VAR
  export       :xlK.Dll;
  e            :xlK.Entry;

  file         :xlFiles.File;
  res          :ChanConsts.OpenResults;
  fname, ename :String;
  dir,name,ext :String;
  fstr         :ARRAY 1024 OF CHAR;
  ordstr       :ARRAY 64   OF CHAR;
  err          :BOOLEAN;
BEGIN
  err := FALSE;
  impexpArgs (err);
  IF err THEN
    xlMsg.perror('%s\n','Export source not found');
    HALT(1)
  END;
  prefix  := xlArgs.Equation(OptPREFIX);
  postfix := xlArgs.Equation(OptPOSTFIX);

  ename := xlArgs.args.body;
  xlFiles.Split ( ename^, dir, name, ext );
  IF ( ext^="" ) THEN ext:=xlStrings.Make("DLL") END;
  xlFiles.Merge ( fname, dir^, name^, ext^ );

  xlFiles.Open ( file, fname^, FALSE, res );
  IF ( res = ChanConsts.opened ) THEN
    xlDll.ReadDllExport( file.cid, fname^, xlArgs.Option(OptUSEORD), export );
    file.Close ( FALSE );
    IF ( export=NIL ) THEN err := TRUE END;
  ELSE
    openError ( fname^, res ); HALT(1);
  END;
  xlArgs.DeleteArg(xlArgs.args);
  IF err THEN RETURN END;

  xlFiles.Create ( file, edfname, TRUE, ~xlArgs.Option(OptNOBAK), res );
  IF ( res = ChanConsts.opened ) THEN

    xlFiles.WriteLn ( file.cid );
    xlFiles.WriteString ( file.cid, "LIBRARY " );
    xlFiles.WriteString ( file.cid, export.name^ );
    xlFiles.WriteLn ( file.cid );

    xlFiles.WriteLn ( file.cid );
    xlFiles.WriteString ( file.cid, "EXPORTS" );
    xlFiles.WriteLn ( file.cid );

    e := export.list;
    WHILE (e # NIL) DO
      COPY ( "   ", fstr );
      Strings.Append ( prefix^,  fstr );
      Strings.Append ( e.int^,   fstr );
      Strings.Append ( postfix^, fstr );

      IF ( e.ord # -1 ) THEN
        Strings.Append ( "  @", fstr );
        WholeStr.CardToStr ( VAL(SYSTEM.CARD32, e.ord), ordstr );
        Strings.Append ( ordstr, fstr );
      END;

      xlFiles.WriteString ( file.cid, fstr );
      xlFiles.WriteLn ( file.cid );
      e:=e.next;
    END;
  ELSE
    openError( edfname, res ); HALT(1);
  END;
END edfGen;

--------------------------------------- Librarian ------------------------------------



PROCEDURE libArgs(VAR list: Command; VAR err: BOOLEAN);
VAR a :xlArgs.Arg;
    last,new: Command;
    tag :SET;
    i   :LONGINT;

BEGIN
  err  := FALSE;
  list := NIL;
  xlArgs.DefineBool(OptNEW,FALSE);
  responceArgs(err);
  last:=NIL;
  xlArgs.TakeOptions(err);
  IF err THEN RETURN END;
  WHILE xlArgs.args#NIL DO
    a:=xlArgs.args; xlArgs.DeleteArg(a);
    i:=0; tag:={};
    LOOP
      IF i>=LEN(a.body^) THEN EXIT END;
      IF a.body[i]=0X THEN EXIT END;
      CASE a.body[i] OF
        |"-": INCL(tag,fRemove );
        |"+": INCL(tag,fAppend );
        |"*": INCL(tag,fExtract);
      ELSE
        EXIT
      END;
      INC(i);
    END;
    IF (i<LEN(a.body^)) & (a.body[i]#0X) THEN
      IF tag={} THEN INCL(tag,fAppend) END;
      NEW(new);
      new.flg:=tag;
      new.next:=NIL;
      NEW(new.arg,LEN(a.body^)-i+1);
      Strings.Extract(a.body^,i,LEN(a.body^)-i,new.arg^);
      IF last=NIL THEN list:=new ELSE last.next:=new END;
      last:=new;
    ELSE
      xlMsg.error('invalid argument "%s"',a.body^);
      err:=TRUE;
    END;
  END;
END libArgs;

PROCEDURE appendModule(lib: xlK.Library; name-: ARRAY OF CHAR; VAR err: BOOLEAN);
  VAR m: xlK.Module;
      l: xlK.Library;
   file: xlFiles.File;
  fname: String;
    res: ChanConsts.OpenResults;
BEGIN
  defaultExt(fname,name,"OBJ");
  xlFiles.Open(file,fname^,FALSE,res);
  IF res#ChanConsts.opened THEN
    openError(fname^,res);
    err:=TRUE; RETURN
  END;
  IF xlK.dir.IsModule(file.cid) THEN
    ASSERT(FALSE);
  ELSIF xlK.dir.IsLibrary(file.cid) THEN
    xlK.dir.OpenLibrary(l,file);
    IF l=NIL THEN
      err:=TRUE;
      xlMsg.error('invalid or corrupt file "%s"',fname^);
    ELSE
      WHILE l.list#NIL DO
        m:=l.list;
        l.Remove(m);
        lib.Add(m);
      END;
    END;
  ELSE
    err:=TRUE;
    xlMsg.error('invalid or corrupted file "%s"',fname^);
  END;
  IF err THEN
    file.Close(FALSE)
  ELSE
    file.Suspend;
  END;
END appendModule;

PROCEDURE removeModule(lib: xlK.Library; name-: ARRAY OF CHAR; new: BOOLEAN);
  VAR mod: xlK.Module;
BEGIN
  mod:=lib.Search(name);
  IF mod#NIL THEN
    lib.Remove(mod)
  ELSIF ~new THEN
    xlMsg.warning('module "%s" not found for removing',name);
  END;
END removeModule;

PROCEDURE extractModule(lib: xlK.Library; name-: ARRAY OF CHAR; new: BOOLEAN);
  VAR m: xlK.Module;
   file: xlFiles.File;
  fname: String;
    res: ChanConsts.OpenResults;
BEGIN
  m:=lib.Search(name);
  IF m=NIL THEN
    IF ~new THEN
      xlMsg.warning('module "%s" is not found for the extraction',name);
    END;
    RETURN
  END;
  defaultExt(fname,name,"OBJ");
  xlFiles.Create(file,fname^,FALSE,FALSE,res);
  IF res#ChanConsts.opened THEN
    openError(fname^,res);
    RETURN
  END;
  m.Write(file.cid);
  file.Close(TRUE);
END extractModule;

PROCEDURE librarian ( name- :ARRAY OF CHAR );
VAR
  err,new  :BOOLEAN;
  file,out :xlFiles.File;
  res      :ChanConsts.OpenResults;
  lib      :xlK.Library;
  cmd,c    :Command;
BEGIN
  libArgs(cmd,err);
  IF err THEN HALT(1) END;
  IF cmd=NIL THEN
    xlMsg.error('no commands specified');
    HALT(1);
  END;
  new    := TRUE;
  IF xlArgs.Option(OptNEW) THEN
    lib:=xlK.dir.NewLibrary(TimeConv.time());
  ELSE
    xlFiles.Open(file,name,FALSE,res);
    IF res=ChanConsts.opened THEN
      new:=FALSE;
      xlK.dir.OpenLibrary(lib,file);
      IF lib=NIL THEN
        new:=TRUE;
        xlMsg.warning('library "%s" is invalid or corrupt. New one will be created',name);
        lib:=xlK.dir.NewLibrary(TimeConv.time());
      END;
    ELSIF res=ChanConsts.noSuchFile THEN
      xlMsg.warning('library "%s" does not exists. New one will be created',name);
      lib:=xlK.dir.NewLibrary(TimeConv.time());
    ELSE
      openError(name,res); HALT(1);
    END;
  END;
  c:=cmd; err:=FALSE;
  IF xlArgs.Option(OptNODICK) THEN
    lib.SetProperties ({xlK.nodick_property})
  END;

  REPEAT
    IF fExtract IN c.flg THEN extractModule(lib,c.arg^,new) END;
    IF fRemove  IN c.flg THEN removeModule(lib,c.arg^,new)  END;
    IF fAppend  IN c.flg THEN appendModule(lib,c.arg^,err)  END;
    c:=c.next;
  UNTIL c=NIL;
  IF err THEN HALT(1) END;
  lib.Prepare(err);
  IF err THEN HALT(1) END;
  xlFiles.Create(out,name,FALSE,~xlArgs.Option(OptNOBAK),res);
  IF res#ChanConsts.opened THEN
    openError(name,res);
    HALT(1);
  END;
  lib.Write(out.cid);
  IF ~new THEN file.Close(FALSE) END;
  out.Close(TRUE);
END librarian;

-----------------------------------------  Viewer  -----------------------------------------


PROCEDURE (VAR iter: ShowNames) Take(s-: ARRAY OF CHAR);
BEGIN
  IF iter.info#NIL THEN
    xlMsg.print("    %-25s ( %s )\n",s,iter.info^);
  ELSE
    xlMsg.print("    %s\n",s);
  END;
END Take;

PROCEDURE viewer(name-: ARRAY OF CHAR);
  VAR file: xlFiles.File;
       res: ChanConsts.OpenResults;
       lib: xlK.Library;
      iter: ShowNames;
       mod: xlK.Module;
BEGIN
  xlFiles.Open(file,name,FALSE,res);
  IF res#ChanConsts.opened THEN
    openError(name,res);
    HALT(1);
  END;
  xlK.dir.OpenLibrary(lib,file);
  IF lib=NIL THEN
    xlMsg.error('invalid or corrupt library file "%s"',name);
    HALT(1);
  END;
  xlMsg.print('Library "%s"\n',name);
  mod:=lib.list;
  WHILE mod#NIL DO
    xlMsg.print('  MODULE %s\n',mod.name^);
    mod.IteratePublics(iter);
    mod:=mod.next;
  END;
  file.Close(FALSE);
END viewer;




PROCEDURE ShowLogo;
BEGIN
  IF logo THEN RETURN END;
  logo:=TRUE;
  xlMsg.print("\nXDS Library manager v2.10  (c) 1999-2000 Excelsior\n");
END ShowLogo;

PROCEDURE ShowUsage;
BEGIN
  ShowLogo;
  xlMsg.print(
         'usage   = xlib { option } libname[.lib] { command }\n'
       + '        | xlib /LIST { option } libname[.lib]\n'
       + '        | xlib /IMPLIB { option } libname[.lib] { file }\n'
       + '        | xlib /EDF { option } edfname[.edf] dllname[.dll]\n'
       + 'file    = filename([.dll]|.exe|.idf) | @ResponceFileName\n'
       + 'command = { +|-|* }module[.obj] | @ResponceFileName\n'
       + '  "+" - add module to library\n'
       + '  "-" - remove module from library\n'
       + '  "*" - extract module to separate object file\n'
       + 'Options:\n'
       + '  /IMPLIB - create import library\n'
       + '  /EDF    - create export definition file\n'
       + '  /LIST   - show library contens\n'
       + '  /NOLOGO - do not show logo\n'
       + '  /HELP   - print this text\n'
       + '  /NOBAK  - no "BAK" files\n'
       + '  /NODIC  - do not write dictionary\n'
       + '  /NEW    - create new library file (without /LIST and /IMPLIB only)\n'
       + '  /PREFIX=str  - set internal name prefix to "str"  (with /IMPLIB only)\n'
       + '  /POSTFIX=str - set internal name postfix to "str" (with /IMPLIB only)\n'
       + '  /USEORD - use import by ordinal (with /IMPLIB and /EDF only)\n'
    );
END ShowUsage;

PROCEDURE setDir();
VAR
  omf :xlOMF.Directory;
BEGIN
  NEW(omf);
  xlK.Set(omf);
END setDir;

PROCEDURE resetDir();
BEGIN
  xlK.Set(NIL);
END resetDir;


PROCEDURE Main;
  VAR err: BOOLEAN; a: xlArgs.Arg; fname: String;
BEGIN
  xlArgs.DefineBool(OptIMPLIB,FALSE);
  xlArgs.DefineBool(OptEDF,FALSE);
  xlArgs.DefineBool(OptNOLOGO,FALSE);
  xlArgs.DefineBool(OptHELP,FALSE);
  xlArgs.DefineBool(OptNOBAK,FALSE);
  xlArgs.DefineBool(OptNODICK,FALSE);
  xlArgs.DefineBool(OptUSEORD,FALSE);
  xlArgs.DefineBool(OptLIST,FALSE);
  xlArgs.DefineBool(OptXOMF,FALSE);
  xlArgs.Arguments(err);
  IF err THEN RETURN END;
  xlArgs.TakeOptions(err);
  IF err THEN RETURN END;
  IF ~xlArgs.Option(OptNOLOGO) THEN ShowLogo END;
  IF xlArgs.Option(OptHELP) OR (xlArgs.args=NIL) THEN ShowUsage; HALT(1) END;
  a:=xlArgs.args;
  xlArgs.DeleteArg(a);
  setDir;
  IF xlArgs.Option(OptEDF) THEN
    defaultExt(fname,a.body^,"edf");
    edfGen (fname^);
  ELSE
    defaultExt(fname,a.body^,"lib");
    IF xlArgs.Option(OptIMPLIB) THEN
      impLib(fname^);
    ELSIF xlArgs.Option(OptLIST) THEN
      viewer(fname^);
    ELSE
      librarian(fname^);
    END;
  END;
  resetDir;
  xlArgs.WarnUnusedOptions;
END Main;

BEGIN
  logo:=FALSE;
  Main;
END xlib.
